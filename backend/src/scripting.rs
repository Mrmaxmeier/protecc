use crate::configuration::Configuration;
use crate::database::{Database, StreamPayloadID, TagID};
use crate::stream::QueryIndex;
use crate::stream::Stream;
use ::regex::bytes::Regex;
use smallvec::SmallVec;
use starlark::environment::{GlobalsBuilder, Module};
use starlark::eval::Evaluator;
use starlark::syntax::{AstModule, Dialect};
use starlark::values::none::NoneType;
use starlark::{starlark_simple_value, starlark_type, values::*};
use starlark_derive::starlark_module;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;

thread_local! {
    static REGEX_CACHE: RefCell<HashMap<String, Arc<Regex>>> = RefCell::new(HashMap::new());
}

#[derive(Debug, Default, Clone)]
pub(crate) struct StreamDecisions {
    pub(crate) index: Option<QueryIndex>,
    pub(crate) accept: Option<bool>,
    pub(crate) added_tags: SmallVec<[TagID; 4]>,
    pub(crate) attached: Option<serde_json::Value>,
    pub(crate) sort_key: Option<i32>,
}

#[derive(ProvidesStaticType)]
struct StreamDecisionSession {
    client_payload_id: StreamPayloadID,
    server_payload_id: StreamPayloadID,
    db: Arc<Database>,
    outcome: RefCell<StreamDecisions>,
}

#[derive(Debug)]
enum CustomKVValue {
    Bool(bool),
    I32(i32),
}

#[derive(Debug, ProvidesStaticType, NoSerialize)]
// #[display(fmt = "CustomKV")]
struct CustomKV {
    fields: HashMap<String, CustomKVValue>,
}

impl CustomKV {
    pub(crate) fn new(fields: HashMap<String, CustomKVValue>) -> Self {
        CustomKV { fields }
    }
}

starlark_simple_value!(CustomKV);
impl<'v> StarlarkValue<'v> for CustomKV {
    starlark_type!("customkv");
    fn get_attr(&self, attribute: &str, _heap: &'v Heap) -> Option<Value<'v>> {
        Some(match self.fields.get(attribute)? {
            CustomKVValue::Bool(val) => Value::new_bool(*val),
            CustomKVValue::I32(val) => Value::new_int(*val),
        })
    }

    fn has_attr(&self, attribute: &str) -> bool {
        self.fields.contains_key(attribute)
    }
}

impl std::fmt::Display for CustomKV {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("CustomKV { .. }")
    }
}

pub(crate) fn env_with_config_globals(env: &mut GlobalsBuilder, config: &Configuration) {
    {
        let mut tags_map = HashMap::new();
        for (k, v) in config.tags.iter() {
            tags_map.insert(v.slug.clone(), CustomKVValue::I32(k.0 as i32));
        }
        env.set("tags", CustomKV::new(tags_map));
    }

    {
        let mut services_map = HashMap::new();
        for (_, v) in config.services.iter() {
            services_map.insert(v.slug.clone(), CustomKVValue::I32(v.port as i32));
        }
        env.set("services", CustomKV::new(services_map));
    }
}

pub(crate) struct StarlarkEngine {
    db: Arc<Database>,
    config: Configuration,
    script: String,
}

impl StarlarkEngine {
    pub(crate) fn new(content: &str, config: Configuration, db: Arc<Database>) -> Self {
        StarlarkEngine {
            config,
            db,
            script: content.to_owned(),
        }
    }

    pub(crate) fn get_meta(&self) -> Result<QueryIndex, Box<dyn Error>> {
        tracyrs::zone!("get_meta");
        Ok(self
            .get_verdict(&Stream::dummy())?
            .index
            .unwrap_or(QueryIndex::All))
    }

    pub(crate) fn get_verdict(&self, stream: &Stream) -> Result<StreamDecisions, Box<dyn Error>> {
        tracyrs::zone!("get_verdict");
        let ctx = StreamDecisionSession {
            db: self.db.clone(),
            outcome: RefCell::new(StreamDecisions::default()),
            client_payload_id: stream.client_data_id,
            server_payload_id: stream.server_data_id,
        };

        let mut env = GlobalsBuilder::extended().with(decision_functions);
        env_with_config_globals(&mut env, &self.config);
        {
            tracyrs::zone!("get_verdict", "populate env");
            let mut tag_map = HashMap::new();
            for (k, v) in self.config.tags.iter() {
                tag_map.insert(v.slug.clone(), CustomKVValue::Bool(stream.tags.contains(k)));
            }
            let tag = CustomKV::new(tag_map);
            env.set("tag", tag);

            let mut service_map = HashMap::new();
            for (_, v) in self.config.services.iter() {
                service_map.insert(
                    v.slug.clone(),
                    CustomKVValue::Bool(stream.service() == v.port),
                );
            }
            let service = CustomKV::new(service_map);
            env.set("service", service);

            env.set(
                "tag_list",
                stream.tags.iter().map(|t| t.0 as i32).collect::<Vec<_>>(),
            );
            env.set("client_len", stream.client_data_len as i32);
            env.set("server_len", stream.server_data_len as i32);
            env.set(
                "data_len",
                stream.client_data_len as i32 + stream.server_data_len as i32,
            );
            env.set("client_ip", format!("{}", stream.client.0));
            env.set("server_ip", format!("{}", stream.server.0));
            env.set("client_port", stream.client.1 as i32);
            env.set("server_port", stream.server.1 as i32);
            env.set("id", stream.id.idx() as i32);
            env.set("stream_id", stream.id.idx() as i32);
        }

        {
            tracyrs::zone!("get_verdict", "eval_module");
            let module = Module::new();
            let mut eval = Evaluator::new(&module);
            let ast = AstModule::parse("input", self.script.clone(), &Dialect::Extended)?;
            // TODO: fuel? eval.enable_before_stmt_instrumentation();
            eval.extra = Some(&ctx);
            let res = eval.eval_module(ast, &env.build())?;

            let mut _ctx = ctx.outcome.borrow().clone();
            if res.get_type() == "bool" {
                assert!(_ctx.accept.is_none());
                _ctx.accept = Some(res.to_bool());
            } else if res.get_type() == "NoneType" {
                assert!(_ctx.accept.is_none());
                _ctx.accept = Some(false);
            }
            Ok(_ctx)
        }
    }
}

fn modify_decisions<F: Fn(&mut StreamDecisions)>(
    eval: &mut Evaluator,
    f: F,
) -> anyhow::Result<NoneType> {
    let _ctx = eval
        .extra
        .unwrap()
        .downcast_ref::<StreamDecisionSession>()
        .unwrap();
    let mut outcome = _ctx.outcome.borrow_mut();
    f(&mut *outcome);
    //println!("decisions: {:?}", outcome);
    Ok(NoneType)
}

enum Direction {
    Client,
    Server,
    Both,
}

fn data_matches_(eval: &mut Evaluator, regex: &str, direction: Direction) -> bool {
    let session = eval
        .extra
        .unwrap()
        .downcast_ref::<StreamDecisionSession>()
        .unwrap();

    let regex = REGEX_CACHE.with(|re_cache| {
        re_cache
            .borrow_mut()
            .entry(regex.to_owned())
            .or_insert_with(|| Arc::new(Regex::new(regex).unwrap()))
            .clone()
    });

    if matches!(direction, Direction::Client | Direction::Both) {
        if let Some(client_data) = session.db.datablob(session.client_payload_id) {
            // TODO: cache
            if regex.is_match(&client_data) {
                return true;
            }
        }
    }

    if matches!(direction, Direction::Server | Direction::Both) {
        if let Some(server_data) = session.db.datablob(session.server_payload_id) {
            // TODO: cache
            if regex.is_match(&server_data) {
                return true;
            }
        }
    }
    false
}

fn data_capture_(eval: &mut Evaluator, regex: &str, direction: Direction) -> Option<Vec<String>> {
    let session = eval
        .extra
        .unwrap()
        .downcast_ref::<StreamDecisionSession>()
        .unwrap();

    let regex = REGEX_CACHE.with(|re_cache| {
        re_cache
            .borrow_mut()
            .entry(regex.to_owned())
            .or_insert_with(|| Arc::new(Regex::new(regex).unwrap()))
            .clone()
    });

    let mut res = Vec::new();

    if matches!(direction, Direction::Client | Direction::Both) {
        if let Some(client_data) = session.db.datablob(session.client_payload_id) {
            // TODO: cache
            if let Some(captures) = regex.captures(&client_data) {
                for i in 0..captures.len() {
                    res.push(String::from_utf8_lossy(&captures[i]).to_string());
                }
            }
        }
    }

    if matches!(direction, Direction::Server | Direction::Both) {
        if let Some(server_data) = session.db.datablob(session.server_payload_id) {
            // TODO: cache
            if let Some(captures) = regex.captures(&server_data) {
                for i in 0..captures.len() {
                    res.push(String::from_utf8_lossy(&captures[i]).to_string());
                }
            }
        }
    }

    if res.is_empty() {
        None
    } else {
        Some(res)
    }
}

#[starlark_module]
fn decision_functions(registry: &mut GlobalsBuilder) {
    fn index(
        service: Option<i32>,
        tag: Option<i32>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<NoneType> {
        let index = match (service, tag) {
            (None, None) => QueryIndex::All,
            (Some(service), None) => QueryIndex::Service(service as u16),
            (None, Some(tag)) => QueryIndex::Tagged(TagID(tag as u16)),
            (Some(service), Some(tag)) => {
                QueryIndex::ServiceTagged(service as u16, TagID(tag as u16))
            }
        };
        modify_decisions(eval, |o| {
            o.index = Some(index);
        })
    }

    fn filter(value: bool, eval: &mut Evaluator) -> anyhow::Result<NoneType> {
        modify_decisions(eval, |o| o.accept = Some(value))
    }

    fn emit<'v>(value: Value<'v>, eval: &mut Evaluator) -> anyhow::Result<Value<'v>> {
        let _ = modify_decisions(eval, |o| {
            o.attached = Some(serde_json::value::to_value(&value).unwrap());
        })?;
        Ok(value)
    }

    fn sort_key(value: i32, eval: &mut Evaluator) -> anyhow::Result<NoneType> {
        modify_decisions(eval, |o| o.sort_key = Some(value))
    }

    fn add_tag(tag: i32, eval: &mut Evaluator) -> anyhow::Result<NoneType> {
        modify_decisions(eval, |o| o.added_tags.push(TagID(tag as u16)))
    }

    fn data_matches(regex: &str, eval: &mut Evaluator) -> anyhow::Result<bool> {
        Ok(data_matches_(eval, regex, Direction::Both))
    }

    fn client_data_matches(regex: &str, eval: &mut Evaluator) -> anyhow::Result<bool> {
        Ok(data_matches_(eval, regex, Direction::Client))
    }

    fn server_data_matches(regex: &str, eval: &mut Evaluator) -> anyhow::Result<bool> {
        Ok(data_matches_(eval, regex, Direction::Server))
    }

    fn data_capture(regex: &str, eval: &mut Evaluator) -> anyhow::Result<Option<Vec<String>>> {
        Ok(match data_capture_(eval, regex, Direction::Both) {
            Some(result) => result.into(),
            None => None,
        })
    }

    fn client_data_capture(
        regex: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<Option<Vec<String>>> {
        Ok(match data_capture_(eval, regex, Direction::Client) {
            Some(result) => result.into(),
            None => None,
        })
    }

    fn server_data_capture(
        regex: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<Option<Vec<String>>> {
        Ok(match data_capture_(eval, regex, Direction::Server) {
            Some(result) => result.into(),
            None => None,
        })
    }
}

// # Safety:
// Non-send fields are not exposed via StarlarkEngine's
// pub(crate) API and StarlarkEngine can't be cloned.
unsafe impl Send for StarlarkEngine {}
