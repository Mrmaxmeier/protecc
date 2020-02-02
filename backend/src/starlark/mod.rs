use crate::configuration::Configuration;
use crate::database::{Database, StreamID, StreamPayloadID, TagID};
use crate::query::QueryIndex;
use crate::stream::Stream;
use regex::bytes::Regex;
use starlark::codemap::CodeMap;
use starlark::codemap_diagnostic::Diagnostic;
use starlark::environment::{Environment, TypeValues};
use starlark::stdlib::global_environment;
use starlark::syntax::parser::parse;
use starlark::values::error::*;
use starlark::values::none::NoneType;
use starlark::values::*;
use starlark::{
    starlark_fun, starlark_module, starlark_parse_param_type, starlark_signature,
    starlark_signature_extraction, starlark_signatures,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Debug, Default, Clone)]
pub(crate) struct StreamDecisions {
    pub(crate) index: Option<QueryIndex>,
    pub(crate) accept: Option<bool>,
    pub(crate) added_tags: Vec<TagID>,
    pub(crate) attached: Option<serde_json::Value>,
    pub(crate) sort_key: Option<i64>,
}

struct StreamDecisionSession {
    stream_id: StreamID,
    client_payload_id: StreamPayloadID,
    server_payload_id: StreamPayloadID,
    db: Arc<Database>,
    outcome: RefCell<StreamDecisions>,
}

impl TypedValue for StreamDecisionSession {
    type Holder = Immutable<StreamDecisionSession>;

    fn values_for_descendant_check_and_freeze<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::new(std::iter::empty())
    }

    const TYPE: &'static str = "internal interpreter dispatch struct";
}

pub struct CustomKV {
    fields: HashMap<String, Value>,
}

impl CustomKV {
    pub(crate) fn new(fields: HashMap<String, Value>) -> CustomKV {
        CustomKV { fields }
    }
}

impl TypedValue for CustomKV {
    type Holder = Immutable<CustomKV>;

    fn values_for_descendant_check_and_freeze<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::new(std::iter::empty())
    }

    const TYPE: &'static str = "stream meta struct";

    fn get_attr(&self, attribute: &str) -> Result<Value, ValueError> {
        match self.fields.get(attribute) {
            Some(v) => Ok(v.clone()),
            None => Err(ValueError::OperationNotSupported {
                op: UnsupportedOperation::GetAttr(attribute.to_owned()),
                left: self.to_repr(),
                right: None,
            }),
        }
    }

    fn has_attr(&self, attribute: &str) -> Result<bool, ValueError> {
        Ok(self.fields.contains_key(attribute))
    }

    /*
    fn dir_attr(&self) -> Result<Vec<RcString>, ValueError> {
        Ok(self.fields.keys().cloned().collect())
    }
    */
}

pub(crate) fn environment(config: &Configuration) -> (Environment, TypeValues) {
    let (env, type_values) = global_environment();

    {
        let mut tags_map = HashMap::new();
        for (k, v) in config.tags.iter() {
            tags_map.insert(v.slug.clone(), (k.0 as i64).into());
        }
        env.set("tags", Value::new(CustomKV::new(tags_map)))
            .unwrap();
    }

    {
        let mut services_map = HashMap::new();
        for (_, v) in config.services.iter() {
            services_map.insert(v.slug.clone(), (v.port as i64).into());
        }
        env.set("services", Value::new(CustomKV::new(services_map)))
            .unwrap();
    }

    (env, type_values)
}

pub(crate) struct QueryFilterCore {
    db: Arc<Database>,
    module: starlark::eval::module::Module,
    code_map: Arc<Mutex<CodeMap>>,
    env: Environment,
    config: Configuration,
    type_values: TypeValues,
}

impl QueryFilterCore {
    pub(crate) fn new(
        content: &str,
        config: Configuration,
        db: Arc<Database>,
    ) -> Result<Self, Diagnostic> {
        let code_map = Arc::new(Mutex::new(CodeMap::new()));

        let (mut env, mut type_values) = environment(&config);
        decision_functions(&mut env, &mut type_values);

        let dialect = starlark::syntax::dialect::Dialect::Bzl;
        let module = parse(&code_map, "input", content, dialect)?;

        Ok(QueryFilterCore {
            config,
            db,
            module,
            code_map,
            env,
            type_values,
        })
    }

    pub(crate) fn get_meta(&self) -> Result<QueryIndex, starlark::eval::EvalException> {
        tracyrs::zone!("get_meta");
        Ok(self
            .get_verdict(&Stream::dummy())?
            .index
            .unwrap_or(QueryIndex::All))
    }

    pub(crate) fn get_verdict(
        &self,
        stream: &Stream,
    ) -> Result<StreamDecisions, starlark::eval::EvalException> {
        tracyrs::zone!("get_verdict");
        let mut env = self.env.child("stream");
        {
            tracyrs::zone!("get_verdict", "populate env");
            let ctx = StreamDecisionSession {
                db: self.db.clone(),
                outcome: RefCell::new(StreamDecisions::default()),
                stream_id: stream.id,
                client_payload_id: stream.client_data_id,
                server_payload_id: stream.server_data_id,
            };
            env.set("$ctx", Value::new(ctx)).unwrap();
            let mut tag_map = HashMap::new();
            for (k, v) in self.config.tags.iter() {
                tag_map.insert(v.slug.clone(), stream.tags.contains(k).into());
            }
            let tag = CustomKV::new(tag_map);
            env.set("tag", Value::new(tag)).unwrap();

            let mut service_map = HashMap::new();
            for (_, v) in self.config.services.iter() {
                service_map.insert(v.slug.clone(), (stream.service() == v.port).into());
            }
            let service = CustomKV::new(service_map);
            env.set("service", Value::new(service)).unwrap();

            env.set(
                "tag_list",
                Value::from(stream.tags.iter().map(|t| t.0 as i64).collect::<Vec<_>>()),
            )
            .unwrap();
            env.set("client_len", Value::new(stream.client_data_len as i64))
                .unwrap();
            env.set("server_len", Value::new(stream.server_data_len as i64))
                .unwrap();
            env.set(
                "data_len",
                Value::new(stream.client_data_len as i64 + stream.server_data_len as i64),
            )
            .unwrap();
            env.set("client_ip", Value::new(format!("{}", stream.client.0)))
                .unwrap();
            env.set("server_ip", Value::new(format!("{}", stream.server.0)))
                .unwrap();
            env.set("client_port", Value::new(stream.client.1 as i64))
                .unwrap();
            env.set("server_port", Value::new(stream.server.1 as i64))
                .unwrap();
            env.set("id", Value::new(stream.id.idx() as i64)).unwrap();
            env.set("stream_id", Value::new(stream.id.idx() as i64))
                .unwrap();
        }

        let res = {
            tracyrs::zone!("get_verdict", "eval_module");
            starlark::eval::eval_module(
                &self.module,
                &mut env,
                &self.type_values,
                self.code_map.clone(),
                &starlark::eval::noload::NoLoadFileLoader,
                1337, // fuel
            )?
        };

        let val = env.get("$ctx").unwrap();
        let holder = val.value_holder();
        let mut _ctx = holder
            .as_any_ref()
            .downcast_ref::<StreamDecisionSession>()
            .unwrap()
            .outcome
            .borrow()
            .clone();

        if res.get_type() == "bool" {
            assert!(_ctx.accept.is_none());
            _ctx.accept = Some(res.to_bool());
        }
        Ok(_ctx)
    }
}

fn modify_decisions<F: Fn(&mut StreamDecisions)>(env: &Environment, f: F) -> ValueResult {
    let val = env.get("$ctx").unwrap();
    let holder = val.value_holder();
    let _ctx = holder.as_any_ref().downcast_ref::<StreamDecisionSession>();
    if let Some(_ctx) = _ctx {
        let mut outcome = _ctx.outcome.borrow_mut();
        f(&mut *outcome);
    //println!("decisions: {:?}", outcome);
    } else {
        Err(ValueError::Runtime(RuntimeError {
            code: "internal error",
            message: String::from("internal error"),
            label: String::from("internal error"),
        }))?
    }
    Ok(Value::new(NoneType::None))
}

fn data_matches(env: &Environment, regex: &str) -> bool {
    // TODO: refactor
    let val = env.get("$ctx").unwrap();
    let holder = val.value_holder();
    let session = holder
        .as_any_ref()
        .downcast_ref::<StreamDecisionSession>()
        .expect("session downcast failed");

    let regex = Regex::new(regex).unwrap(); // TODO: cache

    if let Some(client_data) = session.db.datablob(session.client_payload_id) {
        // TODO: cache
        if regex.is_match(&client_data) {
            return true;
        }
    }

    if let Some(server_data) = session.db.datablob(session.server_payload_id) {
        // TODO: cache
        if regex.is_match(&server_data) {
            return true;
        }
    }
    false
}

fn data_capture(env: &Environment, regex: &str) -> Option<Vec<String>> {
    // TODO: refactor
    let val = env.get("$ctx").unwrap();
    let holder = val.value_holder();
    let session = holder
        .as_any_ref()
        .downcast_ref::<StreamDecisionSession>()
        .expect("session downcast failed");

    let regex = Regex::new(regex).unwrap(); // TODO: cache

    if let Some(client_data) = session.db.datablob(session.client_payload_id) {
        // TODO: cache
        if let Some(captures) = regex.captures(&client_data) {
            let mut res = Vec::new();
            for i in 0..captures.len() {
                res.push(String::from_utf8_lossy(&captures[i]).to_string());
            }
            return Some(res);
        }
    }

    if let Some(server_data) = session.db.datablob(session.server_payload_id) {
        // TODO: cache
        if let Some(captures) = regex.captures(&server_data) {
            let mut res = Vec::new();
            for i in 0..captures.len() {
                res.push(String::from_utf8_lossy(&captures[i]).to_string());
            }
            return Some(res);
        }
    }
    None
}

fn starlark_to_json(val: &starlark::values::Value) -> Result<serde_json::Value, ()> {
    if let Ok(v) = val.to_int() {
        Ok(v.into())
    } else if let Ok(v) = val.to_vec() {
        let mut res = Vec::with_capacity(v.len());
        for elem in v {
            res.push(starlark_to_json(&elem)?);
        }
        Ok(res.into())
    } else if val.get_type() == "NoneType" {
        Ok(serde_json::Value::Null)
    } else if val.get_type() == "bool" {
        Ok(val.to_bool().into())
    } else if val.get_type() == "string" {
        Ok(val.to_str().into())
    } else if val.get_type() == "dict" {
        let mut map = serde_json::map::Map::new();
        for key in val.dir_attr().map_err(|_| ())? {
            let val = val.get_attr(&key).map_err(|_| ())?;
            map.insert(key.to_string(), starlark_to_json(&val)?);
        }
        Ok(map.into())
    } else {
        Err(())
    }
}

starlark_module! { decision_functions =>
    index(renv env, service = NoneType::None, tag = NoneType::None) {
        let index = match (service.to_int(), tag.to_int()) {
            (Err(_), Err(_)) => QueryIndex::All,
            (Ok(service), Err(_)) => QueryIndex::Service(service as u16),
            (Err(_), Ok(tag)) => QueryIndex::Tagged(TagID(tag as u16)),
            (Ok(service), Ok(tag)) => QueryIndex::ServiceTagged(service as u16, TagID(tag as u16)),
        };
        modify_decisions(env, |o| {
            o.index = Some(index);
        })
    }

    filter(renv env, value: bool) {
        modify_decisions(env, |o| o.accept = Some(value))
    }

    emit(renv env, value) {
        let _ = modify_decisions(env, |o| o.attached = Some(starlark_to_json(&value).unwrap())); // TODO: exception
        Ok(value)
    }

    sort_key(renv env, value: i64) {
        modify_decisions(env, |o| o.sort_key = Some(value))
    }

    add_tag(renv env, tag: i64) {
        modify_decisions(env, |o| o.added_tags.push(TagID(tag as u16)))
    }

    data_matches_(renv env, regex: String) {
        Ok(data_matches(env, &regex).into())

    }

    data_capture_(renv env, regex: String) {
        Ok(match data_capture(env, &regex) {
            Some(result) => result.into(),
            None => NoneType::None.into()
        })
    }
}

// # Safety:
// Non-send fields are not exposed via QueryFilterCore's
// pub(crate) API and QueryFilterCore can't be cloned.
unsafe impl Send for QueryFilterCore {}
