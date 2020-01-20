use crate::configuration::Configuration;
use crate::database::{Database, StreamID, TagID};
use crate::query::QueryIndex;
use crate::stream::Stream;
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

#[derive(Debug, Clone)]
pub(crate) struct StreamDecisions {
    pub(crate) index: Option<QueryIndex>,
    pub(crate) accept: Option<bool>,
    pub(crate) add_tag: Option<TagID>,
}

struct StreamDecisionSession {
    stream_id: StreamID,
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

pub struct StarlarkTagsStruct {
    tags: HashMap<String, Value>,
}

impl StarlarkTagsStruct {
    pub(crate) fn new(tags: HashMap<String, bool>) -> StarlarkTagsStruct {
        let tags = tags.into_iter().map(|(k, v)| (k, v.into())).collect();
        StarlarkTagsStruct { tags }
    }
}

impl TypedValue for StarlarkTagsStruct {
    type Holder = Immutable<StarlarkTagsStruct>;

    fn values_for_descendant_check_and_freeze<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::new(std::iter::empty())
    }

    const TYPE: &'static str = "tag struct";

    fn get_attr(&self, attribute: &str) -> Result<Value, ValueError> {
        match self.tags.get(attribute) {
            Some(v) => Ok(v.clone()),
            None => Err(ValueError::OperationNotSupported {
                op: UnsupportedOperation::GetAttr(attribute.to_owned()),
                left: self.to_repr(),
                right: None,
            }),
        }
    }

    fn has_attr(&self, attribute: &str) -> Result<bool, ValueError> {
        Ok(self.tags.contains_key(attribute))
    }

    /*
    fn dir_attr(&self) -> Result<Vec<RcString>, ValueError> {
        Ok(self.tags.keys().cloned().collect())
    }
    */
}

pub(crate) fn environment() -> (Environment, TypeValues) {
    let (env, type_values) = global_environment();
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

        let (mut env, mut type_values) = environment();
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
        let ctx = StreamDecisionSession {
            db: self.db.clone(),
            outcome: RefCell::new(StreamDecisions {
                index: None,
                add_tag: None,
                accept: None,
            }),
            stream_id: stream.id,
        };
        env.set("$ctx", Value::new(ctx)).unwrap();
        let mut tag_map = HashMap::new();
        for (k, v) in self.config.tags.iter() {
            tag_map.insert(v.slug.clone(), stream.tags.contains(k));
        }
        let tag = StarlarkTagsStruct::new(tag_map);
        env.set("tag", Value::new(tag)).unwrap();

        env.set(
            "tags",
            Value::from(stream.tags.iter().map(|t| t.0 as i64).collect::<Vec<_>>()),
        )
        .unwrap();
        env.set("client_data_len", Value::new(stream.client_data_len as i64))
            .unwrap();
        env.set("server_data_len", Value::new(stream.server_data_len as i64))
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

starlark_module! { decision_functions =>
    index(renv env, service = NoneType::None, tag = NoneType::None) {
        let index = match (service.to_int(), tag.to_int()) {
            (Err(_), Err(_)) => QueryIndex::All,
            (Ok(service), Err(_)) => QueryIndex::Service(service as u16),
            (Err(_), Ok(tag)) => QueryIndex::Tagged(TagID(tag as u32)),
            (Ok(service), Ok(tag)) => QueryIndex::ServiceTagged(service as u16, TagID(tag as u32)),
        };
        modify_decisions(env, |o| {
            o.index = Some(index);
        })
    }
    filter(renv env, value: bool) {
        modify_decisions(env, |o| o.accept = Some(value))
    }
}

// # Safety: Starlark (non-Send) values are not exposed via QueryFilterCore's pub(crate) API and QueryFilterCore can't be cloned.
unsafe impl Send for QueryFilterCore {}
fn assert_send<T: Send>() {}
fn test_send() {
    assert_send::<QueryFilterCore>();
}
