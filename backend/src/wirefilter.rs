use crate::configuration::{Configuration, ServiceID};
use crate::database::{Database, TagID};
use crate::stream::QueryIndex;
use crate::stream::Stream;
use std::mem::transmute;
use std::{collections::HashMap, sync::Arc};
use wirefilter::{ExecutionContext, Filter, Scheme, Type};

pub(crate) struct WirefilterContext<'a> {
    db: Arc<Database>,
    config: Configuration,
    scheme: &'a Scheme,
    filter: Filter<'a>,
    ctx: ExecutionContext<'a>,
    tag_keys: HashMap<TagID, String>,
    service_keys: HashMap<ServiceID, String>,
    uses_data: bool,
    uses_data_combined: bool,
    data_combined_backing: Vec<u8>,
}

impl<'a> WirefilterContext<'a> {
    pub(crate) fn new<'i>(
        content: &'i str,
        scheme: &'a Scheme,
        config: Configuration,
        db: Arc<Database>,
    ) -> Result<Self, wirefilter::ParseError<'i>> {
        let filter_ast = scheme.parse(content)?;
        let ctx = ExecutionContext::new(&scheme);

        let tag_keys = config
            .tags
            .iter()
            .map(|(k, v)| (*k, format!("tag.{}", v.slug)))
            .filter(|(_, k)| filter_ast.uses(k).unwrap())
            .collect();

        let service_keys = config
            .services
            .iter()
            .map(|(k, v)| (*k, format!("service.{}", v.slug)))
            .filter(|(_, k)| filter_ast.uses(k).unwrap())
            .collect();

        let uses_data = filter_ast.uses("data.client").unwrap()
            || filter_ast.uses("data.client").unwrap()
            || filter_ast.uses("data").unwrap();

        let uses_data_combined = filter_ast.uses("data").unwrap();

        let filter = filter_ast.compile();

        Ok(WirefilterContext {
            config,
            db,
            scheme,
            filter,
            ctx,
            tag_keys,
            service_keys,
            uses_data,
            uses_data_combined,
            data_combined_backing: Vec::new(),
        })
    }

    pub(crate) fn make_scheme(config: &Configuration) -> Scheme {
        let mut scheme = Scheme! {
            port.client: Int,
            port.server: Int,
            ip.client: Ip,
            ip.server: Ip,
            data: Bytes,
            data.client: Bytes,
            data.server: Bytes,
            data.len: Int,
            data.client.len: Int,
            data.server.len: Int,
            stream.id: Int,
            stream.packets: Int,
        };

        for (_, service) in config.services.iter() {
            scheme
                .add_field(format!("service.{}", service.slug), Type::Bool)
                .expect("failed to add service bool to scheme");
        }

        for (_, tag) in config.tags.iter() {
            scheme
                .add_field(format!("tag.{}", tag.slug), Type::Bool)
                .expect("failed to add service bool to scheme");
        }

        scheme
    }

    // TODO: accept StreamWithData
    pub(crate) fn matches(&mut self, stream: &Stream) -> bool {
        tracyrs::zone!("wirefilter::matches");

        macro_rules! set {
            ($field:expr, $val:expr) => {
                self.ctx.set_field_value(stringify!($field), $val).unwrap();
            };
        }

        set!(stream.id, stream.id.idx() as i32);
        set!(stream.packets, stream.segments.len() as i32);

        set!(port.client, stream.client.1 as i32);
        set!(port.server, stream.server.1 as i32);
        set!(ip.client, stream.client.0);
        set!(ip.server, stream.server.0);

        set!(data.client.len, stream.client_data_len as i32);
        set!(data.server.len, stream.server_data_len as i32);
        set!(
            data.len,
            (stream.client_data_len + stream.server_data_len) as i32
        );

        let mut client_data = sled::IVec::default();
        let mut server_data = sled::IVec::default();
        if self.uses_data {
            client_data = self.db.datablob(stream.client_data_id).unwrap();
            server_data = self.db.datablob(stream.server_data_id).unwrap();
        }

        // This is dirty but safe:
        // We set data.{client,server} to an empty ref on function (every) exit,
        // effectically shortening the reference to 'self again.
        // Possible fix: shorten lifetime of ctx to 'self. We don't want this to keep the backing vector of ctx?
        // FIXME: this really isn't worth it.
        let client_data_ref = &client_data as &[u8];
        let client_data_ref_extended = unsafe { transmute::<&[u8], &'a [u8]>(client_data_ref) };
        set!(data.client, client_data_ref_extended);
        let server_data_ref = &server_data as &[u8];
        let server_data_ref_extended = unsafe { transmute::<&[u8], &'a [u8]>(server_data_ref) };
        set!(data.server, server_data_ref_extended);

        if self.uses_data_combined {
            self.data_combined_backing.clear();
            self.data_combined_backing.extend_from_slice(&client_data);
            self.data_combined_backing.extend_from_slice(&server_data);
            let combined_ref_extended =
                unsafe { transmute::<&[u8], &'a [u8]>(&self.data_combined_backing) };
            set!(data, combined_ref_extended);
        }

        for (_, service) in self.config.services.iter() {
            self.ctx
                .set_field_value(&format!("service.{}", service.slug), false)
                .unwrap();
        }

        for (_, key) in self.tag_keys.iter() {
            self.ctx.set_field_value(key, false).unwrap();
        }
        for tag in &stream.tags {
            if let Some(key) = self.tag_keys.get(tag) {
                self.ctx.set_field_value(key, true).unwrap();
            }
        }

        for (id, key) in self.service_keys.iter() {
            let service = self.config.services.get(id).unwrap();
            let matches = service.port == stream.service();
            self.ctx.set_field_value(key, matches).unwrap();
        }

        let result = self.filter.execute(&self.ctx).unwrap();

        set!(data, &b""[..]); // see comment above
        set!(data.client, &b""[..]);
        set!(data.server, &b""[..]);

        result
    }

    pub(crate) fn get_index(&self) -> QueryIndex {
        QueryIndex::All // TODO: inspect FilterAst
    }
}
