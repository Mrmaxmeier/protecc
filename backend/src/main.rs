#![feature(drain_filter)]

mod api;
pub(crate) mod counters;
mod database;
mod pcapreader;
mod query;
pub(crate) mod reassembly;

use crate::api::ToolApiImpl;
use reassembly::Reassembler;

use std::env::args;
use std::sync::Arc;
use tokio::prelude::*;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let database = Arc::new(database::Database::new());
    let mut reassembler = Reassembler::new(database.clone());

    for path in args().skip(1) {
        println!("importing pcap {:?}", path);
        pcapreader::read_pcap_file(&path, &mut reassembler);
    }

    for (k, v) in database.services.read().unwrap().iter() {
        println!("service {}: #{}", k, v.lock().unwrap().streams.len());
    }

    let query = query::Query {
        kind: query::QueryKind::All,
        limit: None,
        filter: None,
    };
    let mut buf = Vec::new();
    let mut cursor = query.into_cursor(&database);

    while cursor.has_next() {
        cursor = cursor.execute(&database, &mut buf);
        dbg!(&cursor);
        dbg!(buf.len());
    }

    println!("--------------");
    buf.clear();
    cursor = query::Query {
        kind: query::QueryKind::Service(8080),
        limit: None,
        filter: None,
    }.into_cursor(&database);
    dbg!(cursor.execute(&database, &mut buf));
    dbg!(buf);

    let addr = "[::1]:10000".parse().unwrap();
    println!("serving on {:?}", addr);
    Server::builder()
        .add_service(api::tools_server::ToolsServer::new(ToolApiImpl {}))
        .serve(addr)
        .await?;
    Ok(())
}
