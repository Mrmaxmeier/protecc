#![feature(drain_filter)]

mod api;
mod database;
mod pcapreader;
mod query;
pub(crate) mod counters;
pub(crate) mod reassembly;

use crate::api::ToolApiImpl;
use reassembly::Reassembler;

use std::env::args;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    //let addr = "[::1]:10000".parse().unwrap();
    //println!("serving on {:?}", addr);
    let database = database::Database::new();
    let mut reassembler = Reassembler::new(database.clone());

    for path in args().skip(1) {
        println!("importing pcap {:?}", path);
        pcapreader::read_pcap_file(&path, &mut reassembler);
    }

    for (k, v) in database.services.read().unwrap().iter(){
        println!("service {}: #{}", k, v.lock().unwrap().streams.len());
    }

    let query = query::Query {
        kind: query::QueryKind::All,
        limit: None,
    };
    query.execute(&database);

    /*
    Server::builder()
        .add_service(api::server::ToolApiServer::new(ToolApiImpl {}))
        .serve(addr)
        .await?;
    */
    Ok(())
}
