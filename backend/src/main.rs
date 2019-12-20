#![feature(drain_filter)]

mod api;
mod database;
mod pcapreader;
pub(crate) mod reassembly;

use crate::api::ToolApiImpl;
use reassembly::Reassembler;

use std::env::args;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:10000".parse().unwrap();
    println!("serving on {:?}", addr);

    let mut reassembler = Reassembler::new();

    for path in args().skip(1) {
        println!("importing pcap {:?}", path);
        pcapreader::read_pcap_file(&path, &mut reassembler);
    }

    Server::builder()
        .add_service(api::server::ToolApiServer::new(ToolApiImpl {}))
        .serve(addr)
        .await?;

    Ok(())
}
