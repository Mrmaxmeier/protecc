#![feature(drain_filter)]

mod api;
mod reassembly;

use crate::api::ToolApiImpl;
use std::sync::Arc;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:10000".parse().unwrap();

    Server::builder()
        .add_service(api::server::ToolApiServer::new(ToolApiImpl {}))
        .serve(addr)
        .await?;

    Ok(())
}
