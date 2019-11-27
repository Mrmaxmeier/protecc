mod api;

use tonic::transport::Server;
use std::sync::Arc;
use crate::api::ToolApiImpl;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:10000".parse().unwrap();

    Server::builder()
        .add_service(api::server::ToolApiServer::new(ToolApiImpl {}))
        .serve(addr)
        .await?;

    Ok(())
}