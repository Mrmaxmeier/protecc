#![feature(drain_filter, duration_constants)]

// for heaptrack
use std::alloc::System;
#[global_allocator]
static GLOBAL: System = System;

pub(crate) mod counters;
mod database;
mod pcapreader;
mod query;
pub(crate) mod reassembly;
mod wsserver;

use reassembly::Reassembler;

use std::env::args;
use std::sync::Arc;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let database = Arc::new(database::Database::new());
    let mut reassembler = Reassembler::new(database.clone());

    for path in args().skip(1) {
        println!("importing pcap {:?}", path);
        pcapreader::read_pcap_file(&path, &mut reassembler).await;
        // std::thread::sleep(std::time::Duration::from_millis(100));
        reassembler.expire().await;
    }

    let query = query::Query {
        kind: query::QueryKind::All,
        filter: None,
    };
    let mut buf = Vec::new();
    let mut cursor = query.into_cursor(&database).await;

    dbg!(&cursor);
    while cursor.has_next() {
        cursor = cursor.execute(&database, &mut buf).await;
    }
    dbg!(&cursor);
    dbg!(buf.len());

    println!("--------------");
    buf.clear();
    cursor = query::Query {
        kind: query::QueryKind::Service(8080),
        filter: None,
    }
    .into_cursor(&database)
    .await;
    dbg!(cursor.execute(&database, &mut buf).await);

    /*
    let addr = "[::1]:10000".parse().unwrap();
    println!("serving on {:?}", addr);
    Server::builder()
        .add_service(api::tools_server::ToolsServer::new(ToolApiImpl {}))
        .serve(addr)
        .await?;
    */

    let addr = "[::1]:10000".parse::<std::net::SocketAddr>().unwrap();
    let try_socket = TcpListener::bind(&addr).await;
    let mut listener = try_socket.expect("Failed to bind");
    println!("Listening on: {}", addr);
    
    while let Ok((stream, _)) = listener.accept().await {
        tokio::spawn(wsserver::accept_connection(stream, database.clone()));
    }


    Ok(())
}
