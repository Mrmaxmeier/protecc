#![feature(drain_filter, duration_constants, async_closure)]
#![recursion_limit = "512"] // for futures::select!

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

    let fut = tokio::spawn(async move {
        let addr = "[::1]:10000".parse::<std::net::SocketAddr>().unwrap();
        let try_socket = TcpListener::bind(&addr).await;
        let mut listener = try_socket.expect("Failed to bind");
        println!("Listening on: {}", addr);

        while let Ok((stream, _)) = listener.accept().await {
            tokio::spawn(wsserver::accept_connection(stream, database.clone()));
        }
    });

    // TODO: move pcap parser into own thread
    let pcaps = args().skip(1).collect::<Vec<_>>();
    for path in &pcaps {
        println!("importing pcap {:?}", path);
        pcapreader::read_pcap_file(&path, &mut reassembler).await;
        // std::thread::sleep(std::time::Duration::from_millis(100));
        reassembler.expire().await;
    }

    fut.await.expect("wsserver died");
    Ok(())
}
