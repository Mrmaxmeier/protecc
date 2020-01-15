#![feature(drain_filter, duration_constants, async_closure)]
#![recursion_limit = "512"] // for futures::select!

// for heaptrack
use std::alloc::System;
#[global_allocator]
static GLOBAL: System = System;

pub(crate) mod configuration;
pub(crate) mod counters;
pub(crate) mod database;
pub(crate) mod pcapreader;
pub(crate) mod pipeline;
pub(crate) mod query;
pub(crate) mod reassembly;
pub(crate) mod serde_aux;
pub(crate) mod starlark;
pub(crate) mod stream;
pub(crate) mod window;
pub(crate) mod wsserver;

use reassembly::Reassembler;

use std::env::args;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let database = database::Database::new();
    let mut reassembler = Reassembler::new(database.clone());

    /*
    let mut config = configuration::Configuration::default();
    let tag = configuration::Tag::from_slug_and_owner("abc".into(), "abc".into());
    config.tags.insert(tag.as_id(), tag);
    let score =
        starlark::QueryFilterCore::new("index(service=123)\ntag.abc", config, database.clone());
    for i in 0u64.. {
        score.get_meta();
        if i.is_power_of_two() {
            println!("{}", i);
        }
    } */

    let fut = tokio::spawn(async move {
        let addr = "0.0.0.0:10000".parse::<std::net::SocketAddr>().unwrap();
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
        // std::thread::sleep(std::time::Duration::from_millis(450));
        reassembler.expire().await;
    }

    fut.await.expect("wsserver died");
    Ok(())
}
