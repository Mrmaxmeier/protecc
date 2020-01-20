#![feature(drain_filter, duration_constants, async_closure)]
#![recursion_limit = "512"] // for futures::select!

// for heaptrack
use std::alloc::System;
#[global_allocator]
static GLOBAL: System = System;

pub(crate) mod configuration;
pub(crate) mod counters;
pub(crate) mod database;
pub(crate) mod pcapmanager;
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
    let pcap_folder = args().skip(1).next().unwrap_or("pcaps/".into());
    let mut pcap_process_rx = crate::pcapmanager::PcapManager::start(&pcap_folder);

    let database = database::Database::new();
    let mut reassembler = Reassembler::new(database.clone());

    let fut = tokio::spawn(async move {
        let addr = "0.0.0.0:10000".parse::<std::net::SocketAddr>().unwrap();
        let try_socket = TcpListener::bind(&addr).await;
        let mut listener = try_socket.expect("Failed to bind");
        println!("Listening on: {}", addr);

        while let Ok((stream, _)) = listener.accept().await {
            tokio::spawn(wsserver::accept_connection(stream, database.clone()));
        }
    });

    let fut2 = tokio::spawn(async move {
        while let Some(path) = pcap_process_rx.recv().await {
            println!("importing pcap {:?}", path);
            pcapreader::read_pcap_file(&path, &mut reassembler).await;
            {
                tracyrs::zone!("sleep between pcap imports");
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
            reassembler.expire().await;
        }
    });

    fut.await.expect("wsserver died");
    fut2.await.expect("parser died");
    Ok(())
}
