#![recursion_limit = "512"] // for futures::select!

// for heaptrack
use std::alloc::System;
#[global_allocator]
static GLOBAL: System = System;

use snacc::database::Database;
use snacc::pcapmanager::PcapManager;
use snacc::pcapreader::read_pcap_file;
use snacc::reassembly::Reassembler;
use snacc::wsserver;

use std::env::args;
use std::path::Path;
use tokio::net::TcpListener;

const SLEEP_BETWEEN_PCAPS: u64 = 0;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let pcap_folder = args().skip(1).next().unwrap_or("pcaps/".into());
    let mut pcap_process_rx = PcapManager::start(&pcap_folder);

    let database = Database::open(Path::new(&pcap_folder));
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

    println!("connect ws now :)");
    tokio::time::delay_for(std::time::Duration::from_secs(1)).await;

    let fut2 = tokio::spawn(async move {
        while let Some(path) = pcap_process_rx.recv().await {
            println!("importing pcap {:?}", path);
            if let Err(err) = read_pcap_file(&path, &mut reassembler).await {
                eprintln!("{:?}", err)
            }
            if SLEEP_BETWEEN_PCAPS != 0 {
                tracyrs::message!("sleep between pcap imports");
                tokio::time::delay_for(std::time::Duration::from_millis(SLEEP_BETWEEN_PCAPS)).await;
            }
            reassembler.expire().await;
        }
    });

    fut.await.expect("wsserver died");
    fut2.await.expect("parser died");
    Ok(())
}
