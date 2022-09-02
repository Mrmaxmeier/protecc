#![recursion_limit = "512"] // for futures::select!

/*
// for heaptrack
use std::alloc::System;
#[global_allocator]
static GLOBAL: System = System;
*/

use clap::Parser;
use std::path::Path;
use tokio::net::TcpListener;

use snacc::database::Database;
use snacc::pcapmanager::PcapManager;
use snacc::pcapreader::read_pcap_file;
use snacc::reassembly::Reassembler;
use snacc::wsserver;

#[derive(Parser, Debug)]
struct Args {
    /// Pcap folder to watch and serve (will import .pcap and/or .pcap.zst files)
    #[clap(default_value = "pcaps/")]
    pcap_folder: String,

    /// Replace .pcaps with Zstd-compressed versions to save storage.
    #[clap(short = 'z', long)]
    compress_pcaps: bool,

    /// Sleep between pcap processing to simulate real-time imports (in seconds)
    #[clap(long)]
    import_delay: Option<f64>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // console_subscriber::init();
    let args = Args::parse();
    let mut pcap_process_rx = PcapManager::start(&args.pcap_folder, args.compress_pcaps);

    let database = Database::open(Path::new(&args.pcap_folder));
    let mut reassembler = Reassembler::new(database.clone());

    let fut = tokio::spawn(async move {
        let addr = "0.0.0.0:10000".parse::<std::net::SocketAddr>().unwrap();
        let try_socket = TcpListener::bind(&addr).await;
        let listener = try_socket.expect("Failed to bind");
        println!("Listening on: {}", addr);

        while let Ok((stream, _)) = listener.accept().await {
            tokio::spawn(wsserver::accept_connection(stream, database.clone()));
        }
        eprintln!("failed to accept websocket listener?!");
    });

    println!("connect ws now :)");
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;

    let fut2 = tokio::spawn(async move {
        while let Some(path) = pcap_process_rx.recv().await {
            println!("importing pcap {:?}", path);
            if let Err(err) = read_pcap_file(&path, &mut reassembler).await {
                eprintln!("{:?}", err)
            }
            if let Some(delay) = args.import_delay {
                tracyrs::message!("sleep between pcap imports");
                tokio::time::sleep(std::time::Duration::from_secs_f64(delay)).await;
            }
            reassembler.expire().await;
        }
    });

    fut.await.expect("wsserver died");
    fut2.await.expect("parser died");
    Ok(())
}
