use std::fs;
use std::fs::File;
// use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::time::Duration;

use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;

/*
use crate::database::StreamID;
struct PcapRange {
    path: String,
    range: RangeInclusive<StreamID>,
}
*/

pub struct PcapManager {
    // imported: Vec<PcapRange>,
}

impl PcapManager {
    pub fn start(pcap_folder: &str, compress_pcaps: bool) -> mpsc::UnboundedReceiver<PathBuf> {
        let (tx, rx) = mpsc::unbounded_channel();

        let path = PathBuf::from(pcap_folder);

        assert!(path.exists(), "pcap folder does not exist ({:?})", path);

        let mut existing_pcaps = path
            .read_dir()
            .expect("couldn't list pcap folder")
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<Result<Vec<_>, _>>()
            .expect("io error listing pcap folder");

        let handle_file = move |file_path: PathBuf, tx: &mpsc::UnboundedSender<PathBuf>| {
            let file_name = match file_path.file_name() {
                Some(name) => name.to_str().expect("non-utf8 pcap file name").to_owned(),
                None => return,
            };

            dbg!(&file_name);

            if file_name.ends_with(".pcap.zst") || (file_name.ends_with(".pcap") && !compress_pcaps)
            {
                tx.send(file_path)
                    .expect("pcapmanager rx dropped before send");
            } else if file_name.ends_with(".pcap") && compress_pcaps {
                Self::compress_pcap(&file_path)
            }
        };

        let (tx_, rx_) = std::sync::mpsc::channel();

        let mut watcher: RecommendedWatcher =
            Watcher::new(tx_, Duration::from_secs(1)).expect("failed to initialize notify backend");

        watcher.watch(&path, RecursiveMode::Recursive).unwrap();

        std::mem::forget(watcher); // keep this alive for the whole session

        existing_pcaps.sort();
        println!(
            "queueing {} existing pcaps from folder...",
            existing_pcaps.len()
        );

        // Handle existing files
        for file_path in existing_pcaps.into_iter() {
            handle_file(file_path, &tx);
        }

        // Catch up with newly created and live files
        std::thread::spawn(move || {
            for event in rx_ {
                eprintln!("{:?}", event);
                if let DebouncedEvent::Write(file_path) = event {
                    handle_file(file_path, &tx);
                }
            }
        });

        rx
    }

    fn compress_pcap(path: &PathBuf) {
        tracyrs::zone!("compress_pcap");
        println!("compressing pcap {:?}...", path);
        let src = File::open(path).expect("couldn't open file");

        let path_zst = match path.extension() {
            Some(extension) => path.with_extension(format!("{}.zst", extension.to_str().unwrap())),
            None => path.with_extension("zst"),
        };
        let dst = File::create(path_zst).expect("couldn't create .pcap.zst");
        zstd::stream::copy_encode(src, dst, 11).expect("failed to compress pcap");
        fs::remove_file(path).expect("failed to remove non-compressed file");
    }
}
