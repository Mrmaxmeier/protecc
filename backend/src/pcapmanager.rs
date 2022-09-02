use std::fs;
use std::fs::File;
// use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::time::Duration;

use notify_debouncer_mini::{new_debouncer, notify, DebouncedEventKind};
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

            if file_name.ends_with(".pcap.zst") || (file_name.ends_with(".pcap") && !compress_pcaps)
            {
                tx.send(file_path)
                    .expect("pcapmanager rx dropped before send");
            } else if file_name.ends_with(".pcap") && compress_pcaps {
                Self::compress_pcap(&file_path)
            }
        };

        let (tx_, rx_) = std::sync::mpsc::channel();

        // Select recommended watcher for debouncer.
        // Using a callback here, could also be a channel.
        let mut watcher = new_debouncer(Duration::from_secs(2), None, tx_).unwrap();

        // Add a path to be watched. All files and directories at that path and
        // below will be monitored for changes.
        watcher
            .watcher()
            .watch(&path, notify::RecursiveMode::Recursive)
            .expect("failed to initialize notify backend");

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
                match event {
                    Ok(events) => {
                        if events.len() > 1 {
                            eprintln!("pcap watcher events:");
                            for event in &events {
                                eprintln!("- {:?}", event);
                            }
                        } else if !events.is_empty() {
                            eprintln!("pcap watcher event: {:?}", events[0]);
                        }
                        for event in events {
                            if event.kind == DebouncedEventKind::Any && event.path.exists() {
                                // TODO: track duplicates?
                                handle_file(event.path, &tx);
                            }
                        }
                    }
                    Err(err) => eprintln!("pcap watcher errored: {:?}", err),
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
        if path_zst.exists() {
            println!(
                "compressing pcap {:?} ... compressed path exists already?",
                path
            );
            return;
        }
        let dst = File::create(path_zst).expect("couldn't create .pcap.zst");
        zstd::stream::copy_encode(src, dst, 11).expect("failed to compress pcap");
        fs::remove_file(path).expect("failed to remove non-compressed file");
    }
}
