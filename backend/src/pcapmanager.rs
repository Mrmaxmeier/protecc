use std::fs;
use std::fs::File;
// use std::ops::RangeInclusive;
use std::path::PathBuf;

use notify::event::{AccessKind, AccessMode, Event, EventKind};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;

const ENABLE_COMPRESSION: bool = true;

/*
use crate::database::StreamID;
struct PcapRange {
    path: String,
    range: RangeInclusive<StreamID>,
}
*/

pub(crate) struct PcapManager {
    // imported: Vec<PcapRange>,
}

impl PcapManager {
    pub(crate) fn start(pcap_folder: &str) -> mpsc::UnboundedReceiver<PathBuf> {
        let (tx, rx) = mpsc::unbounded_channel();

        let path = PathBuf::from(pcap_folder);

        let mut existing_pcaps = path
            .read_dir()
            .expect("couldn't list pcap folder")
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<Result<Vec<_>, _>>()
            .expect("io error listing pcap folder");

        existing_pcaps.sort();
        println!(
            "queueing {} existing pcaps from folder...",
            existing_pcaps.len()
        );

        for file_path in existing_pcaps.into_iter() {
            if let Some(file_name) = file_path.file_name() {
                if !file_name.to_string_lossy().contains(".pcap") {
                    continue;
                }
            }
            if ENABLE_COMPRESSION && file_path.extension().and_then(|x| x.to_str()) == Some("zst") {
                tx.send(file_path)
                    .expect("pcapmanager rx dropped before send");
            } else {
                Self::compress_pcap(&file_path)
            }
        }

        let mut watcher: RecommendedWatcher =
            Watcher::new_immediate(move |event: notify::Result<Event>| {
                let event = event.unwrap();
                dbg!(&event);
                // TODO: windows doesn't emit AccessKind::Close :/
                if let EventKind::Access(AccessKind::Close(AccessMode::Write)) = event.kind {
                    for file_path in event.paths {
                        if let Some(file_name) = file_path.file_name() {
                            if !file_name.to_string_lossy().contains(".pcap") {
                                continue;
                            }
                        }
                        if ENABLE_COMPRESSION
                            && file_path.extension().and_then(|x| x.to_str()) == Some("zst")
                        {
                            tx.send(file_path)
                                .expect("pcapmanager rx dropped before send");
                        } else {
                            Self::compress_pcap(&file_path)
                        }
                    }
                }
            })
            .expect("failed to initialize notify backend");

        watcher.watch(dbg!(path), RecursiveMode::Recursive).unwrap();

        std::mem::forget(watcher); // keep this alive for the whole session

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
