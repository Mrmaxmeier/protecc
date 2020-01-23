use std::ffi::OsString;
use std::fs;
use std::fs::File;
use std::ops::RangeInclusive;
use std::path::PathBuf;

use futures::StreamExt;
use inotify::{Event, Inotify, WatchMask};
use tokio::sync::mpsc;

use crate::database::StreamID;

struct PcapRange {
    path: String,
    range: RangeInclusive<StreamID>,
}

pub(crate) struct PcapManager {
    imported: Vec<PcapRange>,
}

impl PcapManager {
    pub(crate) fn start(pcap_folder: &str) -> mpsc::UnboundedReceiver<PathBuf> {
        let mut inotify = Inotify::init().expect("Failed to initialize inotify");

        let (tx, rx) = mpsc::unbounded_channel();

        let path = PathBuf::from(pcap_folder);

        inotify
            .add_watch(path.clone(), WatchMask::CLOSE_WRITE)
            .expect("couldn't watch pcap folder");

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
            tx.send(file_path).unwrap();
        }

        tokio::spawn(async move {
            let mut buffer = vec![0; 128];
            let mut stream = inotify.event_stream(&mut buffer).unwrap();
            while let Some(event_or_error) = stream.next().await {
                println!("inotify: {:?}", event_or_error);
                let event: Event<OsString> = event_or_error.unwrap();
                if let Some(name) = event.name {
                    let file_path = path.join(name);
                    if file_path.extension().and_then(|x| x.to_str()) == Some("zst") {
                        tx.send(file_path)
                            .expect("pcapmanager rx dropped before send");
                    } else {
                        Self::compress_pcap(&file_path)
                    }
                }
            }
        });

        rx
    }

    fn compress_pcap(path: &PathBuf) {
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
