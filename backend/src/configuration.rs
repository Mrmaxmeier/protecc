use crate::database::TagID;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tokio::sync::{mpsc, watch};

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Tag {
    pub(crate) slug: String,
    pub(crate) name: String,
    pub(crate) color: String,
    pub(crate) owner: String,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Service {
    pub(crate) slug: String,
    pub(crate) name: String,
    pub(crate) port: u16,
}

impl Service {
    pub fn as_id(&self) -> ServiceID {
        ServiceID::from_slug(self.slug.as_bytes())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct ServiceID(u32);
impl ServiceID {
    pub fn from_slug(slug: &[u8]) -> Self {
        use std::hash::Hasher;
        let mut hasher = metrohash::MetroHash64::with_seed(0x1337_1337_1337_1337);
        hasher.write(slug);
        ServiceID(hasher.finish() as u32)
    }
}

impl Tag {
    pub fn as_id(&self) -> TagID {
        TagID::from_slug(self.slug.as_bytes())
    }
}
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub(crate) enum ConfigurationUpdate {
    SetService(Service),
    RegisterTag(Tag),
    SetTag(Tag),
    SetScript(String, String),
    RemoveScript(String),
}

#[derive(Clone, Debug)]
pub(crate) struct ConfigurationHandle {
    pub(crate) rx: watch::Receiver<Configuration>,
    pub(crate) tx: mpsc::Sender<ConfigurationUpdate>,
}

impl ConfigurationHandle {
    pub(crate) async fn register_tag(
        &mut self,
        slug: &str,
        owner: &str,
        name: &str,
        color: &str,
    ) -> TagID {
        let tag = Tag {
            slug: slug.into(),
            owner: owner.into(),
            name: name.into(),
            color: color.into(),
        };
        let tag_id = tag.as_id();
        let mut rx = self.rx.clone();
        self.tx
            .send(ConfigurationUpdate::RegisterTag(tag))
            .await
            .unwrap();
        // return once config reflects registered tag
        while let Ok(()) = rx.changed().await {
            if let Some(_) = rx.borrow().tags.get(&tag_id) {
                return tag_id;
            }
        }
        unreachable!()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Default, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Configuration {
    pub(crate) tags: HashMap<TagID, Tag>,
    pub(crate) services: HashMap<ServiceID, Service>,
    pub(crate) scripts: HashMap<String, String>,
}

impl Configuration {
    pub(crate) fn spawn(pcaps_folder: &Path) -> ConfigurationHandle {
        let mut path = pcaps_folder.to_path_buf();
        path.push("config.json");
        let config = Self::load_from_folder(&path).unwrap_or_default();
        let (tx_, rx) = watch::channel(config.clone());
        let (tx, rx_) = mpsc::channel(1);
        tokio::spawn(Self::arbitrate(rx_, tx_, path, config));
        ConfigurationHandle { rx, tx }
    }

    fn load_from_folder(path: &Path) -> Result<Configuration, std::io::Error> {
        std::fs::File::open(path)
            .map(|file| serde_json::from_reader(file).expect("couldn't deserialize config json"))
    }

    pub(crate) async fn arbitrate(
        mut rx: mpsc::Receiver<ConfigurationUpdate>,
        tx: watch::Sender<Configuration>,
        path: PathBuf,
        mut config: Configuration,
    ) {
        while let Some(msg) = rx.recv().await {
            let prev = config.clone();
            config.update(msg);
            tx.send(config.clone()).unwrap();
            if prev != config {
                let file = std::fs::File::create(path.clone()).unwrap();
                serde_json::to_writer_pretty(file, &config)
                    .expect("failed to persist config to disk");
            }
        }
    }

    fn update(&mut self, update: ConfigurationUpdate) {
        match update {
            ConfigurationUpdate::SetService(service) => {
                self.services.insert(service.as_id(), service);
            }
            ConfigurationUpdate::SetTag(tag) => {
                self.tags.insert(tag.as_id(), tag);
            }
            ConfigurationUpdate::RegisterTag(tag) => {
                self.tags.entry(tag.as_id()).or_insert_with(|| tag);
            }
            ConfigurationUpdate::SetScript(name, data) => {
                self.scripts.insert(name, data);
            }
            ConfigurationUpdate::RemoveScript(name) => {
                self.scripts.remove(&name);
            }
        }
    }
}
