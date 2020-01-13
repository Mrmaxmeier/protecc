use crate::database::TagID;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tokio::sync::{mpsc, watch};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Tag {
    slug: String,
    name: String,
    color: String,
    owner: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Service {
    slug: String,
    name: String,
    port: u16,
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
    pub fn from_slug_and_owner(slug: String, owner: String) -> Self {
        Tag {
            name: slug.clone(),
            color: String::from("grey"),
            slug,
            owner,
        }
    }
}
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub(crate) enum ConfigurationUpdate {
    SetService(Service),
    RegisterTag { slug: String, owner: String },
    SetTag(Tag),
}

#[derive(Clone, Debug)]
pub(crate) struct ConfigurationHandle {
    pub(crate) rx: watch::Receiver<Configuration>,
    pub(crate) tx: mpsc::Sender<ConfigurationUpdate>,
}

impl ConfigurationHandle {
    pub(crate) async fn register_tag(&mut self, slug: String, owner: String) -> TagID {
        let tag_id = TagID::from_slug(slug.as_bytes());
        self.tx
            .send(ConfigurationUpdate::RegisterTag { slug, owner })
            .await
            .unwrap();

        // return iff config reflects registered tag
        while let Some(config) = self.rx.recv().await {
            if let Some(_) = config.tags.get(&tag_id) {
                return tag_id;
            }
        }
        panic!()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Configuration {
    tags: HashMap<TagID, Tag>,
    services: HashMap<ServiceID, Service>,
}

impl Configuration {
    pub(crate) fn spawn() -> ConfigurationHandle {
        let (tx_, rx) = watch::channel(Configuration::default());
        let (tx, rx_) = mpsc::channel(1);
        tokio::spawn(Self::arbitrate(rx_, tx_));
        ConfigurationHandle { rx, tx }
    }

    pub(crate) async fn arbitrate(
        mut rx: mpsc::Receiver<ConfigurationUpdate>,
        tx: watch::Sender<Configuration>,
    ) {
        let mut config = Configuration::default();
        while let Some(msg) = rx.recv().await {
            config.update(msg);
            tx.broadcast(config.clone()).unwrap();
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
            ConfigurationUpdate::RegisterTag { slug, owner } => {
                self.tags
                    .entry(TagID::from_slug(slug.as_bytes()))
                    .or_insert_with(|| Tag::from_slug_and_owner(slug, owner));
            }
        }
    }
}
