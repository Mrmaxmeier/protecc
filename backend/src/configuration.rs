use crate::database::TagID;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tokio::sync::{mpsc, watch};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
struct Tag {
    slug: String,
    name: String,
    color: String,
}

impl Tag {
    pub fn as_id(&self) -> TagID {
        use std::hash::Hasher;
        let mut hasher = metrohash::MetroHash64::with_seed(0x1337_1337_1337_1337);
        hasher.write(self.slug.as_bytes());
        TagID(hasher.finish())
    }
}
#[derive(Clone, Debug)]
pub(crate) enum ConfigurationUpdate {}

#[derive(Clone, Debug)]
pub(crate) struct ConfigurationHandle {
    pub(crate) rx: watch::Receiver<Configuration>,
    pub(crate) tx: mpsc::Sender<ConfigurationUpdate>,
}

impl ConfigurationHandle {
    pub fn register_tag(&mut self, slug: String) {}
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Configuration {
    tags: HashMap<TagID, Tag>,
    services: HashMap<u16, String>,
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
        match update {}
    }
}
