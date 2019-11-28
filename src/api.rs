use tonic::{Request, Response, Status, Streaming};
use futures::Stream;

pub mod api {
    tonic::include_proto!("api");
}

pub use api::{
    server,
    server::{ToolApi, ToolApiServer},
    RegisterStreamTaggerRequest,
    RegisterStreamMapperRequest,
    RegisterStreamReducerRequest,
    RegisterPacketTaggerRequest
};
use std::pin::Pin;

pub struct ToolApiImpl {}

#[tonic::async_trait]
impl ToolApi for ToolApiImpl {
    type RegisterStreamTaggerStream = Pin<Box<dyn Stream<Item = Result<api::Stream, Status>> + Send + Sync + 'static>>;
    async fn register_stream_tagger(&self, request: Request<Streaming<RegisterStreamTaggerRequest>>) -> Result<Response<Self::RegisterStreamTaggerStream>, Status> {
        let stream = request.into_inner();

        let output = async_stream::try_stream! {
            futures::pin_mut!(stream);

            while let Some(request) = stream.next().await {
                let note = note?;

                let location = note.location.clone().unwrap();

                let location_notes = notes.entry(location).or_insert(vec![]);
                location_notes.push(note);

                for note in location_notes {
                    yield note.clone();
                }
            }
        };

        Ok(Response::new(Box::pin(output)
            as Pin<
            Box<dyn Stream<Item = Result<RouteNote, Status>> + Send + Sync + 'static>,
        >))
    }

    type RegisterStreamMapperStream = Pin<Box<dyn Stream<Item = Result<api::Stream, Status>> + Send + Sync + 'static>>;
    async fn register_stream_mapper(&self, request: Request<Streaming<RegisterStreamMapperRequest>>) -> Result<Response<Self::RegisterStreamMapperStream>, Status> {
        unimplemented!()
    }

    type RegisterStreamReducerStream = Pin<Box<dyn Stream<Item = Result<api::RegisterStreamReducerResponse, Status>> + Send + Sync + 'static>>;
    async fn register_stream_reducer(&self, request: Request<Streaming<RegisterStreamReducerRequest>>) -> Result<Response<Self::RegisterStreamReducerStream>, Status> {
        unimplemented!()
    }

    type RegisterPacketTaggerStream = Pin<Box<dyn Stream<Item = Result<api::UdpPacket, Status>> + Send + Sync + 'static>>;
    async fn register_packet_tagger(&self, request: Request<Streaming<RegisterPacketTaggerRequest>>) -> Result<Response<Self::RegisterPacketTaggerStream>, Status> {
        unimplemented!()
    }
}