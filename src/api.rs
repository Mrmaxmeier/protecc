use futures::Stream;
use tonic::{Request, Response, Status, Streaming};

pub mod api {
    tonic::include_proto!("api");
}

pub use api::{
    server,
    server::{ToolApi, ToolApiServer},
    RegisterPacketTaggerRequest, RegisterStreamMapperRequest, RegisterStreamReducerRequest,
    RegisterStreamTaggerRequest,
};
use std::pin::Pin;

pub struct ToolApiImpl {}

#[tonic::async_trait]
impl ToolApi for ToolApiImpl {
    type RegisterStreamTaggerStream =
        Pin<Box<dyn Stream<Item = Result<api::Stream, Status>> + Send + Sync + 'static>>;
    async fn register_stream_tagger(
        &self,
        request: Request<Streaming<RegisterStreamTaggerRequest>>,
    ) -> Result<Response<Self::RegisterStreamTaggerStream>, Status> {
        let stream = request.into_inner();

        let output = async_stream::try_stream! {
            futures::pin_mut!(stream);
            unimplemented!()
        };

        unimplemented!()
    }

    type RegisterStreamMapperStream =
        Pin<Box<dyn Stream<Item = Result<api::Stream, Status>> + Send + Sync + 'static>>;
    async fn register_stream_mapper(
        &self,
        request: Request<Streaming<RegisterStreamMapperRequest>>,
    ) -> Result<Response<Self::RegisterStreamMapperStream>, Status> {
        unimplemented!()
    }

    type RegisterStreamReducerStream = Pin<
        Box<
            dyn Stream<Item = Result<api::RegisterStreamReducerResponse, Status>>
                + Send
                + Sync
                + 'static,
        >,
    >;
    async fn register_stream_reducer(
        &self,
        request: Request<Streaming<RegisterStreamReducerRequest>>,
    ) -> Result<Response<Self::RegisterStreamReducerStream>, Status> {
        unimplemented!()
    }

    type RegisterPacketTaggerStream =
        Pin<Box<dyn Stream<Item = Result<api::UdpPacket, Status>> + Send + Sync + 'static>>;
    async fn register_packet_tagger(
        &self,
        request: Request<Streaming<RegisterPacketTaggerRequest>>,
    ) -> Result<Response<Self::RegisterPacketTaggerStream>, Status> {
        unimplemented!()
    }
}
