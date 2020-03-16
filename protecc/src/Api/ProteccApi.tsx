import React, { Context, createContext, useState, useEffect } from 'react';
import io from 'socket.io-client';
import { Record, Static, String, Number, Unknown, Dictionary, Literal, Union } from 'runtypes';

const StreamMessage = Record({
    id: Number,
    payload: Unknown
});
type StreamMessage = Static<typeof StreamMessage>

export class ProteccApi {
    private socket: SocketIOClient.Socket
    private currentId = 0
    private listeners: { [id: number]: (msg: any) => void } = {}

    constructor(url: string) {
        this.socket = io(url);
        this.socket.on('msg', (msg: string) => {
            let streamMsg = StreamMessage.check(JSON.parse(msg));
            let listener = this.listeners[streamMsg.id];
            if (listener)
                listener(streamMsg.payload);
        });
    }

    emit(payload: any) {
        let id = this.currentId++;
        this.socket.emit('msg', JSON.stringify({ id, payload }));
        return id;
    }

    emitOn(id: number, payload: any) {
        this.socket.emit('msg', JSON.stringify({ id, payload }));
    }

    listen(payload: any, cb: (msg: any) => void): () => void {
        let id: number | null = null
        let cleanup = this.onConnect(() => {
            id = this.emit(payload);
            this.listeners[id] = cb;
        })
        return () => {
            cleanup()
            if (id !== null && this.listeners[id]) {
                delete this.listeners[id]
                this.emitOn(id, 'cancel')
            }
        }
    }

    onConnect(cb: () => void): () => void {
        if (this.socket.connected)
            cb();
        this.socket.on('connect', cb)
        return () => this.socket.removeListener('connect', cb)
    }
    onDisconnect(cb: (reason: string) => void) {
        this.socket.on('disconnect', cb)
        return () => this.socket.removeListener('disconnect', cb)
    }
    onReconnect(cb: (attemnt: number) => void) {
        this.socket.on('reconnecting', cb)
        return () => this.socket.removeListener('reconnecting', cb)
    }
    isConnected = () => this.socket.connected
}

const api = new ProteccApi("http://localhost:4000")
export const Api: Context<ProteccApi> = createContext(api);


// TODO: type
export type Counters = { [name: string]: number }
export const Counters = createContext<Counters | null>(null);


const Tag = Record({
    slug: String,
    name: String,
    color: String,
    owner: String
});
export type Tag = Static<typeof Tag>
const Service = Record({
    id: Number,
    slug: String,
    port: Number,
    name: String
});
export type Service = Static<typeof Service>
const Configuration = Record({
    tags: Dictionary(Tag),
    services: Dictionary(Service),
    scripts: Dictionary(String)
})
const OuterConfiguration = Record({
    configuration: Configuration
})
export type Configuration = Static<typeof Configuration>
type OuterConfiguration = Static<typeof OuterConfiguration>

export const Config = createContext<Configuration | null>(null);


export const ConnectionStatus = Union(
    Literal('connected'),
    Literal('disconnected'),
    Literal('reconnecting'),
)
export type ConnectionStatus = Static<typeof ConnectionStatus>

export const Connected = createContext<ConnectionStatus>('disconnected');


export const ApiProvider: React.FC = ({ children }) => {
    const [config, setConfig] = useState<Configuration | null>(null);
    const [connected, setConnected] = useState<ConnectionStatus>(api.isConnected ? 'connected' : 'disconnected');

    useEffect(() => api.listen({ watch: 'configuration' }, (msg) =>
        setConfig(OuterConfiguration.check(msg).configuration)
    ), []);

    useEffect(() => {
        let connectCleanup = api.onConnect(() => setConnected('connected'))
        let reconnectingCleanup = api.onReconnect(() => setConnected('reconnecting'))
        let disconnectCleanup = api.onDisconnect(() => setConnected('disconnected'))
        return () => {
            connectCleanup();
            reconnectingCleanup();
            disconnectCleanup();
        }
    }, [])

    return (
        <Api.Provider value={api}>
            <Config.Provider value={config}>
                <Connected.Provider value={connected}>
                    {children}
                </Connected.Provider>
            </Config.Provider>
        </Api.Provider>
    )
}