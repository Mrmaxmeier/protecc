import React, { Context, createContext, useState, useEffect, useContext, useCallback } from 'react';
import io from 'socket.io-client';
import { Record, Static, String, Number, Unknown, Dictionary, Literal, Union } from 'runtypes';
import { SemanticColor } from '../Components/ColoredLabel';
import { StreamDetailed } from './Types';
import process from "process";

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

    listen(payload: any, cb: (msg: any) => void, idCb?: (id: number) => void): () => void {
        let id: number | null = null
        let cleanup = this.onConnect(() => {
            id = this.emit(payload);
            this.listeners[id] = cb;
            if (idCb) idCb(id)
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

const api = new ProteccApi(window.location.protocol + '//' + window.location.host)
export const Api: Context<ProteccApi> = createContext(api);


// TODO: type
export type Counters = { [name: string]: number }


const Tag = Record({
    slug: String,
    name: String,
    color: SemanticColor,
    owner: String
});
export type Tag = Static<typeof Tag>
const Service = Record({
    slug: String,
    port: Number,
    name: String
});
export type Service = Static<typeof Service>
const Configuration = Record({
    tags: Dictionary(Tag, 'number'),
    services: Dictionary(Service, 'number'),
    scripts: Dictionary(String)
})
const OuterConfiguration = Record({
    configuration: Configuration
})
export type Configuration = Static<typeof Configuration>
type OuterConfiguration = Static<typeof OuterConfiguration>

export const Config = createContext<Configuration | null>(null);

export function serviceFromPort(services: { [id: number]: Service }, port: number): Service | null {
    return Object.values(services).find(s => s.port === port) || null
}


export const ConnectionStatus = Union(
    Literal('connected'),
    Literal('disconnected'),
    Literal('reconnecting'),
)
export type ConnectionStatus = Static<typeof ConnectionStatus>

export const Connected = createContext<ConnectionStatus>('disconnected');


export function useUpdatingValue<T>(payload: any, check: (msg: any) => T, dependencies: any[]): T | null {
    const api = useContext(Api)
    const [value, setValue] = useState<T | null>(null)

    const cb = useCallback(check, [])

    useEffect(() => api.listen(payload, (msg) =>
        setValue(cb(msg))
        // eslint-disable-next-line
    ), [api, cb].concat(dependencies))

    return value
}

const ConfigProvider: React.FC = ({ children }) => {
    const config = useUpdatingValue({ watch: 'configuration' }, m => OuterConfiguration.check(m).configuration, []);
    return <Config.Provider value={config}>{children}</Config.Provider>
}

export const ApiProvider: React.FC = ({ children }) => {
    const [connected, setConnected] = useState<ConnectionStatus>(api.isConnected() ? 'connected' : 'disconnected');

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
            <ConfigProvider >
                <Connected.Provider value={connected}>
                    {children}
                </Connected.Provider>
            </ConfigProvider>
        </Api.Provider>
    )
}


export const DetailsUpdate = Record({
    streamDetails: StreamDetailed
})
export type DetailsUpdate = Static<typeof DetailsUpdate>

export function useStream(id: number) {
    return useUpdatingValue({ watch: { streamDetails: id } }, m => DetailsUpdate.check(m).streamDetails, [id])
}