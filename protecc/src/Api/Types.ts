import { Record, Number, Tuple, String, Static, Array } from "runtypes";

export const Endpoint = Tuple(String, Number)
export type Endpoint = Static<typeof Endpoint>

export function prettyPrintEndpoint(e: Endpoint) {
    return e[0] + ':' + e[1]
}

export const StreamOverview = Record({
    id: Number,
    client: Endpoint,
    server: Endpoint,
    tags: Array(Number),
    clientDataLen: Number,
    serverDataLen: Number
})
export type StreamOverview = Static<typeof StreamOverview>