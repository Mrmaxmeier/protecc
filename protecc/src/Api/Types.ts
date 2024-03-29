import { Record, Number, Tuple, String, Static, Array, Union, Literal, Dictionary, Unknown, Null } from "runtypes";

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

export const DataSegment = Record({
    sender: Union(Literal('client'), Literal('server')),
    data: String,
    seq: Number,
    ack: Number,
    timestamp: Number,
    flags: Number
})
export type DataSegment = Static<typeof DataSegment>

export const StreamDetailed = StreamOverview.And(Record({
    segments: Array(DataSegment)
}))
export type StreamDetailed = Static<typeof StreamDetailed>

export const Flag = Union(Literal('fin'), Literal('syn'), Literal('rst'), Literal('psh'), Literal('ack'), Literal('ack'), Literal('urg'), Literal('ece'), Literal('cwr'))
export type Flag = Static<typeof Flag>
export const Flags: Flag[] = ['fin', 'syn', 'rst', 'psh', 'ack', 'urg', 'ece', 'cwr']

export function parseFlags(flags: number): Set<Flag> {
    let result = new Set<Flag>()
    Flags.forEach((f, i) => {
        if (((1 << i) & flags) !== 0)
            result.add(f)
    })
    return result
}


export const IndexSizes = Record({
    indexSizes: Record({
        services: Dictionary(Number, 'number'),
        tags: Dictionary(Number, 'number'),
    })
})
export type IndexSizes = Static<typeof IndexSizes>


export const QueryResult = Record({
    stream: StreamOverview,
    attached: Unknown.Or(Null),
    sortKey: Number.Or(Null),
    addedTags: Array(Number)
})

export type QueryResult = Static<typeof QueryResult>