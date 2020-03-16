import React, { useState, useContext, useEffect } from 'react';
import { Api } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { Record, Static, Dictionary, Union, Literal, String } from 'runtypes';


const NodeKind = Union(
    Literal('mapper'),
    Literal('reducer'),
    Literal('tagger'),
)

const NodeStatus = Union(
    Literal('running'),
    Literal('disabled'),
    Record({ errored: String }),
)

const NodeStatusSummary = Record({
    kind: NodeKind,
    status: NodeStatus,
})

const PipelineStatus = Record({
    pipelineStatus: Record({
        nodes: Dictionary(NodeStatusSummary)
    })
})
type PipelineStatus = Static<typeof PipelineStatus>

export function Taggers() {
    let api = useContext(Api)
    const [pipelineStatus, setPipelineStatus] = useState<PipelineStatus | null>(null);

    useEffect(() => api.listen({ watch: 'pipelineStatus' }, status =>
        setPipelineStatus(PipelineStatus.check(status))
    ), [api]);

    if (pipelineStatus == null) return <Loading />;
    return <pre>{JSON.stringify(pipelineStatus, null, 2)}</pre>;
}