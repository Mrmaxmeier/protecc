import React, { useState, useContext, useEffect, FC } from 'react';
import { Api } from '../Api/ProteccApi';
import { Record, Static, Dictionary, Number, Union, Literal, String } from 'runtypes';
import { EmptyState, Gallery, GalleryItem, Card, CardHead, CardActions, Dropdown, KebabToggle, CardHeader, CardBody, CardFooter, DropdownItem, DropdownSeparator, Progress, ProgressVariant, Title } from '@patternfly/react-core';
import { Loading } from '../Components/Loading';


const NodeKind = Union(
    Literal('mapper'),
    Literal('reducer'),
    Literal('tagger'),
)
type NodeKind = Static<typeof NodeKind>

const NodeStatus = Union(
    Literal('running'),
    Literal('disabled'),
    Record({ errored: String }),
)

const NodeStatusSummary = Record({
    kind: NodeKind,
    status: NodeStatus,
    name: String,
    queuedStreams: Number,
    processedStreams: Number,
})
type NodeStatusSummary = Static<typeof NodeStatusSummary>

const PipelineStatus = Record({
    pipelineStatus: Record({
        nodes: Dictionary(NodeStatusSummary)
    })
})
type PipelineStatus = Static<typeof PipelineStatus>

const NodeControl: FC<NodeStatusSummary> = ({ kind, status, name, queuedStreams, processedStreams }) => {
    let [isOpen, setIsOpen] = useState(false)

    const dropdownItems = [
        <DropdownItem key="link">Link</DropdownItem>,
        <DropdownItem key="action" component="button">
            Action
        </DropdownItem>,
        <DropdownItem key="disabled link" isDisabled>
            Disabled Link
        </DropdownItem>,
        <DropdownItem key="disabled action" isDisabled component="button">
            Disabled Action
        </DropdownItem>,
        <DropdownSeparator key="separator" />,
        <DropdownItem key="separated link">Separated Link</DropdownItem>,
        <DropdownItem key="separated action" component="button">
            Separated Action
        </DropdownItem>
    ];

    let statusTitle;
    if (status === 'running') {
        statusTitle = 'Running';
        if (queuedStreams === 0)
            statusTitle += ' (Idle)'
    } else if (status === 'disabled') {
        statusTitle = 'Disabled';
    } else {
        statusTitle = 'Errored: ' + status.errored
    }
    let progress = processedStreams ? (processedStreams + queuedStreams) / processedStreams : 0;
    return <Card>
        <CardHead>
            <CardActions>
                <Dropdown
                    onSelect={() => { }}
                    toggle={<KebabToggle onToggle={() => setIsOpen(open => !open)} />}
                    isOpen={isOpen}
                    isPlain
                    dropdownItems={dropdownItems}
                    position={'right'}
                />
            </CardActions>
        </CardHead>
        <CardHeader>{name}</CardHeader>
        <CardBody>
            <pre>{JSON.stringify({ name, kind, status, queuedStreams, processedStreams }, null, 2)}</pre>
        </CardBody>
        <CardFooter>
            <Progress value={progress * 100} title={statusTitle} variant={status === 'running' ? ProgressVariant.success : ProgressVariant.danger} />
        </CardFooter>
    </Card>
}

export function Taggers() {
    let api = useContext(Api)
    const [pipelineStatus, setPipelineStatus] = useState<PipelineStatus | null>(null);

    useEffect(() => api.listen({ watch: 'pipelineStatus' }, status =>
        setPipelineStatus(PipelineStatus.check(status))
    ), [api]);

    if (pipelineStatus === null) return <Loading />

    if (pipelineStatus?.pipelineStatus.nodes === {})
        return <EmptyState>
            TODO: empty state
        </EmptyState>;

    let nodes = pipelineStatus.pipelineStatus.nodes
    let sorted = Object.keys(nodes).sort()

    let nodeSection = (kind: NodeKind) => {
        return (
            <Gallery gutter="md">
                {sorted
                    .filter(node => nodes[node].kind === kind)
                    .map(node => <GalleryItem key={node}><NodeControl {...nodes[node]} /></GalleryItem>)}
            </Gallery>
        )
    }

    return <>
        <Title size="4xl">Mappers</Title>
        {nodeSection("mapper")}
        <Title size="4xl">Taggers</Title>
        {nodeSection("tagger")}
        <Title size="4xl">Reducers</Title>
        {nodeSection("reducer")}
    </>
}