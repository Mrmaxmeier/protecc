import React, { useState, useContext, useEffect, FC } from 'react';
import { Api } from '../Api/ProteccApi';
import { Record, Static, Dictionary, Number, Union, Literal, String } from 'runtypes';
import { EmptyState, Gallery, GalleryItem, Card, CardHead, CardActions, Dropdown, KebabToggle, CardBody, CardFooter, DropdownItem, DropdownSeparator, Progress, ProgressVariant, Title, EmptyStateVariant, EmptyStateIcon } from '@patternfly/react-core';
import { Loading } from '../Components/Loading';
import { CubesIcon } from '@patternfly/react-icons';


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
    missedStreams: Number,
})
type NodeStatusSummary = Static<typeof NodeStatusSummary>

const PipelineStatus = Record({
    pipelineStatus: Record({
        nodes: Dictionary(NodeStatusSummary)
    })
})
type PipelineStatus = Static<typeof PipelineStatus>

const NodeControl: FC<NodeStatusSummary> = ({ kind, status, name, queuedStreams, processedStreams, missedStreams }) => {
    let [isOpen, setIsOpen] = useState(false)

    const dropdownItems = [
        <DropdownItem key="disable" component="button" isDisabled={status !== 'running'}>
            Disable
        </DropdownItem>,
        <DropdownItem key="catch-up" component="button" isDisabled={missedStreams === 0}>
            Catch-Up
        </DropdownItem>,
        <DropdownSeparator key="separator" />,
        <DropdownItem key="remove" component="button">
            Remove
        </DropdownItem>,
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
            {name}
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
        <CardBody>
            <pre>{JSON.stringify({ name, kind, status, queuedStreams, processedStreams, missedStreams }, null, 2)}</pre>
        </CardBody>
        <CardFooter>
            <Progress value={progress * 100} title={statusTitle} variant={status === 'running' ? ProgressVariant.success : ProgressVariant.danger} />
        </CardFooter>
    </Card>
}

export function Pipeline() {
    let api = useContext(Api)
    const [pipelineStatus, setPipelineStatus] = useState<PipelineStatus | null>(null);

    useEffect(() => api.listen({ watch: 'pipelineStatus' }, status =>
        setPipelineStatus(PipelineStatus.check(status))
    ), [api]);

    if (pipelineStatus === null) return <Loading />

    let nodes = pipelineStatus.pipelineStatus.nodes
    let sorted = Object.keys(nodes).sort()

    let nodeSection = (kind: NodeKind) => {
        let filtered = sorted.filter(node => nodes[node].kind === kind)
        if (filtered.length === 0)
            return <EmptyState variant={EmptyStateVariant.full}>
                <EmptyStateIcon icon={CubesIcon} />
                <Title headingLevel="h5" size="lg">
                    No {kind} nodes (... yet!)
                </Title>
            </EmptyState>
        return (
            <Gallery gutter="md">
                {filtered.map(node =>
                    <GalleryItem key={node}><NodeControl {...nodes[node]} /></GalleryItem>)}
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