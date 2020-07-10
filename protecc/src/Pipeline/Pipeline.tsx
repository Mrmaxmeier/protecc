import React, { useState, useContext, useEffect, FC } from 'react';
import { Api } from '../Api/ProteccApi';
import { Record, Static, Dictionary, Number, Union, Literal, String, Boolean } from 'runtypes';
import { EmptyState, Gallery, GalleryItem, Card, CardHeader, CardHeaderMain, CardActions, Dropdown, KebabToggle, CardBody, CardFooter, DropdownItem, DropdownSeparator, Progress, ProgressVariant, Title, EmptyStateVariant, EmptyStateIcon, Stack, StackItem, Tooltip, Alert, Spinner, Bullseye } from '@patternfly/react-core';
import { Loading } from '../Components/Loading';
import { CubesIcon, PluggedIcon, StarIcon } from '@patternfly/react-icons';
import { ChartDonut, ChartThemeColor } from '@patternfly/react-charts';
import { formatPercent } from '../Util';


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
    catchingUp: Boolean,
    isStarlark: Boolean,
})
type NodeStatusSummary = Static<typeof NodeStatusSummary>

const PipelineStatus = Record({
    pipelineStatus: Record({
        nodes: Dictionary(NodeStatusSummary)
    })
})
type PipelineStatus = Static<typeof PipelineStatus>

const NodeControl: FC<NodeStatusSummary> = ({ kind, status, name, queuedStreams, processedStreams, missedStreams, catchingUp, isStarlark }) => {
    let api = useContext(Api)
    let [isOpen, setIsOpen] = useState(false)

    const dropdownItems = [
        <DropdownItem
            key="disable"
            component="button"
            isDisabled={status !== 'running'}
            onClick={() => api.emit({
                managePipelineNode: {
                    disable: name,
                }
            })}
        >
            Disable
        </DropdownItem>,
        <DropdownItem
            key="enable"
            component="button"
            isDisabled={status !== 'disabled'}
            onClick={() => api.emit({
                managePipelineNode: {
                    enable: name,
                }
            })}
        >
            Enable
        </DropdownItem>,
        <DropdownItem
            key="catch-up"
            component="button"
            isDisabled={missedStreams === 0 || status !== 'running' || catchingUp}
            onClick={() => api.emit({
                managePipelineNode: {
                    catchUp: name,
                }
            })}
        >
            Catch-Up
        </DropdownItem>,
        <DropdownSeparator key="separator" />,
        <DropdownItem
            key="remove"
            component="button"
            onClick={() => api.emit({
                managePipelineNode: {
                    remove: name,
                }
            })}
        >
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
        statusTitle = 'Errored'
    }
    let progress = processedStreams ? (processedStreams + queuedStreams) / processedStreams : 0;
    return <Card>
        <CardHeader>
            <CardHeaderMain>
                <span style={{ marginRight: '.3em' }}>
                    {isStarlark ? <Tooltip content='Starlark node'><StarIcon /></Tooltip> : <Tooltip content='External node'><PluggedIcon /></Tooltip>}
                </span>
            </CardHeaderMain>
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
        </CardHeader>
        <CardBody>
            <Stack hasGutter>
                {catchingUp &&
                    <StackItem>
                        <Bullseye>
                            Catching up...
                        </Bullseye>
                    </StackItem>
                }
                <StackItem>
                    <ChartDonut
                        constrainToVisibleArea
                        data={[
                            { x: 'Processed', y: processedStreams },
                            { x: 'Queued', y: queuedStreams },
                            { x: 'Missed', y: missedStreams }
                        ]}
                        labels={({ datum }: { datum: { x: string, y: number } }) => `${datum.x}: ${datum.y}`}
                        title={formatPercent(processedStreams / (processedStreams + queuedStreams + missedStreams) * 100) + '%'}
                        subTitle={'processed'}
                        themeColor={ChartThemeColor.default}
                    />
                </StackItem>
                {status !== 'running' && status !== 'disabled' &&
                    <StackItem>
                        <Alert variant='danger' title='Tagger errored'>{status.errored}</Alert>
                    </StackItem>
                }
            </Stack>

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
            <Gallery hasGutter>
                {filtered.map(node =>
                    <GalleryItem key={node}><NodeControl {...nodes[node]} /></GalleryItem>)}
            </Gallery>
        )
    }

    return <>
        <Stack hasGutter>
            <StackItem>
                <Stack hasGutter>
                    <StackItem>
                        <Title headingLevel="h1">Mappers</Title>
                    </StackItem>
                    <StackItem>
                        {nodeSection("mapper")}
                    </StackItem>
                </Stack>
            </StackItem>
            <StackItem>
                <Stack hasGutter>
                    <StackItem>
                        <Title headingLevel="h1">Taggers</Title>
                    </StackItem>
                    <StackItem>
                        {nodeSection("tagger")}
                    </StackItem>
                </Stack>
            </StackItem>
            <StackItem>
                <Stack hasGutter>
                    <StackItem>
                        <Title headingLevel="h1">Reducers</Title>
                    </StackItem>
                    <StackItem>
                        {nodeSection("reducer")}
                    </StackItem>
                </Stack>
            </StackItem>
        </Stack>
    </>
}