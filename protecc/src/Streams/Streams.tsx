import React, { useContext, useState, useEffect } from 'react';
import { Config, Api } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { Stack, StackItem, Split, SplitItem, OptionsMenu, OptionsMenuToggle, OptionsMenuItem, TextInput, Pagination, Flex, FlexItem, FlexModifiers, Switch, Modal, Title } from '@patternfly/react-core';
import { onEnter, nanToNull } from '../Util';
import { Table, TableHeader, TableBody, cellWidth, RowWrapperProps } from '@patternfly/react-table';
import { useHistory, useParams, Link } from 'react-router-dom';
import { ColoredDot } from '../Components/ColoredDot';
import { Record, Array, Number, Static } from 'runtypes';
import { StreamOverview, prettyPrintEndpoint } from '../Api/Types';
import { GlobalHotKeys } from 'react-hotkeys';
import { Tags } from '../Components/Tags';
import { StreamDetails } from '../Components/StreamDetails';



interface Params {
    port: number | null
    tag: number | null
}

function makeUrl({ port, tag }: Params): string {
    if (port !== null && tag !== null)
        return '/streams/' + port.toString() + '/' + tag
    if (tag !== null)
        return '/streams/tag/' + tag
    if (port !== null)
        return '/streams/port/' + port.toString()
    return '/streams'
}



function Menu(props: { name: string, id: string, menuItems: any[] }) {
    let [open, setOpen] = useState(false);

    return <OptionsMenu
        isOpen={open}
        id={props.id}
        menuItems={props.menuItems}
        toggle={<OptionsMenuToggle id='toggle' onToggle={(open) => setOpen(open)} toggleTemplate={props.name} />}
    />
}



function PortMenu({ port, tag }: Params) {
    let history = useHistory();
    let config = useContext(Config);

    let selectedService = (port !== null && config !== null && Object.values(config.services).find((s) => s.port === port)) || null

    let [input, setInput] = useState((selectedService !== null || port === null) ? '' : port.toString());

    if (config === null)
        return <Loading />

    let items = [
        <TextInput
            key="inputitem"
            placeholder={'Port'}
            value={input}
            type='text'
            id='portinput'
            onChange={setInput}
            css=''
            onKeyPress={onEnter(() => history.push(makeUrl({ port: nanToNull(parseInt(input)), tag })))}
        />,
    ].concat(Object.entries(config.services).map(([id, service]) =>
        <OptionsMenuItem
            isSelected={port === service.port}
            key={id}
            onSelect={() => history.push(makeUrl({ port: service.port, tag }))}
        >
            {service.name}
        </OptionsMenuItem>
    ))


    return <Menu name={selectedService !== null ? selectedService.name : (port === null ? "Port" : port.toString())} id='port-menu' menuItems={items} />
}



function TagMenu(params: Params) {
    let history = useHistory();
    let config = useContext(Config);


    let selectedTag = params.tag !== null && config !== null && config.tags[params.tag] ? config.tags[params.tag] : null

    if (config === null)
        return <Loading />

    let items = [
        <OptionsMenuItem
            isSelected={params.tag === null}
            key={'empty'}
            onSelect={() => history.push(makeUrl({ port: params.port, tag: null }))}
        >&nbsp;</OptionsMenuItem>
    ].concat(Object.entries(config.tags).map(([id, tag]) => ({ id: parseInt(id), tag })).map(({ id, tag }) =>
        <OptionsMenuItem
            isSelected={params.tag === id}
            key={id.toString()}
            onSelect={() => history.push(makeUrl({ port: params.port, tag: id }))}
        >
            <ColoredDot useSemanticColors color={tag.color} /> {tag.name}
        </OptionsMenuItem>
    ))


    return <Menu name={selectedTag === null ? 'Tag' : selectedTag.name} id='tag-menu' menuItems={items} />
}



const WindowUpdate = Record({
    windowUpdate: Record({
        new: Array(StreamOverview),
        changed: Array(StreamOverview),
        deleted: Array(Number)
    })
})
type WindowUpdate = Static<typeof WindowUpdate>

function StreamsTable(params: Params) {
    let api = useContext(Api)

    let [loaded, setLoaded] = useState<StreamOverview[]>([])
    let [windowParams, setWindowParams] = useState({ pages: 2, attached: true })
    const pageSize = 50
    let [page, setPage] = useState(1)
    let [streamId, setStreamId] = useState<number | null>(null)
    let [details, setDetails] = useState<number | null>(null)
    let [detailsOpen, setDetailsOpen] = useState(false)

    useEffect(() => {
        if (loaded.length > windowParams.pages * pageSize)
            setLoaded((loaded) => loaded.slice(0, windowParams.pages * pageSize))
        if (page > 1)
            setWindowParams({ attached: false, pages: windowParams.pages + (page >= windowParams.pages ? 1 : 0) })
    }, [loaded.length, windowParams.pages, windowParams.attached, page])


    useEffect(() => {
        if (detailsOpen)
            setWindowParams((params) => ({ ...params, attached: false }))
    }, [detailsOpen])

    useEffect(() => {
        if (streamId != null)
            api.emit({ windowUpdate: { id: streamId, params: { size: windowParams.pages * pageSize, attached: windowParams.attached } } })
    }, [api, windowParams.pages, windowParams.attached, streamId])

    useEffect(() => {
        let index = (() => {
            if (params.port !== null && params.tag !== null)
                return { serviceTagged: [params.port, params.tag] }
            if (params.port !== null)
                return { service: params.port }
            if (params.tag !== null)
                return { tagged: params.tag }
            return 'all'
        })()
        let windowParams = { size: pageSize * 2, attached: true }

        api.listen({ watch: { window: { index, params: windowParams } } }, (msg) => {
            let { windowUpdate } = WindowUpdate.check(msg)
            setLoaded((loaded) => {
                let newLoaded = loaded.slice(0)
                let deleted = new Set(windowUpdate.deleted.concat(windowUpdate.changed.map((a) => a.id)))

                newLoaded = newLoaded
                    .filter((e) => !deleted.has(e.id))
                    .concat(windowUpdate.new)
                    .concat(windowUpdate.changed)
                    .sort((a, b) => b.id - a.id)
                return newLoaded
            })
        }, setStreamId)
    }, [params, api])

    let columns = [
        { transforms: [cellWidth(10)], title: 'Id' },
        { transforms: [cellWidth(10)], title: 'Server' },
        { transforms: [cellWidth(10)], title: 'Client' },
        { transforms: [cellWidth(10)], title: 'Server  data' },
        { transforms: [cellWidth(10)], title: 'Client data' },
        { transforms: [cellWidth('max')], title: 'Tags' },
    ]
    let rows = loaded.map((stream) => ({
        cells: [
            { title: <Link to={'/stream/' + stream.id}>{stream.id}</Link> },
            prettyPrintEndpoint(stream.server),
            prettyPrintEndpoint(stream.client),
            stream.serverDataLen,
            stream.clientDataLen,
            { title: <Tags tags={stream.tags} /> }
        ],
        key: stream.id
    }))

    const customRowWrapper = ({ rowProps, ...props }: RowWrapperProps) => {
        let clicked = rowProps && (
            loaded[pageSize * (page - 1) + rowProps.rowIndex].id === details
        )
        const customStyle = {
            borderLeft: '3px solid var(--pf-global--primary-color--100)'
        }
        return (
            <tr
                {...props}
                style={clicked ? customStyle : {}}
            />
        );
    }

    const onNext = () => {
        if (detailsOpen) {
            let index = loaded.findIndex((v) => v.id === details)
            if (index === -1 || index + 1 >= loaded.length) return
            setDetails(loaded[index + 1].id)
            setPage(Math.floor((index + 1) / pageSize) + 1)
        } else {
            setPage((page) => page + 1)
        }
    }

    const onPrevious = () => {
        if (detailsOpen) {
            let index = loaded.findIndex((v) => v.id === details)
            if (index === -1 || index - 1 < 0) return
            setDetails(loaded[index - 1].id)
            setPage(() => Math.floor((index - 1) / pageSize) + 1)
        } else {
            setPage((page) => page > 1 ? page - 1 : page)
        }
    }
    const toggleAttach = () => {
        if (!windowParams.attached) {
            setPage(1)
            setWindowParams({ attached: true, pages: 2 })
        }
        else
            setWindowParams({ ...windowParams, attached: false })
    }

    return (
        <>
            <Flex>
                <FlexItem>
                    <Switch
                        id="attached"
                        label="Attached"
                        isChecked={windowParams.attached}
                        onChange={toggleAttach}
                    />
                </FlexItem>
                <FlexItem breakpointMods={[{ modifier: FlexModifiers["align-right"] }]}>
                    <Pagination
                        itemCount={Math.min(loaded.length, windowParams.pages * pageSize)}
                        perPage={pageSize}
                        page={page}
                        perPageOptions={[{ title: '50', value: 50 }]}
                        onSetPage={(_, p) => setPage(p)}
                        isCompact
                    />
                </FlexItem>
            </Flex>
            <Table
                aria-label="Streams table"
                rows={rows.slice((page - 1) * pageSize, page * pageSize)}
                cells={columns}
                variant={'compact'}
                rowWrapper={customRowWrapper}
            >
                <TableHeader />
                <TableBody
                    rowKey={(s: any) => s.rowData.key}
                    onRowClick={(e, o) => {
                        let anyTarget = e.target as any
                        if (anyTarget.tagName && anyTarget.tagName.toLowerCase() === 'a') {
                            e.stopPropagation()
                        }
                        else {
                            setDetails(o.key)
                            setDetailsOpen(true)
                        }
                    }}
                />
            </Table>
            <GlobalHotKeys allowChanges={true} keyMap={{ NEXT: "right", PREVIOUS: "left", ATTACH: 'a' }} handlers={{
                NEXT: onNext,
                PREVIOUS: onPrevious,
                ATTACH: toggleAttach
            }} />
            {details !== null && (
                <Modal
                    title={'Stream ' + details}
                    header={<Title headingLevel={'h1'} size={'3xl'}><Link to={'/stream/' + details}>{"Stream " + details}</Link></Title>}
                    isOpen={details !== null && detailsOpen}
                    onClose={() => setDetailsOpen(false)}
                    style={{ padding: '1em', backgroundColor: 'var(--pf-global--BackgroundColor--300)' }}
                >
                    <StreamDetails streamId={details} />
                </Modal>
            )
            }
        </>
    )
}



export function Streams() {
    let unparsedParams = useParams<{ port?: string, tag?: string }>();
    let params = {
        port: unparsedParams.port === undefined ? null : nanToNull(parseInt(unparsedParams.port)),
        tag: unparsedParams.tag === undefined ? null : nanToNull(parseInt(unparsedParams.tag))
    }

    return (
        <Stack gutter='md'>
            <StackItem>
                <Split gutter='sm'>
                    <SplitItem>
                        Port: <PortMenu {...params} />
                    </SplitItem>
                    <SplitItem>
                        Tag: <TagMenu {...params} />
                    </SplitItem>
                </Split>
            </StackItem>
            <StackItem isFilled>
                <StreamsTable {...params} />
            </StackItem>
        </Stack>
    )
}