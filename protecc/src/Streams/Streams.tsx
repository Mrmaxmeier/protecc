import React, { useContext, useState, useEffect, useCallback } from 'react';
import { Config, Api, serviceFromPort } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { Stack, StackItem, Split, SplitItem, OptionsMenu, OptionsMenuToggle, OptionsMenuItem, TextInput, Pagination, Flex, FlexItem, Switch, Modal, Title } from '@patternfly/react-core';
import { onEnter, nanToNull, formatBytes } from '../Util';
import { useHistory, useParams, Link } from 'react-router-dom';
import { ColoredDot } from '../Components/ColoredDot';
import { Record, Array, Number, Static } from 'runtypes';
import { StreamOverview, prettyPrintEndpoint } from '../Api/Types';
import { GlobalHotKeys } from 'react-hotkeys';
import { Tags } from '../Components/Tags';
import { StreamDetails } from '../Components/StreamDetails';
import { LightweightTable, LightweightTableHeader } from '../Components/LightweightTable';



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

let StreamsTable: React.FC<Params> = React.memo((params: Params) => {
    let api = useContext(Api)
    let config = useContext(Config)

    let [loaded, setLoaded] = useState<StreamOverview[]>([])
    let [windowParams, setWindowParams] = useState({ pages: 2, attached: true })
    const pageSize = 25
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
        setLoaded([])
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
    }, [params.port, params.tag, api])


    const onNext = useCallback(() => {
        if (detailsOpen) {
            let index = loaded.findIndex((v) => v.id === details)
            if (index === -1 || index + 1 >= loaded.length) return
            setDetails(loaded[index + 1].id)
            setPage(Math.floor((index + 1) / pageSize) + 1)
        } else {
            setPage((page) => page + 1)
        }
    }, [loaded, details, detailsOpen])

    const onPrevious = useCallback(() => {
        if (detailsOpen) {
            let index = loaded.findIndex((v) => v.id === details)
            if (index === -1 || index - 1 < 0) return
            setDetails(loaded[index - 1].id)
            setPage(() => Math.floor((index - 1) / pageSize) + 1)
        } else {
            setPage((page) => page > 1 ? page - 1 : page)
        }
    }, [loaded, details, detailsOpen])

    const toggleAttach = useCallback(() => {
        if (!windowParams.attached) {
            setPage(1)
            setWindowParams({ attached: true, pages: 2 })
        }
        else
            setWindowParams({ ...windowParams, attached: false })
    }, [windowParams])

    let headers = [
        { content: 'Id', width: 10 },
        { content: 'Server', width: 10 },
        { content: 'Client', width: 10 },
        { content: 'Server  data', width: 10 },
        { content: 'Client data', width: 10 },
        { content: 'Service', width: 10 },
        { content: 'Tags', width: 'max' },
    ]

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
                <FlexItem align={{ default: 'alignRight' }}>
                    <Pagination
                        itemCount={Math.min(loaded.length, windowParams.pages * pageSize)}
                        perPage={pageSize}
                        page={page}
                        perPageOptions={[{ title: '' + pageSize, value: pageSize }]}
                        onSetPage={(_, p) => setPage(p)}
                        isCompact
                    />
                </FlexItem>
            </Flex>
            <LightweightTable compact>
                <LightweightTableHeader headers={headers} />
                <tbody>
                    {loaded.slice((page - 1) * pageSize, page * pageSize).map((stream) => {
                        let service = config === null ? null : serviceFromPort(config.services, stream.server[1])
                        return (
                            <tr
                                key={stream.id}
                                className={stream.id === details ? 'highlighted' : ''}
                                onClick={() => {
                                    setDetails(stream.id)
                                    setDetailsOpen(true)
                                }}
                            >
                                <td><Link to={'/stream/' + stream.id}>{stream.id}</Link></td>
                                <td>{prettyPrintEndpoint(stream.server)}</td>
                                <td>{prettyPrintEndpoint(stream.client)}</td>
                                <td>{formatBytes(stream.serverDataLen)}</td>
                                <td>{formatBytes(stream.clientDataLen)}</td>
                                <td>{service && service !== null ? service.name : '-'}</td>
                                <td><Tags tags={stream.tags} /></td>
                            </tr>
                        )
                    })}
                </tbody>
            </LightweightTable>
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
})

export function Streams() {
    let unparsedParams = useParams<{ port?: string, tag?: string }>();
    let params = {
        port: unparsedParams.port === undefined ? null : nanToNull(parseInt(unparsedParams.port)),
        tag: unparsedParams.tag === undefined ? null : nanToNull(parseInt(unparsedParams.tag))
    }

    return (
        <Stack hasGutter>
            <StackItem>
                <Split hasGutter>
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