import React, { useState, useContext, useCallback, useReducer, Dispatch, useEffect } from 'react'
import { Stack, StackItem, ToolbarGroup, Toolbar, TextInput, Button, ToolbarItem, Progress, Split, SplitItem, Bullseye, Pagination, Modal, Title, Divider, Tooltip } from '@patternfly/react-core'
import { EditorWidget } from './EditorWidget'
import { ArrowRightIcon, TrashAltIcon, LessIcon, ArrowLeftIcon, PlayIcon, PauseIcon } from '@patternfly/react-icons'
import { setIfInt, setIfIntOrEmpty, negate, compare, uniqBy, formatPercent, beautify, onEnter } from '../Util'
import { QueryResult, prettyPrintEndpoint } from '../Api/Types'
import { Api, ProteccApi, useStream } from '../Api/ProteccApi'
import { Record, String, Number, Boolean, Array, Static, Null } from 'runtypes'
import { LightweightTable, LightweightTableHeader } from '../Components/LightweightTable'
import { Link } from 'react-router-dom'
import { Tags } from '../Components/Tags'
import { GlobalHotKeys } from 'react-hotkeys'

const QueryResponse = Record({
    starlarkScan: Record({
        error: String.Or(Null),
        scanProgress: Number,
        rangeExhausted: Boolean,
        boundLow: Number,
        boundHigh: Number,
        scanResults: Array(QueryResult)
    })
})
type QueryResponse = Static<typeof QueryResponse>

type QueryState = {
    progress: {
        lastScanned: number,
        from: number,
        to: number
    } | null,
    results: QueryResult[],
    program: string,
    pastToFuture: boolean,
    pageSize: number,
    paused: boolean,
    runState: 'running' | 'idle' | 'done' | 'errored',
    discarding: boolean,
    deduplicate: boolean,
    maxPages: number
}

type QueryStateAction = {
    type: 'handleResponse'
    arg: QueryResponse
    setError: (v: string) => void
    api: ProteccApi
    dispatch: Dispatch<QueryStateAction>
} | {
    type: 'continueFetching'
    arg: ProteccApi
    setError: (v: string) => void
    dispatch: Dispatch<QueryStateAction>
} | {
    type: 'set'
    arg: (v: QueryState | null) => QueryState | null
}

const maxPageFetch = 10



function continueFetching(state: QueryState, api: ProteccApi, dispatch: Dispatch<QueryStateAction>, setError: (v: string) => void): QueryState {
    if (state.progress === null || state.runState === 'errored' || state.runState === 'done')
        return state
    const lower = state.pastToFuture ? state.progress.lastScanned + 1 : state.progress.to
    const upper = state.pastToFuture ? state.progress.to : state.progress.lastScanned - 1
    const maxPages = state.results.length >= state.pageSize * state.maxPages ? state.maxPages + maxPageFetch : state.maxPages
    const windowSize = Math.min(state.pageSize, maxPages * state.pageSize - state.results.length)

    api.listen({
        starlarkScan: {
            code: state.program,
            boundLow: lower,
            boundHigh: upper,
            reverse: state.pastToFuture,
            windowSize: windowSize
        }
    }, msg => dispatch({ type: 'handleResponse', setError: setError, arg: QueryResponse.check(msg), api, dispatch }))

    return {
        ...state,
        runState: 'running',
        paused: false,
        maxPages: maxPages
    }
}

function handleResponse(state: QueryState, { starlarkScan }: QueryResponse, setError: (v: string) => void, api: ProteccApi, dispatch: Dispatch<QueryStateAction>): QueryState {
    if (state === null || state.runState !== 'running')
        return state

    const progress = state.progress === null ? {
        lastScanned: starlarkScan.scanProgress,
        from: state.pastToFuture ? starlarkScan.boundLow : starlarkScan.boundHigh,
        to: state.pastToFuture ? starlarkScan.boundHigh : starlarkScan.boundLow,
    } : { ...state.progress, lastScanned: starlarkScan.scanProgress }

    const runState = starlarkScan.error ? 'errored' : (starlarkScan.rangeExhausted || progress.lastScanned === progress.to ? 'done' : 'running')

    const nonSorted = state.results.filter(r => r.sortKey === undefined)
        .concat(starlarkScan.scanResults.filter(r => r.sortKey === undefined))
    const sorted = state.results.filter(r => r.sortKey !== undefined)
        .concat(starlarkScan.scanResults.filter(r => r.sortKey !== undefined))
    let results = sorted.sort((a, b) => compare(b.sortKey, a.sortKey)).concat(nonSorted)

    if (state.deduplicate)
        results = uniqBy(results, v => v.attached)
    if (state.discarding)
        results = results.slice(0, state.pageSize * (maxPageFetch - 1))

    state = { ...state, progress, runState, results }

    if (starlarkScan.error)
        setError(starlarkScan.error)

    const pause = state.paused || state.results.length >= state.pageSize * state.maxPages

    if (state.runState === 'running') {
        if (pause)
            state = { ...state, paused: true, runState: 'idle' }
        else
            state = continueFetching(state, api, dispatch, setError)
    }

    return state
}

function queryStateReducer(state: QueryState | null, action: QueryStateAction): QueryState | null {
    switch (action.type) {
        case 'continueFetching':
            return state === null ? null : continueFetching(state, action.arg, action.dispatch, action.setError)
        case 'set':
            return action.arg(state)
        case 'handleResponse':
            return state === null ? null : handleResponse(state, action.arg, action.setError, action.api, action.dispatch)
    }
}


function QueryProgress({ state, onChangePause }: { state: QueryState, onChangePause?: (v: boolean) => void }) {
    const max = state.progress === null ? 100 : Math.abs(state.progress.to - state.progress.from)
    const value = state.progress === null ? 0 : Math.min(Math.abs(state.progress.lastScanned - state.progress.from), max)
    const percentage = value / max * 100.0
    const progressText = state.progress === null ? 'Initializing query...' : (formatPercent(percentage) + '%')
    const variant = state.runState === 'done' ? 'success' : (state.runState === 'errored' ? 'danger' : 'info')
    const isDone = state.runState !== 'running' && state.runState !== 'idle'

    return <Split>
        <SplitItem isFilled>
            <Bullseye>
                <Progress
                    style={{ width: '100%' }}
                    value={state.runState === 'errored' ? max : value}
                    valueText={progressText}
                    label={progressText}
                    variant={variant}
                    min={0}
                    max={max}
                />
            </Bullseye>
        </SplitItem>
        <SplitItem>
            <Button variant='plain' isDisabled={isDone} onClick={() => onChangePause && onChangePause(!state.paused)}>{state.paused || isDone ? <PlayIcon /> : <PauseIcon />}</Button>
        </SplitItem>
        <GlobalHotKeys allowChanges={true} keyMap={{ PAUSE: "p" }} handlers={{
            PAUSE: () => onChangePause && onChangePause(!state.paused)
        }} />
    </Split>
}

function ResultDetails({ result }: { result: QueryResult }) {
    let updatedStream = useStream(result.stream.id)
    const stream = updatedStream === null ? result.stream : updatedStream

    return <Bullseye>
        <Stack style={{ width: '90%' }} gutter='lg'>
            <StackItem>
                <LightweightTable compact>
                    <LightweightTableHeader headers={[
                        { content: 'Id', width: 10 },
                        { content: 'Server', width: 10 },
                        { content: 'Client', width: 10 },
                        { content: 'Server  data', width: 10 },
                        { content: 'Client data', width: 10 },
                        { content: 'Sort key', width: 10 },
                        { content: 'Tags', width: 'max' },
                    ]} />
                    <tbody>
                        <tr>
                            <td>{stream.id}</td>
                            <td>{prettyPrintEndpoint(stream.server)}</td>
                            <td>{prettyPrintEndpoint(stream.client)}</td>
                            <td>{stream.serverDataLen}</td>
                            <td>{stream.clientDataLen}</td>
                            <td>{result.sortKey}</td>
                            <td><Tags tags={stream.tags} streamId={stream.id} /></td>
                        </tr>
                    </tbody>
                </LightweightTable>
            </StackItem>
            <Divider />
            <StackItem>
                <Title size={'lg'}>Added Tags:</Title>
                <Tags tags={result.addedTags} />
            </StackItem>
            <Divider />
            <StackItem>
                <Title size={'lg'}>Emitted data:</Title>
                <div>
                    <pre className='scroll'>{result.attached === null ? '' : beautify(result.attached)}</pre>
                </div>
            </StackItem>
        </Stack>
    </Bullseye>
}

function QueryTable({ state }: { state: QueryState }) {
    const [page, setPage] = useState(1)
    const [details, setDetails] = useState<QueryResult | null>(null)
    const [detailsOpen, setDetailsOpen] = useState(false)
    const [pageSize, setPageSize] = useState(state.pageSize)

    useEffect(() => setPageSize(state.pageSize), [state.pageSize])

    let pageSizes = [pageSize, 10, 20, 50, 100]
        .filter((n, i) => i === 0 || n !== pageSize)
        .map(n => ({ title: n.toString(), value: n }))

    const onNext = useCallback(() => {
        if (detailsOpen) {
            let index = state.results.findIndex((v) => v.stream.id === details?.stream.id)
            if (index === -1 || index + 1 >= state.results.length) return
            setDetails(state.results[index + 1])
            setPage(Math.floor((index + 1) / pageSize) + 1)
        } else {
            setPage((page) => page + 1 <= Math.ceil(state.results.length / pageSize) ? page + 1 : page)
        }
    }, [details, detailsOpen, pageSize, state.results])

    const onPrevious = useCallback(() => {
        if (detailsOpen) {
            let index = state.results.findIndex((v) => v.stream.id === details?.stream.id)
            if (index === -1 || index - 1 < 0) return
            setDetails(state.results[index - 1])
            setPage(() => Math.floor((index - 1) / pageSize) + 1)
        } else {
            setPage((page) => page > 1 ? page - 1 : page)
        }
    }, [details, detailsOpen, pageSize, state.results])

    return <>
        <Pagination
            style={{ '--pf-c-pagination__nav-page-select--c-form-control--Width': '5em' } as React.CSSProperties}
            itemCount={state.results.length}
            perPage={pageSize}
            page={page}
            perPageOptions={pageSizes}
            onPerPageSelect={(_, v) => setPageSize(v)}
            onSetPage={(_, p) => setPage(p)}
        />
        <LightweightTable compact>
            <LightweightTableHeader headers={[
                { content: 'Id', width: 10 },
                { content: 'Server', width: 10 },
                { content: 'Client', width: 10 },
                { content: 'Sort key', width: 10 },
                { content: 'Tags', width: 20 },
                { content: 'Data', width: 'max' }
            ]} />
            <tbody>{
                state.results.slice((page - 1) * pageSize, page * pageSize).map(result =>
                    <tr
                        key={result.stream.id}
                        onClick={() => { setDetails(result); setDetailsOpen(true) }}
                        className={result.stream.id === details?.stream.id ? 'highlighted' : ''}
                    >
                        <td><Link to={'/stream/' + result.stream.id}>{result.stream.id}</Link></td>
                        <td>{prettyPrintEndpoint(result.stream.server)}</td>
                        <td>{prettyPrintEndpoint(result.stream.client)}</td>
                        <td>{result.sortKey === null ? '-' : result.sortKey}</td>
                        <td><Tags tags={result.addedTags === null ? [] : result.addedTags}></Tags></td>
                        <td><pre className='ellipsis'>{result.attached === null ? '' : JSON.stringify(result.attached)}</pre></td>
                    </tr>
                )
            }</tbody>
        </LightweightTable>
        <GlobalHotKeys allowChanges={true} keyMap={{ NEXT: "right", PREVIOUS: "left" }} handlers={{
            NEXT: onNext,
            PREVIOUS: onPrevious,
        }} />
        {details !== null && (
            <Modal
                title={'Stream ' + details.stream.id}
                header={<Title headingLevel={'h1'} size={'3xl'}><Link to={'/stream/' + details.stream.id}>{"Stream " + details.stream.id}</Link></Title>}
                isOpen={detailsOpen}
                onClose={() => setDetailsOpen(false)}
                style={{ padding: '1em', backgroundColor: 'var(--pf-global--BackgroundColor--300)' }}
            >
                <ResultDetails result={details} />
            </Modal>
        )
        }
    </>
}

export function Query() {
    const api = useContext(Api)

    const [discarding, setDiscarding] = useState(false)
    const [deduplicate, setDeduplicate] = useState(false)
    const [pageSize, setPageSize] = useState(20)
    const [lowerBound, setLowerBound] = useState<number | null>(null)
    const [upperBound, setUpperBound] = useState<number | null>(null)
    const [pastToFuture, setPastToFuture] = useState(false)
    const [error, setError] = useState<string | null>(null)
    const [queryState, dispatch] = useReducer(queryStateReducer, null)
    const [editorContents, setEditorContents] = useState<string | null>(null)

    const continueFetching = useCallback(() => dispatch({ type: 'continueFetching', arg: api, setError, dispatch }), [api])

    const execute = useCallback(() => {
        if (editorContents === null) return
        api.listen(
            {
                starlarkScan: {
                    code: editorContents,
                    boundLow: lowerBound,
                    boundHigh: upperBound,
                    reverse: pastToFuture,
                    windowSize: pageSize
                }
            },
            msg => dispatch({ type: 'handleResponse', setError: setError, arg: QueryResponse.check(msg), api, dispatch }),
        )
        dispatch({
            type: 'set',
            arg: () => ({
                progress: null,
                results: [],
                program: editorContents,
                pastToFuture,
                pageSize,
                paused: false,
                runState: 'running',
                discarding,
                deduplicate,
                maxPages: maxPageFetch
            })
        })
    }, [api, deduplicate, discarding, editorContents, lowerBound, pageSize, pastToFuture, upperBound])

    return (
        <Stack gutter='md'>
            <StackItem>
                <EditorWidget error={error !== null ? error : undefined} onExecute={execute} onChange={setEditorContents} />
            </StackItem>
            <StackItem>
                <Split gutter='lg'>
                    <SplitItem>
                        <Bullseye>
                            <Toolbar>
                                <ToolbarGroup>
                                    <ToolbarItem>
                                        Range:&nbsp;
                                </ToolbarItem>
                                    <ToolbarItem>
                                        <TextInput onKeyDown={onEnter(execute)} placeholder='Latest' style={{ width: '5em' }} value={lowerBound === null ? '' : lowerBound} onChange={setIfIntOrEmpty(setLowerBound)} css='' />
                                    </ToolbarItem>
                                    <ToolbarItem>
                                        <Button placeholder='Lower bound' variant='plain' onClick={negate(setPastToFuture)}>{pastToFuture ? <ArrowLeftIcon /> : <ArrowRightIcon />}</Button>
                                    </ToolbarItem>
                                    <ToolbarItem>
                                        <TextInput onKeyDown={onEnter(execute)} placeholder='Earliest' style={{ width: '5em' }} value={upperBound === null ? '' : upperBound} onChange={setIfIntOrEmpty(setUpperBound)} css='' />
                                    </ToolbarItem>
                                </ToolbarGroup>
                                <ToolbarGroup>
                                    <ToolbarItem>
                                        <Tooltip content={'Only keep 9 pages and discard the rest. This will cause the query to go on until the end of the data, use this in combination with sort_key'}>
                                            <Button variant={discarding ? 'primary' : 'tertiary'} onClick={negate(setDiscarding)}><TrashAltIcon /></Button>
                                        </Tooltip>
                                    </ToolbarItem>
                                    <ToolbarItem>
                                        <Tooltip content={'Deduplicate results by emitted data'}>
                                            <Button variant={deduplicate ? 'primary' : 'tertiary'} onClick={negate(setDeduplicate)}><LessIcon /></Button>
                                        </Tooltip>
                                    </ToolbarItem>
                                </ToolbarGroup>
                                <ToolbarGroup>
                                    <ToolbarItem>
                                        Page Size:&nbsp;
                                </ToolbarItem>
                                    <ToolbarItem>
                                        <TextInput onKeyDown={onEnter(execute)} style={{ width: '5em' }} value={pageSize} onChange={setIfInt(setPageSize)} css='' />
                                    </ToolbarItem>
                                </ToolbarGroup>
                                <ToolbarGroup>
                                    <ToolbarItem>
                                        <Button variant='primary' onClick={execute}>Execute</Button>
                                    </ToolbarItem>
                                </ToolbarGroup>
                            </Toolbar>
                        </Bullseye>
                    </SplitItem>
                    <SplitItem isFilled>
                        {queryState !== null &&
                            <QueryProgress
                                state={queryState}
                                onChangePause={paused => {
                                    dispatch({ type: 'set', arg: s => s === null ? s : { ...s, paused } })
                                    if (!paused)
                                        continueFetching()
                                }}
                            />
                        }
                    </SplitItem>
                </Split>
            </StackItem>
            {queryState &&
                <StackItem>
                    <QueryTable state={queryState} />
                </StackItem>
            }
        </Stack>
    )
}