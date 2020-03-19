import React, { useState, useContext, useCallback, useReducer, Dispatch } from 'react'
import { Stack, StackItem, ToolbarGroup, Toolbar, TextInput, Button, ToolbarItem, Progress, Split, SplitItem, Bullseye } from '@patternfly/react-core'
import { EditorWidget } from './EditorWidget'
import { ArrowRightIcon, TrashAltIcon, LessIcon, ArrowLeftIcon, PlayIcon, PauseIcon } from '@patternfly/react-icons'
import { setIfInt, setIfIntOrEmpty, negate, compare, uniqBy } from '../Util'
import { QueryResult } from '../Api/Types'
import { Api, ProteccApi } from '../Api/ProteccApi'
import { Record, String, Number, Boolean, Array, Static, Null } from 'runtypes'

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
    page: number,
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

    const runState = starlarkScan.error ? 'errored' : (starlarkScan.rangeExhausted ? 'done' : 'running')

    const nonSorted = state.results.filter(r => r.sortKey === undefined)
        .concat(starlarkScan.scanResults.filter(r => r.sortKey === undefined))
    const sorted = state.results.filter(r => r.sortKey !== undefined)
        .concat(starlarkScan.scanResults.filter(r => r.sortKey !== undefined))
    let results = sorted.sort((a, b) => compare(a.sortKey, b.sortKey)).concat(nonSorted)

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


function RenderProgress({ state, onChangePause }: { state: QueryState, onChangePause?: (v: boolean) => void }) {
    const percentage = state.progress === null ? 0 : Math.abs(state.progress.lastScanned - state.progress.from) / Math.abs(state.progress.to - state.progress.from) * 100.0
    const progressText = state.progress === null ? 'Initializing query...' : percentage + '%'
    const variant = state.runState === 'done' || state.runState === 'running' ? 'success' : (state.runState === 'idle' ? 'info' : 'danger')
    const isDone = state.runState !== 'running' && state.runState !== 'idle'

    return <Split>
        <SplitItem isFilled>
            <Bullseye>
                <Progress
                    style={{ width: '100%' }}
                    value={state.runState === 'errored' ? 100 : percentage}
                    valueText={progressText}
                    variant={variant}
                    measureLocation='inside'
                />
            </Bullseye>
        </SplitItem>
        <SplitItem>
            <Button variant='plain' isDisabled={isDone} onClick={() => onChangePause && onChangePause(!state.paused)}>{state.paused ? <PlayIcon /> : <PauseIcon />}</Button>
        </SplitItem>
    </Split>
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
                lastRequest: null,
                paused: false,
                runState: 'running',
                page: 1,
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
                        <Toolbar>
                            <ToolbarGroup>
                                <ToolbarItem>
                                    Range:&nbsp;
                                </ToolbarItem>
                                <ToolbarItem>
                                    <TextInput placeholder='Latest' style={{ width: '5em' }} value={lowerBound === null ? '' : lowerBound} onChange={setIfIntOrEmpty(setLowerBound)} css='' />
                                </ToolbarItem>
                                <ToolbarItem>
                                    <Button placeholder='Lower bound' variant='plain' onClick={negate(setPastToFuture)}>{pastToFuture ? <ArrowLeftIcon /> : <ArrowRightIcon />}</Button>
                                </ToolbarItem>
                                <ToolbarItem>
                                    <TextInput placeholder='Earliest' style={{ width: '5em' }} value={upperBound === null ? '' : upperBound} onChange={setIfIntOrEmpty(setUpperBound)} css='' />
                                </ToolbarItem>
                            </ToolbarGroup>
                            <ToolbarGroup>
                                <ToolbarItem>
                                    <Button variant={discarding ? 'primary' : 'tertiary'} onClick={negate(setDiscarding)}><TrashAltIcon /></Button>
                                </ToolbarItem>
                                <ToolbarItem>
                                    <Button variant={deduplicate ? 'primary' : 'tertiary'} onClick={negate(setDeduplicate)}><LessIcon /></Button>
                                </ToolbarItem>
                            </ToolbarGroup>
                            <ToolbarGroup>
                                <ToolbarItem>
                                    Page Size:&nbsp;
                                </ToolbarItem>
                                <ToolbarItem>
                                    <TextInput style={{ width: '5em' }} value={pageSize} onChange={setIfInt(setPageSize)} css='' />
                                </ToolbarItem>
                            </ToolbarGroup>
                            <ToolbarGroup>
                                <ToolbarItem>
                                    <Button variant='primary' onClick={execute}>Execute</Button>
                                </ToolbarItem>
                            </ToolbarGroup>
                        </Toolbar>
                    </SplitItem>
                    <SplitItem isFilled>
                        {queryState !== null && <RenderProgress state={queryState} onChangePause={(b) => !b && continueFetching()} />}
                    </SplitItem>
                </Split>
            </StackItem>
            <StackItem>
                <pre>{JSON.stringify(queryState, null, 2)}</pre>
            </StackItem>
        </Stack>
    )
}