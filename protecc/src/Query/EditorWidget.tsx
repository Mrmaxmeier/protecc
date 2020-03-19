import React, { useState, useContext, useEffect, useCallback } from 'react'
import ReactDOM from 'react-dom'
import * as monacoEditor from 'monaco-editor/esm/vs/editor/editor.api'
import { Config, Api } from '../Api/ProteccApi'
import Editor from '@monaco-editor/react'
import { Alert, AlertActionCloseButton } from '@patternfly/react-core'
import { SemanticColor } from '../Components/ColoredLabel'

type StandaloneEditor = monacoEditor.editor.IStandaloneCodeEditor


// TODO this is ugly, but doing it right is going to be even uglier :(
function showTextInput(editor: StandaloneEditor, text: string, onSubmit: (value: string | null) => void) {
    const div = document.createElement("div")
    div.innerHTML =
        `
<div class="monaco-quick-open-widget" style="color: rgb(204, 204, 204); background-color: rgb(30, 30, 30); box-shadow: rgb(0, 0, 0) 0px 5px 8px; width: 600px; margin-left: -300px;">
    <div class="quick-open-input" style="width: 588px;">
        <div class="monaco-inputbox idle" style="background-color: rgb(60, 60, 60); color: rgb(204, 204, 204);">
            <div class="wrapper">
                <input class="input empty" autocorrect="off" autocapitalize="off" spellcheck="false" type="text" wrap="off" role="combobox" style="background-color: rgb(60, 60, 60); color: rgb(204, 204, 204);">
            </div>
        </div>
    </div>
    <div class="quick-open-tree" style="height: 22px;">
        <div class="monaco-tree no-focused-item monaco-tree-instance-4 focused" tabindex="0" role="tree">
                <div class="monaco-tree-wrapper" style="overflow: hidden;">
                    <div class="monaco-tree-rows show-twisties" style="top: 0px;">
                        <div class="monaco-tree-row" draggable="false" role="treeitem" style="height: 22px; padding-left: 11px;">
                            <div class="content actions">
                                <div class="sub-content">
                                    <div class="quick-open-entry">
                                        <div class="quick-open-row"><span class=""></span>
                                            <div class="monaco-icon-label">
                                                <div class="monaco-icon-label-container"><span class="monaco-icon-name-container"><a class="label-name"><span class="monaco-highlighted-label"><span>` + text + `</span></span>
                                                    </a>
                                                    </span><span class="monaco-icon-description-container"></span></div>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="shadow"></div>
                <div class="shadow"></div>
                <div class="shadow top-left-corner"></div>
            </div>
        </div>
    </div>
</div>
`
    const widget = {
        getDomNode: function () { return div },
        getId: function () { return 'text-input' },
        getPosition: function () {
            return {
                preference: 2
            }
        }
    };
    editor.addOverlayWidget(widget as any)
    const input = div.querySelector("input")
    if (input !== null) {
        input.onkeydown = (e) => {
            if (e.key === "Enter") {
                editor.removeOverlayWidget(widget as any);
                onSubmit(input.value);
                e.preventDefault()
                e.stopPropagation()
                editor.focus()
            }
        }
        div.onkeydown = (e) => {
            if (e.key === "Escape") {
                editor.removeOverlayWidget(widget as any);
                onSubmit("")
                e.preventDefault()
                e.stopPropagation()
                editor.focus()
            }
        }
        input.focus()
    }
}

const defaultContent =
    `index(service=services.some_service)

def q():
    if id % 1000 == 0:
        emit(client_port)
    return id % 500 == 0

sort_key(data_len)
q()
# press ctrl + space for autocompletion, all relevant builtins are listed there
# press ctrl + space again while autocompletion is open for more information on builtins
# press F1 to see all available commands, these are all the custom ones:
# Execute Query (Shift + Enter)
# Save to local storage (Ctrl + S)
# Load local: <save name>
# Start as tagger
# Create tag (Ctrl + M)`

export function EditorWidget({ onExecute, error, onChange }: { onExecute?: () => void, error?: string, onChange?: (value: string) => void }) {
    const topBarStyle = {
        backgroundColor: 'rgb(27, 28, 29)',
        backgroundImage: 'none',
        borderColor: 'rgb(27, 28, 29)',
        borderRadius: '0.21428571rem 0.21428571rem 0.0rem 0.0rem',
        width: '100%',
        color: 'rgb(255,255,255)',
        display: 'inline-block',
        fontWeight: 700,
        padding: '.5833em .833em',
        verticalAlign: 'baseline',
        margin: '0',
        fontSize: '.85714286rem',
        lineHeight: 1,
        textSizeAdjust: '100%'
    }

    const [editor, setEditor] = useState<StandaloneEditor | null>(null)
    const [loadedFile, setLoadedFile] = useState<{ name: string, content: string } | null>(null)
    const [content, setContent] = useState<string | null>(null)
    const [shownError, setShownError] = useState<[string, string] | null>((error && ["Execution failed", error]) || null)

    const config = useContext(Config)
    const api = useContext(Api)

    useEffect(() => setShownError((error && ["Execution failed", error]) || null), [error])

    useEffect(() => {
        if (editor === null) return
        if (shownError === null || shownError[1] === '') {
            editor.removeOverlayWidget({ getId: () => 'error' } as any)
        }
        else {
            editor.addOverlayWidget({
                getDomNode: () => {
                    let div = document.createElement('div')
                    ReactDOM.render(
                        <Alert
                            title={shownError[0]}
                            isInline
                            variant='danger'
                            action={<AlertActionCloseButton onClose={() => setShownError(null)} />}
                        >
                            {shownError[1]}
                        </Alert>
                        , div)
                    return div
                },
                getId: () => 'error',
                getPosition: () => ({
                    preference: 1
                })
            })
        }
    }, [editor, shownError])

    useEffect(() => {
        if (editor === null) return
        editor.focus()
        const scratch = window.localStorage.getItem('query-scratch')
        let content = scratch !== null ? scratch : defaultContent
        editor.setValue(content)
        setContent(content)
    }, [editor])

    useEffect(() => {
        if (content !== null)
            onChange && onChange(content)
    }, [onChange, content])


    useEffect(() => {
        editor?.addAction({
            id: 'execute',
            label: 'Execute Query',
            keybindings: [1027],
            contextMenuOrder: 0,
            run: () => onExecute && onExecute()
        })
    }, [editor, onExecute])


    const save = useCallback((name: string) => {
        if (editor === null) return
        const content = editor.getValue()
        api.emit({ updateConfiguration: { setScript: [name, content] } })
        setLoadedFile({ name, content })
    }, [editor, api])

    const saveAs = useCallback(() => {
        if (editor === null) return
        showTextInput(editor, 'Enter script name', (name) => {
            if (name === null || name.length === 0) return
            save(name)
        })
    }, [editor, save])

    useEffect(() => {
        editor?.addAction({
            id: 'save',
            label: 'Save',
            keybindings: [2097],
            contextMenuOrder: 0,
            run: () => {
                if (loadedFile === null)
                    saveAs()
                else
                    save(loadedFile.name)
            }
        })
    }, [editor, loadedFile, saveAs, save])

    useEffect(() => {
        editor?.addAction({
            id: 'startTagger',
            label: 'Start as tagger',
            contextMenuOrder: 0,
            run: () => {
                const startTagger = (name: string) => {
                    api.emit({
                        managePipelineNode: {
                            attachStarlark: name,
                        }
                    })
                }
                if (loadedFile === null) {
                    if (editor === null) return
                    showTextInput(editor, 'Enter script name', (name) => {
                        if (name === null || name.length === 0) return
                        save(name)
                        startTagger(name)
                    })
                } else {
                    save(loadedFile.name)
                    startTagger(loadedFile.name)
                }
            }
        })
    }, [editor, api, save, loadedFile])

    useEffect(() => {
        editor?.addAction({
            id: 'createTag',
            label: 'Create Tag',
            keybindings: [2091],
            contextMenuOrder: 0,
            run: () => {
                showTextInput(editor, 'Enter tag description (&lt;slug&gt; &lt;name&gt; [color])', s => {
                    if (s === null || s.length === 0) return
                    const split = s.split(' ')
                    if (split.length === 2)
                        split.push('grey')
                    if (split.length !== 3 || split[0] === '' || split[1] === '' || split[2] === '') {
                        setShownError(["Couldn't create tag", 'This command requires either 2 or 3 arguments, not ' + split.length])
                        return
                    }
                    if (!SemanticColor.guard(split[2])) {
                        setShownError(["Couldn't create tag", split[2] + ' is not a valid color'])
                        return
                    }


                    api.emit({
                        updateConfiguration: {
                            setTag: {
                                slug: split[0],
                                name: split[1],
                                color: split[2],
                                owner: 'webui'
                            }
                        }
                    })
                })
            }
        })
    }, [editor, api])

    useEffect(() => {
        editor?.addAction({
            id: 'saveAs',
            label: 'Save As',
            contextMenuOrder: 0,
            run: () => saveAs()
        })
    }, [editor, api, saveAs])

    useEffect(() => {
        editor?.addAction({
            id: 'delete',
            label: 'Delete on Server',
            contextMenuOrder: 0,
            run: () => {
                if (loadedFile !== null) {
                    api.emit({ updateConfiguration: { removeScript: loadedFile.name } })
                    setLoadedFile(null)
                }
            }
        })
    }, [editor, api, loadedFile])

    useEffect(() => {
        if (config !== null) {
            Object.entries(config.scripts).forEach(([name, content]) => {
                editor?.addAction({
                    id: 'load-' + name,
                    label: 'Load ' + name,
                    contextMenuOrder: 0,
                    run: () => {
                        setLoadedFile({ name, content })
                        editor.setValue(content)
                    }
                })
            })
        }
    }, [config, editor])


    useEffect(() => {
        editor?.onDidChangeModelContent(() => {
            const value = editor.getValue()
            window.localStorage.setItem('query-scratch', value)
            setContent(value)
        })
    }, [editor])

    const filename = loadedFile !== null ? (
        loadedFile.content !== content ? <i>{loadedFile.name}*</i> : loadedFile.name
    ) : 'Local Scratch Buffer'

    return <>
        {editor &&
            <div style={topBarStyle}>
                {filename}
            </div>
        }
        <Editor
            width='100%'
            height='20em'
            language='starlark'
            theme='starlark-dark'
            editorDidMount={(_, editor) => setEditor(editor)}
        />
    </>
}