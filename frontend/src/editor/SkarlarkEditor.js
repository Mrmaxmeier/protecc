
monaco = require("monaco-editor");

self.MonacoEnvironment = {
    getWorkerUrl: function (moduleId, label) {
        return './editor.worker.js';
    },
};

monaco.languages.register({ id: 'starlark' });
monaco.languages.setLanguageConfiguration('starlark', {
    comments: {
        lineComment: '#',
        blockComment: ['\'\'\'', '\'\'\''],
    },
    brackets: [
        ['{', '}'],
        ['[', ']'],
        ['(', ')']
    ],
    autoClosingPairs: [
        { open: '{', close: '}' },
        { open: '[', close: ']' },
        { open: '(', close: ')' },
        { open: '"', close: '"', notIn: ['string'] },
        { open: '\'', close: '\'', notIn: ['string', 'comment'] },
    ],
    surroundingPairs: [
        { open: '{', close: '}' },
        { open: '[', close: ']' },
        { open: '(', close: ')' },
        { open: '"', close: '"' },
        { open: '\'', close: '\'' },
    ],
    onEnterRules: [
        {
            beforeText: new RegExp("^\\s*(?:def|for|if|elif|else).*?:\\s*$"),
            action: { indentAction: monaco.languages.IndentAction.Indent }
        }
    ],
    folding: {
        offSide: true,
        markers: {
            start: new RegExp("^\\s*#region\\b"),
            end: new RegExp("^\\s*#endregion\\b")
        }
    }
})

monaco.languages.setMonarchTokensProvider('starlark', {
    defaultToken: 'invalid',
    tokenPostfix: '.starlark',
    fake_keywords: [
        'as',
        'assert',
        'class',
        'del',
        'except',
        'finally',
        'from',
        'global',
        'import',
        'is',
        'lambda',
        'nonlocal',
        'raise',
        'try',
        'while',
        'with',
        'yield'
    ],
    keywords: [
        'and',
        'break',
        'continue',
        'def',
        'elif',
        'else',
        'exec',
        'for',
        'if',
        'in',
        'not',
        'or',
        'pass',
        'return',
        'load',
        // constants
        'None',
        'True',
        'False',
        //builtins
        'str',
        'type',
        'bool',
        'int',
        'hash',
        'any',
        'all',
        'dict',
        'dir',
        'enumerate',
        'getattr',
        'hasattr',
        'len',
        'list',
        'max',
        'min',
        'print',
        'range',
        'repr',
        'reversed',
        'sorted',
        'tuple',
        'zip',

    ],
    brackets: [
        { open: '{', close: '}', token: 'delimiter.curly' },
        { open: '[', close: ']', token: 'delimiter.bracket' },
        { open: '(', close: ')', token: 'delimiter.parenthesis' }
    ],
    tokenizer: {
        root: [
            { include: '@whitespace' },
            { include: '@numbers' },
            { include: '@strings' },
            [/[,:;]/, 'delimiter'],
            [/[{}\[\]()]/, '@brackets'],
            [/@[a-zA-Z]\w*/, 'tag'],
            [/[a-zA-Z]\w*/, {
                cases: {
                    '@keywords': 'keyword',
                    '@fake_keywords': 'fake_keyword',
                    '@default': 'identifier'
                }
            }]
        ],
        // Deal with white space, including single and multi-line comments
        whitespace: [
            [/\s+/, 'white'],
            [/(^#.*$)/, 'comment'],
            [/'''/, 'string', '@endDocString'],
            [/"""/, 'string', '@endDblDocString']
        ],
        endDocString: [
            [/[^']+/, 'string'],
            [/\\'/, 'string'],
            [/'''/, 'string', '@popall'],
            [/'/, 'string']
        ],
        endDblDocString: [
            [/[^"]+/, 'string'],
            [/\\"/, 'string'],
            [/"""/, 'string', '@popall'],
            [/"/, 'string']
        ],
        // Recognize hex, negatives, decimals, imaginaries, longs, and scientific notation
        numbers: [
            [/-?0x([abcdef]|[ABCDEF]|\d)+[lL]?/, 'number.hex'],
            [/-?(\d*\.)?\d+([eE][+\-]?\d+)?[jJ]?[lL]?/, 'number']
        ],
        // Recognize strings, including those broken across lines with \ (but not without)
        strings: [
            [/'$/, 'string.escape', '@popall'],
            [/'/, 'string.escape', '@stringBody'],
            [/"$/, 'string.escape', '@popall'],
            [/"/, 'string.escape', '@dblStringBody']
        ],
        stringBody: [
            [/[^\\']+$/, 'string', '@popall'],
            [/[^\\']+/, 'string'],
            [/\\./, 'string'],
            [/'/, 'string.escape', '@popall'],
            [/\\$/, 'string']
        ],
        dblStringBody: [
            [/[^\\"]+$/, 'string', '@popall'],
            [/[^\\"]+/, 'string'],
            [/\\./, 'string'],
            [/"/, 'string.escape', '@popall'],
            [/\\$/, 'string']
        ]
    }
});

exports.setError = (error) => {
    if (error == undefined || error == "")
        return () => {
            monaco.languages.registerCodeLensProvider('starlark', {
                provideCodeLenses: function (model, token) {
                    return {
                        lenses: [
                        ]
                    };
                },
                resolveCodeLens: function (model, codeLens, token) {
                    return codeLens;
                }
            });
        }
    return () => {
        monaco.languages.registerCodeLensProvider('starlark', {
            provideCodeLenses: function (model, token) {
                return {
                    lenses: [
                        {
                            range: {
                                startLineNumber: 1,
                                startColumn: 1,
                                endLineNumber: 2,
                                endColumn: 1
                            },
                            command: {
                                title: "ERROR: " + error
                            }
                        }
                    ]
                };
            },
            resolveCodeLens: function (model, codeLens, token) {
                return codeLens;
            }
        });
    }
}

// Define a new theme that contains only rules that match this language
monaco.editor.defineTheme('starlark-dark', {
    base: 'vs-dark',
    inherit: true,
    rules: [
        { token: 'fake-keyword', foreground: 'ff0000', fontStyle: 'italic' }
    ]
});

monaco.languages.registerCompletionItemProvider('starlark', {
    provideCompletionItems: () => {
        var suggestions = [{
            label: 'client_addr',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'client_addr'
        }, {
            label: 'server_addr',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'server_addr'
        }, {
            label: 'client_port',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'client_port'
        }, {
            label: 'server_port',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'server_port'
        }, {
            label: 'client_data',
            kind: monaco.languages.CompletionItemKind.Snippet,
            insertText: 'c_data = client_data()'
        }, {
            label: 'server_data',
            kind: monaco.languages.CompletionItemKind.Snippet,
            insertText: 's_data = server_data()'
        }, {
            label: 'client_len',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'client_len'
        }, {
            label: 'server_len',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'server_len'
        }, {
            label: 'length',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'length'
        }, {
            label: 'tags',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'tags'
        }, {
            label: 'segments',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'segments'
        }, {
            label: 'segments_with_data',
            kind: monaco.languages.CompletionItemKind.Snippet,
            insertText: 'data_segs = segments_with_data()'
        }, {
            label: 'features',
            kind: monaco.languages.CompletionItemKind.Text,
            insertText: 'features'
        }, {
            label: 'tag',
            kind: monaco.languages.CompletionItemKind.Keyword,
            insertText: 'tag.$0',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
        }, {
            label: 'service',
            kind: monaco.languages.CompletionItemKind.Keyword,
            insertText: 'service.$0',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
        }
        ];
        return { suggestions: suggestions };
    }
});

code = 'True'

exports.init = function (element) {
    return function () {
        editor = monaco.editor.create(element, {
            theme: 'starlark-dark',
            value: code,
            language: 'starlark'
        });
        return editor;
        editor.onDidChangeModelContent(function (model) {
            console.log(editor.getValue());
        });
    }
}

exports.content = function (editor) {
    return function () {
        return editor.getValue();
    }
}


