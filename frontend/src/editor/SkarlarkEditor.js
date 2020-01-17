
monaco = require("monaco-editor");

self.MonacoEnvironment = {
    getWorkerUrl: function (moduleId, label) {
        return './editor.worker.js';
    },
};

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

let tags = [];
let services = [];

const builtinVal = (name, type, doc) => {
    return {
        label: name,
        kind: monaco.languages.CompletionItemKind.Constant,
        insertText: name,
        detail: type,
        documentation: doc
    }
}

monaco.languages.registerCompletionItemProvider('starlark', {
    provideCompletionItems: () => {
        var suggestions = [
            builtinVal('client_addr', 'string', 'The ip address of the client'),
            builtinVal('server_addr', 'string', 'The ip address of the server'),
            builtinVal('client_port', 'int', 'The port of the client'),
            builtinVal('server_port', 'int', 'The port of the server'),
            builtinVal('client_len', 'int', 'The length of the data the client sent'),
            builtinVal('server_len', 'int', 'The length of the data the server sent'),
            builtinVal('length', 'int', 'The total length of the data of this stream'),
            builtinVal('tags', 'list int', 'A list of all tags that are attached to this stream')
        ].concat(tags.map((tag) => builtinVal('tag.' + tag.slug, 'int', 'The id of the tag ' + tag.name)
        ).concat(services.map((service) => builtinVal('service.' + service.slug, 'int', 'The port of the service ' + service.port))));
        return { suggestions: suggestions };
    }
});

exports.updateLanguage = (tags2) => {
    return (services2) => {
        return () => {
            tags = tags2;
            services = services2;
        }
    }
}

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


exports.setError = (error) => {
    if (error == undefined || error == "")
        return () => {
            monaco.languages.registerCodeLensProvider('starlark', {
                provideCodeLenses: function (model, token) {
                    return {
                        lenses: [
                        ],
                        dispose: () => { }
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


