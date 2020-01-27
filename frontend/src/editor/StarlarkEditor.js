
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

const builtinFun = (name, args, retval, doc) => {
    return {
        label: name,
        kind: monaco.languages.CompletionItemKind.Function,
        insertText: name,
        detail: name + "(" + args + "): " + retval,
        documentation: doc
    }
}

exports.setContent = (editor) => {
    return (content) => {
        return () => {
            editor.setValue(content)
        }
    }
}

monaco.languages.registerCompletionItemProvider('starlark', {
    provideCompletionItems: () => {
        var suggestions = [
            builtinVal('id', 'int', 'The id of the stream'),
            builtinVal('client_addr', 'string', 'The ip address of the client'),
            builtinVal('server_addr', 'string', 'The ip address of the server'),
            builtinVal('client_port', 'int', 'The port of the client'),
            builtinVal('server_port', 'int', 'The port of the server'),
            builtinVal('client_len', 'int', 'The length of the data the client sent'),
            builtinVal('server_len', 'int', 'The length of the data the server sent'),
            builtinVal('length', 'int', 'The total length of the data of this stream'),
            builtinVal('tag_list', 'list int', 'A list of all tags that are attached to this stream'),
            builtinFun('index', '[service, tag]', 'void', 'Put this somewhere where it will get called on every execution of your program. Snacc uses this to determine the index on which the query should run'),
            builtinFun('addTag', 'tag: int', 'void', 'Adds a tag to the stream in the result of this query. This will only get reported back as a result of the query, unless you explicitly register this query as a tagger. This will cause the query to accept the stream.'),
            builtinFun('emit', 'x', 'void', 'Adds a result to this stream, it will get reported back to you in json format. This will cause the query to accept the stream.'),
            builtinFun('accept', '', 'void', 'This will accept the stream.'),

            builtinFun('any', 'x: iterable', 'bool', 'any(x) returns True if any element of the iterable sequence x is true. If the iterable is empty, it returns False'),
            builtinFun('all', 'x: iterable', 'bool', 'all(x) returns False if any element of the iterable sequence x is false. If the iterable is empty, it returns True'),
            builtinFun('bool', 'x', 'bool', 'bool(x) interprets x as a Boolean value---True or False. With no argument, bool() returns False'),
            builtinFun('dict', '...', 'dict', `dict creates a dictionary. It accepts up to one positional argument, which is interpreted as an iterable of two-element sequences (pairs), each specifying a key/value pair in the resulting dictionary.
dict also accepts any number of keyword arguments, each of which specifies a key/value pair in the resulting dictionary; each keyword is treated as a string.
With no arguments, dict() returns a new empty dictionary.
dict(x) where x is a dictionary returns a new copy of x.`),
            builtinFun('dir', 'x', 'list', 'dir(x) returns a list of the names of the attributes (fields and methods) of its operand. The attributes of a value x are the names f such that x.f is a valid expression.'),
            builtinFun('enumerate', 'x: iterable', 'list', `enumerate(x) returns a list of (index, value) pairs, each containing successive values of the iterable sequence xand the index of the value within the sequence.
The optional second parameter, start, specifies an integer value to add to each index.`),
            builtinFun('getattr', 'x, name[, default]', '?', `etattr(x, name[, default]) returns the value of the attribute (field or method) of x named name if it exists. If not, it either returns default (if specified) or raises an error.
getattr(x, "f") is equivalent to x.f.`),
            builtinFun('hasattr', 'x, name', 'bool', 'hasattr(x, name) reports whether x has an attribute (field or method) named name.'),
            builtinFun('hash', 'x', 'int', 'hash(x) returns an integer hash value for a string x such that x == y implies hash(x) == hash(y).'),
            builtinFun('int', 'x[, base]', 'int', `int(x[, base]) interprets its argument as an integer.
If x is an int, the result is x. If x is a bool, the result is 0 for False or 1 for True.
If x is a string, it is interpreted as a sequence of digits in the specified base, decimal by default. If base is zero, x is interpreted like an integer literal, the base being inferred from an optional base marker such as 0b, 0o, or 0x preceding the first digit. These markers may also be used if base is the corresponding base. Irrespective of base, the string may start with an optional + or - sign indicating the sign of the result.`),
            builtinFun('len', 'x: iterable', 'int', 'len(x) returns the number of elements in its argument.'),
            builtinFun('list', 'x: iterable', 'list', 'list(x) returns a new list containing the elements of the iterable sequence x. With no arguments, list() returns a new empty list.'),
            builtinFun('max', 'x: iterable', '?', 'max(x) returns the greatest element in the iterable sequence x. It is an error if any element does not support ordered comparison, or if the sequence is empty.'),
            builtinFun('min', 'x: iterable', '?', 'min(x) returns the least element in the iterable sequence x. It is an error if any element does not support ordered comparison, or if the sequence is empty.'),
            builtinFun('range', '[start,] stop [, step]', 'iterable', 'range returns an immutable sequence of integers defined by the specified interval and stride. Behaves the same way as pythons range.'),
            builtinFun('repr', 'x', 'string', 'repr(x) formats its argument as a string.'),
            builtinFun('reversed', 'x: list', 'list', 'reversed(x) returns a new list containing the elements of the iterable sequence x in reverse order.'),
            builtinFun('sorted', 'x: list[, reversed: bool, key: function]', 'list', 'sorted(x) returns a new list containing the elements of the iterable sequence x, in sorted order. The sort algorithm is stable. The optional named parameter reverse, if true, causes sorted to return results in reverse sorted order. The optional named parameter key specifies a function of one argument to apply to obtain the value\'s sort key. The default behavior is the identity function.'),
            builtinFun('str', 'x', 'string', 'str(x) formats its argument as a string.'),
            builtinFun('tuple', 'x: iterable', 'tuple', 'tuple(x) returns a tuple containing the elements of the iterable x. With no arguments, tuple() returns the empty tuple.'),
            builtinFun('type', 'x', 'string', 'type(x) returns a string describing the type of its operand.'),
            builtinFun('zip', 'a: iterable, b: iterable', 'iterable', 'zip() returns a new list of n-tuples formed from corresponding elements of each of the n iterable sequences provided as arguments to zip. That is, the first tuple contains the first element of each of the sequences, the second element contains the second element of each of the sequences, and so on. The result list is only as long as the shortest of the input sequences.'),
        ].concat(tags.map((tag) => builtinVal('tag.' + tag.slug, 'bool', 'True if the tag ' + tag.name + ' is attached to this stream')))
            .concat(tags.map((tag) => builtinVal('tags.' + tag.slug, 'int', 'The id of the tag ' + tag.name)))
            .concat(services.map((service) => builtinVal('service.' + service.slug, 'bool', 'True if this tag belongs to service ' + service.name)))
            .concat(services.map((service) => builtinVal('services.' + service.slug, 'int', 'The port of the service ' + service.name)));
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

exports.addAction = (editor) => {
    return (action) => {
        return () => {
            editor.addAction(action);
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


exports.init = function (element) {
    return function () {
        editor = monaco.editor.create(element, {
            theme: 'starlark-dark',
            language: 'starlark'
        });
        editor.focus();
        return editor;
    }
}

exports.content = function (editor) {
    return function () {
        return editor.getValue();
    }
}


