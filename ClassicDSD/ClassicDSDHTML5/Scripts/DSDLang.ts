// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

var DSDLanguage = {
    displayName: 'DSD',       // start by writing your language name here
    name: 'DSD',
    mimeTypes: ['text/x-dsd'],
    fileExtensions: ['dsd'],

    // used in the editor to insert comments (ctrl+/ or shift+alt+A)
    lineComment: '// ',
    blockCommentStart: '(*',
    blockCommentEnd: '*)',

    // Set defaultToken to invalid to see what you do not tokenize yet
    //defaultToken: 'invalid',

    keywords: [
        "CNil",
        "END",
        "Nil",
        "SNil",
        "agent",
        "and",
        "as",
        "binding",
        "bool",
        "comp",
        "compilation",
        "complement",
        "conc",
        "condensed",
        "constant",
        "copy",
        "data",
        "def",
        "declare",
	    "deterministic",
        "diffusion",
        "directive",
        "div",
	    "dom",
        "false",
        "ff",
        "force",
        "if",
	    "inference",
        "init",
        "inside",
        "int",
        "jit",
        "leak",
        "leaks",
        "lengths",
        "localconcentrations",
        "locations",
        "migrate",
        "module",
        "moments",
        "new",
        "not",
        "nucleotides",
        "or",
        "parameters",
        "plot",
        "plot_settings",
        "polymers",
        "prod",
        "query",
        "rate",
        "rates",
        "rendering",
        "rules",
        "rxn",
        "seed",
        "script",
        "simulation",
        "simulations",
        "simulator",
        "spatial",
        "string",
        "sub",
        "sum",
        "stochastic",
        "sundials",
        "sweeps",
        "synthesis",
        "task",
        "tau",
        "theta",
        "toeholds",
        "true",
        "tt",
        "units",
        "unproductive",
        "vol",
    ],

    /*builtins: [
      'rank', 'rankdir', 'ranksep', 'size', 'ratio',
      'label', 'headlabel', 'taillabel',
      'arrowhead', 'samehead', 'samearrowhead',
      'arrowtail', 'sametail', 'samearrowtail', 'arrowsize',
      'labeldistance', 'labelangle', 'labelfontsize',
      'dir', 'width', 'height', 'angle',
      'fontsize', 'fontcolor', 'same', 'weight', 'color',
      'bgcolor', 'style', 'shape', 'fillcolor', 'nodesep', 'id',
    ],*/

    attributes: [
        'doublecircle', 'circle', 'diamond', 'box', 'point', 'ellipse', 'record',
        'inv', 'invdot', 'dot', 'dashed', 'dotted', 'filled', 'back', 'forward',
    ],

    // we include these common regular expressions
    symbols: /[=><!~?:&|+\-*\/\^%]+/,


    // The main tokenizer for our languages
    tokenizer: {
        root: [
            // identifiers and keywords
            [/[a-zA-Z_\x80-\xFF][\w\x80-\xFF]*/, {
                cases: {
                    '@keywords': 'keyword',
                    //'@builtins': 'predefined',
                    '@attributes': 'constructor',
                    '@default': 'identifier'
                }
            }],

            // whitespace
            { include: '@whitespace' },

            // html identifiers
            //[/<(?!@symbols)/, { token: 'string.html.quote', bracket: '@open', next: 'html' }],

            // delimiters and operators
            [/[{}()\[\]]/, '@brackets'],
            [/@symbols/, {
                cases: {
                    '@keywords': 'keyword',
                    '@default': 'operator'
                }
            }],

            // delimiter
            [/[;|]/, 'delimiter'],

            // numbers 
            [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
            [/0[xX][0-9a-fA-F]+/, 'number.hex'],
            [/\d+/, 'number'],

            // strings
            [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
            [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],
        ],

        comment: [
            [/[^\/*]+/, 'comment'],
            [/\(\*/, 'comment', '@push'],    // nested comment 
            ["\\*\\)", 'comment', '@pop'],
            [/[\/*]/, 'comment']
        ],

        /*html: [
          [/[^<>&]+/, 'string.html'],
          [/&\w+;/, 'string.html.escape'],
          [/&/, 'string.html'],
          [/</, { token: 'string.html.quote', bracket: '@open', next: '@push' }], //nested bracket
          [/>/, { token: 'string.html.quote', bracket: '@close', next: '@pop' }],
        ],*/

        string: [
            [/[^\\"&]+/, 'string'],
            [/\\"/, 'string.escape'],
            [/&\w+;/, 'string.escape'],
            [/[\\&]/, 'string'],
            [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
        ],

        whitespace: [
            [/[ \t\r\n]+/, 'white'],
            [/\(\*/, 'comment', '@comment'],
            [/\/\/.*$/, 'comment'],
            [/#.$/, 'comment'],
        ],
    },
};

export default DSDLanguage