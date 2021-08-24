var crnLanguage = {
    displayName: 'CRN',       // start by writing your language name here
    name: 'crn',
    mimeTypes: ['text/x-crn'],
    fileExtensions: ['crn'],

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
        "conc",
        "copy",
        "directive",
        "ff",
        "float",
        "force",
        "gen",
        "if",
        "init",
        "inside",
        "int",
        "kappa",
        "module",
        "mutation",
        "new",
        "not",
        "or",
        "phos",
        "plot",
        "rate",
        "sample",
        "scale",
        "script",
        "specout",
        "spec",
        "string",
        "sum",
        "tolerance",
        "abstolerance",
        "reltolerance",
        "parameter",
        "spatialplot",
        "spatialic",
        "diffusion",
        "dt",
        "xmax",
        "nx",
        "theta",
        "tt",
        "vol",
        "compilation",
        "simulation",
        "event",
        "parameters",
        "fit",
        "sweep",
        "fit_run",
        "plotwindow",
        "predicate",
        "query",
        "seed"
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

export default crnLanguage;