// darkSyntax/configs/pli.js - PL/I Language Configuration
// ============================================================
// PL/I (Programming Language One) - 1964
// Designed by IBM for the System/360 mainframe family.
// IBM's ambitious "one language for all purposes" - not the first language, but an attempt to unify all prior languages.
//
// Configs
// =======================
// ALIASES: ['pli', 'pl1', 'pl-i', 'pl1lang']
// File extensions: .pli, .pl1
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Case-insensitive, often uppercase
// - Comments: /* ... */
// - Semicolons end statements
// - Blocks: DO; ... END; or BEGIN; ... END;
// - Variable declarations use DECLARE or DCL
// - Strong typing with FIXED, FLOAT, CHAR, BIT, COMPLEX
// - Structures and arrays supported
// - Exception handling via ON units
// - I/O via READ, WRITE, GET, PUT
// - Logical operators: & (AND), | (OR), ¬ (NOT) or .AND. .OR.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Introduced by IBM in 1964 for the System/360 series
// - Combined the features of FORTRAN, COBOL, and ALGOL
// - Used in business, scientific, and systems software
// - Implemented on IBM mainframes, Multics, and VAX/VMS
// - One of the first languages to support multitasking and exceptions
//
// INFLUENCED
// ----------
// - C, Ada, Pascal, PL/M, and even modern data languages
//
// USED FOR
// --------
// - Operating systems (Multics)
// - Financial and scientific analysis
// - Large-scale data processing
//
// KEY FEATURES
// ------------
// - Strong typing and type conversions
// - Exception handling via ON units
// - Rich I/O model with FILE and STREAM
// - Flexible arrays and structures
// - String slicing and bit operations
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "PL/I tried to be everything for everyone."
// - "The last great general-purpose mainframe language."
//
// NOTES ON SYNTAX
// ---------------
// - Comments: /* ... */
// - Strings: 'TEXT' or "TEXT"
// - Keywords: DECLARE, PROCEDURE, CALL, IF, THEN, ELSE, DO, END
// - Built-ins: READ, WRITE, GET, PUT, ON, GOTO, RETURN, STOP
// - Types: FIXED, FLOAT, CHAR, BIT, COMPLEX, DECIMAL, STATIC
// - Operators: &, |, ¬, =, ^=, <, >, <=, >=
// - Boolean constants: TRUE, FALSE


// PL/I SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('pli', {
  rules: [
    // --- COMMENTS ---
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },

    // --- STRINGS ---
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"|'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // --- NUMBERS ---
    {
      class: 'number',
      pattern: /\b\d+(\.\d+)?([ED][+-]?\d+)?\b/g,
      priority: 20
    },

    // --- KEYWORDS ---
    {
      class: 'keyword',
      pattern: /\b(PROCEDURE|DECLARE|DCL|DO|END|IF|THEN|ELSE|CALL|RETURN|GOTO|ON|BEGIN|WHEN|SELECT|OTHERWISE|ALLOCATE|FREE|STOP|LEAVE|ITERATE)\b/gi,
      priority: 60
    },

    // --- TYPES ---
    {
      class: 'builtin',
      pattern: /\b(FIXED|FLOAT|CHAR|BIT|COMPLEX|DECIMAL|BINARY|PICTURE|STATIC|BASED|CONTROLLED|ENTRY|FILE|STREAM)\b/gi,
      priority: 45
    },

    // Built-in I/O keywords like PUT, GET, SKIP, LIST, FILE, etc.
    {
      class: 'builtin',
      pattern: /\b(PUT|GET|SKIP|EDIT|LIST|FILE|READ|WRITE|OPEN|CLOSE)\b/g,
      priority: 45
    },
    
    // Built-in conditions like ERROR, ENDFILE, ZERODIVIDE, etc.
    {
      class: 'builtin',
      pattern: /\b(ERROR|ENDFILE|OVERFLOW|UNDERFLOW|ZERODIVIDE|ENDFILE|CONVERSION)\b/g,
      priority: 45
    },
    
    // Context-specific: LIST only as part of PUT/GET
    {
      class: 'builtin',
      pattern: /(?<=\b(PUT|GET)\s+)LIST\b/g,
      priority: 46
    },

    // --- BUILT-IN FUNCTIONS ---
    {
      class: 'builtin',
      pattern: /\b(READ|WRITE|GET|PUT|INDEX|SUBSTR|LENGTH|VERIFY|TRANSLATE|REPEAT|MOD|ABS|MAX|MIN|RANDOM|DATE|TIME|STRING)\b/gi,
      priority: 40
    },

    // --- LOGICAL / RELATIONAL OPERATORS ---
    {
      class: 'operator',
      pattern: /<=|>=|<|>|=|\^=|¬=|&|\||¬|(\.[A-Z]+\.)/g,
      priority: 58
    },

    // --- FUNCTION / PROCEDURE NAMES ---
    {
      class: 'function',
      pattern: /\b(PROCEDURE)\s+([A-Z_][A-Z0-9_]*)/gi,
      captureGroup: 2,
      priority: 35
    },

    // --- CALLS ---
    {
      class: 'function',
      pattern: /\bCALL\s+([A-Z_][A-Z0-9_]*)/gi,
      captureGroup: 1,
      priority: 30
    },

    // --- VARIABLES (ASSIGNMENT TARGETS) ---
    {
      class: 'variable',
      pattern: /\b([A-Z_][A-Z0-9_]*)\b(?=\s*=)/gi,
      captureGroup: 1,
      priority: 15
    },

    // --- BOOLEANS ---
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE|ON|OFF)\b/gi,
      priority: 25
    }
  ]
});
