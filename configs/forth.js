// darkSyntax/configs/forth.js - Forth language configuration
// ============================================================
// Forth (1970)
// Forth – a stack-based, extensible programming language and interactive environment.
// Created by Charles H. Moore in the late 1960s and publicly released in 1970, Forth
// pioneered direct hardware control with high-level abstraction. It became a mainstay
// in embedded systems, instrumentation, and spaceflight applications.
//
// Configs
// =======================
// ALIASES: ['forth', '4th', 'fth']
// File extensions: .fth, .4th, .fs
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Forth is **stack-oriented**: all operations push or pop values on a stack.
// - Source is **tokenized by whitespace** – every word (token) is an operation or definition.
// - Words are **defined using `:` and `;`**, similar to functions or procedures.
// - Comments begin with `\` (backslash) to end of line, or `( ... )` for inline comments.
// - Numeric literals can be decimal, hexadecimal (`HEX`), or other base modes.
// - Case-insensitive by convention, though modern systems are case-sensitive.
// - Minimal syntax: control flow and data structures are implemented as *words*.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - 1968–1970: Developed by Charles H. Moore while controlling radio telescopes at NRAO.
// - 1970: First formal release, implemented on the IBM 1130.
// - 1973: Used at the Smithsonian Astrophysical Observatory for telescope control.
// - 1978: The Forth Interest Group (FIG) publishes Forth-79, the first standardized version.
// - 1983: Forth-83 released, refining the standard vocabulary.
// - 1994: ANS Forth standard finalized, ensuring cross-platform compatibility.
// - NASA and industry adoption for real-time and embedded systems due to its small footprint.
//
// INFLUENCED
// ----------
// - PostScript (1982) — Stack-based, concatenative syntax directly inspired by Forth.
// - Factor (2003) — Modern concatenative language built on Forth’s principles.
// - Joy, RPL (HP calculators), and Lua VM design draw on its stack semantics.
//
// USED FOR
// --------
// - Embedded systems and firmware (space probes, industrial controllers)
// - Interactive device diagnostics and robotics
// - Early personal computing (Forth-based systems predated BASIC interpreters)
//
// KEY FEATURES
// ------------
// - **Concatenative syntax**: Programs are written as a sequence of stack-manipulating words.
// - **Interactive REPL**: You can define and test words on the fly.
// - **Tiny footprint**: Fits in kilobytes; self-hosting implementations exist.
// - **Metaprogramming**: Forth can modify its own compiler and runtime dictionary.
//
// CORE SYNTAX
// -----------
// : SQUARE ( n -- n² )  DUP * ;      \ Define a new word
// 5 SQUARE .             \ Push 5, call SQUARE, print 25
//
// VARIABLE X              \ Define variable
// 42 X !                  \ Store 42 in X
// X @ .                   \ Fetch from X, print
//
// IF ... ELSE ... THEN     \ Conditional
// BEGIN ... UNTIL          \ Loop
//
// QUIRKS
// ------
// - **Whitespace-driven**: Tokens are separated by spaces only.
// - **Everything is a word**: No reserved keywords; even control flow words can be redefined.
// - **Stack discipline**: Mismanaging the stack leads to subtle bugs, not syntax errors.
// - **Case-insensitive by default**, but some systems preserve case.
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - “Forth is like playing chess with yourself.” — Charles H. Moore
// - “Forth doesn’t protect you from your mistakes — it gives you the power to make them creatively.”
//
// NOTES ON LANGUAGE SYNTAX
// -------------------------
// - Words are separated by spaces (no special delimiters).
// - Comments: `\` (line) or `( ... )` (inline).
// - Definitions start with `:` and end with `;`.
// - Numeric literals are integers or floats, sometimes base-dependent.
// - Built-in “words” include `IF`, `ELSE`, `THEN`, `BEGIN`, `UNTIL`, `DO`, `LOOP`, etc.


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('forth', {
  rules: [
    // COMMENTS — Line comments (\ ...) and inline comments ( ... )
    {
      class: 'comment',
      pattern: /\\.*$/gm, // Line comment
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\(.*?\)/g, // Inline comment
      priority: 100
    },

    {
      class: 'string',
      // Match both ." Hello " and "Hello"
      pattern: /(?:\.\s*")([^"\\\n]*(?:\\.[^"\\\n]*)*)"/g,
      captureGroup: 0,
      priority: 95
    },

    // DEFINITIONS — Words defined between : and ;
    {
      class: 'function',
      pattern: /:\s+([A-Za-z0-9_\-+*\/><=!?]+)/g,
      captureGroup: 1,
      priority: 45
    },

    // END OF DEFINITION — ;
    {
      class: 'keyword',
      pattern: /;/g,
      priority: 44
    },

    // CONTROL FLOW WORDS — Built-in language constructs
    {
      class: 'keyword',
      pattern: /\b(IF|ELSE|THEN|BEGIN|UNTIL|WHILE|REPEAT|DO|LOOP|LEAVE|EXIT|RECURSE)\b/gi,
      priority: 50
    },

    // STACK/ARITHMETIC WORDS — Common built-ins
    {
      class: 'builtin',
      pattern: /\b(DUP|DROP|SWAP|OVER|ROT|2DUP|2DROP|2SWAP|2OVER|EMIT|CR|."|@|!|\+|\-|\*|\/|MOD|ABS|NEGATE)\b/gi,
      priority: 48
    },

    // VARIABLES and CONSTANTS
    {
      class: 'decorator',
      pattern: /\b(VARIABLE|CONSTANT|VALUE|TO|CREATE|ALLOT|CELLS|CHARS)\b/gi,
      priority: 47
    },

    // NUMBERS (integer, float, or base-prefixed)
    {
      class: 'number',
      pattern: /\b\d+(\.\d+)?\b/g,
      priority: 25
    },

    // BOOLEAN and FLAGS
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE)\b/gi,
      priority: 25
    },

    // OPERATORS / SYMBOLIC WORDS
    {
      class: 'operator',
      pattern: /[+\-*\/=<>!?]+/g,
      priority: 55
    },

    // USER-DEFINED WORDS — all-caps tokens (best-effort)
    {
      class: 'variable',
      pattern: /\b[A-Z][A-Z0-9_\-]*\b/g,
      priority: 15
    }
  ]
});
