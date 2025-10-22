// darkSyntax/configs/autocode.js - Autocode (1952) language configuration
// ============================================================
// Autocode (1952)
// The "Autocode" family refers to a series of early symbolic programming systems
// developed by Alick Glennie and others at the University of Manchester and elsewhere.
// The first version, for the Manchester Mark 1 in 1952, is often considered the
// **first compiled programming language** — preceding Fortran by several years.
//
// Autocode (1952, Manchester/Mark 1) is indeed a kind of proto-BASIC, line-numbered,
// symbolic assembly-meets-algebra hybrid. It pre-dated BASIC's educational simplicity
// by a decade but shared its philosophy: "Let non-engineers program".
//
// Configs
// =======================
// ALIASES: ['autocode', 'manchester-autocode', 'auto']
// File extensions: .auto, .autocode
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Each instruction line contained a **line number**, a **label or variable**, and an **operation**.
// - Variables were symbolic names referring to memory addresses.
// - Instructions could perform arithmetic, control flow, and input/output.
// - Comments were not formally part of the syntax, but `;` or `#` are used here for clarity.
// - No fixed syntax standard — many regional variants existed (Mark 1, Pegasus, Mercury, etc.)
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - 1952: Alick Glennie creates the original *Manchester Autocode* for the Mark 1 computer.
// - 1954: Tony Brooker develops *Mark 1 Autocode*, improving abstraction and I/O.
// - 1955–58: IBM, Ferranti, and others produce their own "Autocode" dialects.
// - One of the first systems to compile symbolic source code into binary machine code automatically.
// - Pioneered the separation between "symbolic name" and "numeric address".
//
// INFLUENCED
// ----------
// - Speedcoding (1953) and Fortran (1957) adopted Autocode’s notion of symbolic variables.
// - ALGOL and later BASIC inherited its control structure ideas.
// - Established the idea that compilers could make symbolic programming practical.
//
// USED FOR
// --------
// - Scientific computation on the Manchester Mark 1 and Ferranti Mark 1.
// - University and research programming for mathematics, physics, and engineering.
//
// KEY FEATURES
// ------------
// - Symbolic variables mapped automatically to memory locations.
// - Basic arithmetic operations: ADD, SUB, MUL, DIV.
// - Simple conditional jumps: IF ... GOTO.
// - Linear instruction sequencing by line numbers.
// - Input/Output commands for tape or console I/O.
//
// CORE SYNTAX
// -----------
// 10  X = 3
// 20  Y = X * 2
// 30  IF Y > 5 GOTO 50
// 40  PRINT Y
// 50  END
//
// QUIRKS
// ------
// - No formal loops; all repetition via GOTO and IF.
// - Limited numeric precision — typically fixed-point arithmetic.
// - No strings or arrays in early versions.
// - Labels were often optional, and variables were single letters in the earliest Mark 1 forms.
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Autocode was the first language to show that machines could translate symbol into action."


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('autocode', {
  showLineNumbers: false,
  rules: [
    // COMMENTS — Simulated with ; or #
    {
      class: 'comment',
      pattern: /[;#].*$/gm,
      priority: 100
    },

    // PRIORITY 90: Line numbers
    {
      class: 'decorator',
      pattern: /^\s*\d+\s+/gm,
      priority: 90
    },

    // PRIORITY 80: Keywords (control flow & I/O)
    {
      class: 'keyword',
      pattern: /\b(PRINT|JUMP|IF|TO|STOP|GO|END|INPUT|STORE|LOAD)\b/gi,
      priority: 80
    },

    // PRIORITY 70: Operators (including early × ÷ symbols)
    {
      class: 'operator',
      pattern: /[=<>+\-×÷*/]/g,
      priority: 70
    },

    // PRIORITY 60: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 60
    },

    // PRIORITY 50: Variables (single letters)
    {
      class: 'variable',
      pattern: /\b[A-Z]\b/g,
      priority: 50
    },

    // PRIORITY 40: Strings (very rare but seen in later Autocode)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 40
    }
  ]
});
