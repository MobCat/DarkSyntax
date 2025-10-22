// darkSyntax/configs/plm.js - PL/M Language Configuration
// ============================================================
// PL/M (Programming Language for Microcomputers) - 1972
// Created by Gary Kildall at Intel for 8008/8080/8085 systems.
//
// Configs
// =======================
// ALIASES: ['plm', 'plm80', 'plm86']
// File extensions: .plm, .plm80, .plm86
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Derived from PL/I; procedural and structured
// - Case-insensitive, but traditionally all uppercase
// - Comments use /* ... */
// - Semicolons terminate statements
// - Labels are identifiers followed by colon
// - Procedures use PROC/END
// - Hardware I/O via IN, OUT, and PORT keywords
// - Bitwise and logical operators use .AND., .OR., .NOT.
// - Constants can end in H for hex (e.g. 0FFH)
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - 1972: Designed by Gary Kildall at Intel for embedded firmware
// - 1974: Used to write the original CP/M operating system
// - 1975–1983: Intel distributed PL/M compilers for 8080, 8085, and 8086
// - One of the first high-level languages for microprocessors
//
// INFLUENCED
// ----------
// - CP/M BIOS and BDOS - both written in PL/M
// - Early embedded C compilers and system languages
// - Turbo Pascal’s procedural structure
//
// USED FOR
// --------
// - BIOS and monitor ROMs
// - Embedded controllers and I/O firmware
// - CP/M operating system and utilities
// - Educational tools for microprocessor programming
//
// KEY FEATURES
// ------------
// - Combines high-level structure with low-level access
// - Allows direct manipulation of I/O ports and addresses
// - Compact, fast, and easy to compile for limited hardware
// - Optimized for Intel’s 8-bit CPUs
//
// CORE SYNTAX
// -----------
// DECLARE statements for variable definitions
// PROC/END to define procedures
// IF/THEN/ELSE, DO/END for control flow
// CALL for subroutines
// RETURN exits procedures
// Hardware access via IN, OUT, and PORT
//
// QUIRKS
// ------
// - **Case insensitive** source
// - **Hex constants** use suffix H (e.g. 0FFH)
// - **No true strings**: byte arrays instead
// - **No recursion**: stack usage limited
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - “PL/M was a bridge between high-level and assembly programming.” — Gary Kildall
//
// NOTES ON LANGUAGE SYNTAX
// -------------------------
// - Comments: /* ... */
// - String/char literals: 'A'
// - Keywords: IF, THEN, ELSE, DO, END, CALL, RETURN, DECLARE, PROC
// - Built-ins: IN, OUT, PORT, INTERRUPT, ENABLE, DISABLE, HALT
// - Operators: .AND., .OR., .NOT., +, -, *, /, =, <>, <=, >=
// - Boolean constants: TRUE, FALSE
// - Variables: letters and digits, starting with a letter


// PL/M SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('plm', {
  rules: [
    // --- COMMENTS ---
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },

    // --- STRINGS / CHARACTER LITERALS ---
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // --- NUMBERS ---
    // Decimal or hexadecimal with H suffix (e.g. 0FFH)
    {
      class: 'number',
      pattern: /\b\d+[A-Z]*H?\b/g,
      priority: 20
    },

    // --- KEYWORDS ---
    {
      class: 'keyword',
      pattern: /\b(PROC|END|DECLARE|DO|WHILE|IF|THEN|ELSE|CALL|RETURN|GOTO|EXIT|FOR|TO|BY|UNTIL|REPEAT)\b/gi,
      priority: 50
    },

    // --- INTEL BUILT-IN OPERATIONS ---
    {
      class: 'builtin',
      pattern: /\b(IN|OUT|PORT|ENABLE|DISABLE|INTERRUPT|HALT|NOP|INPUT|OUTPUT)\b/gi,
      priority: 45
    },

    // --- TYPES / DECLARATIONS ---
    {
      class: 'builtin',
      pattern: /\b(BYTE|WORD|ADDRESS|DATA|LITERAL|CONSTANT|EXTERNAL|GLOBAL|LABEL)\b/gi,
      priority: 40
    },

    // --- OPERATORS ---
    {
      class: 'operator',
      pattern: /<>|<=|>=|[+\-*/=<>&|^]|(\.[A-Z]+\.)/g,  // .AND., .OR., etc.
      priority: 58
    },

    // --- FUNCTION / PROCEDURE NAMES ---
    {
      class: 'function',
      pattern: /\bPROC\s+([A-Z_][A-Z0-9_]*)/gi,
      captureGroup: 1,
      priority: 35
    },

    // --- CALLS ---
    {
      class: 'function',
      pattern: /\bCALL\s+([A-Z_][A-Z0-9_]*)/gi,
      captureGroup: 1,
      priority: 30
    },

    // --- VARIABLES / LABELS ---
    {
      class: 'variable',
      pattern: /\b([A-Z_][A-Z0-9_]*)\b(?=\s*=)/gi,
      captureGroup: 1,
      priority: 15
    },
    {
      class: 'variable',
      pattern: /^\s*[A-Z_][A-Z0-9_]*:/gm,
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
