// darkSyntax/configs/a0.js - A-0 System (1951) language configuration
// ============================================================
// A-0 System (1951)
// The A-0 System — created by Grace Hopper for the UNIVAC I — is widely regarded
// as the first compiler in computing history. It transformed symbolic instructions
// into executable machine code, paving the way for modern programming languages.
//
// Configs
// =======================
// ALIASES: ['a0', 'univac1']
// File extensions: .a0
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Each line represented an instruction: one op-code letter and numeric operands.
//   Example: `A 001 002 003` (Add contents of 001 + 002 → store in 003)
// - Subroutines were invoked by numeric identifiers in parentheses: `(123)`
// - Symbolic variables (A–Z) sometimes replaced numeric addresses.
// - Comments were ad-hoc, but we simulate with `;` or `#` prefixes for readability.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - 1951: Grace Hopper’s team produced A-0 for UNIVAC I — the first system that
//   automatically translated symbolic code into machine instructions.
// - Introduced “automatic programming,” a revolutionary step toward compilers.
// - Precursor to A-2 (1953), FLOW-MATIC (1955), and ultimately COBOL (1959).
//
// INFLUENCED
// ----------
// - A-2 System (1953) — Direct evolution.
// - FLOW-MATIC (1955) — English-like syntax, direct descendant.
// - COBOL (1959) — Inherited the data-processing orientation and readability ideals.
//
// USED FOR
// --------
// - Business and scientific programming on UNIVAC I.
// - Simplifying program entry and maintenance by abstracting numeric opcodes.
//
// KEY FEATURES
// ------------
// - Automatic linking of subroutines.
// - Symbolic rather than numeric entry.
// - Macro-like instruction expansion.
// - Fixed-width numeric operands.
//
// CORE SYNTAX
// -----------
// A 001 002 003   ; Add contents of 001 and 002 → 003
// S 010 011 012   ; Subtract
// M 020 021 022   ; Multiply
// D 030 031 032   ; Divide
// T 100           ; Transfer / jump
// V A B C         ; Symbolic variables
// (123)           ; Subroutine call
//
// QUIRKS
// ------
// - **No modern flow control** — branching only via `T` transfer instructions.
// - **Minimal syntax** — each line a command, no punctuation or keywords.
// - **Fixed field widths** — addresses often exactly three digits.
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "The A-0 System was the first step toward letting the computer handle the
//   translation — freeing humans to think about problems, not bits." — Grace Hopper

// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('a0', {
  showLineNumbers: false,
  rules: [
    // COMMENTS — Early style ; or #
    {
      class: 'comment',
      pattern: /[;#].*$/gm,
      priority: 100
    },

    // SUBROUTINE CALLS — (123)
    {
      class: 'function',
      pattern: /\((\d{1,5})\)/g,
      captureGroup: 1,
      priority: 75
    },

    // OPCODE LETTERS — Command letter at line start or after line number
    {
      class: 'keyword',
      pattern: /^(?:\s*\d*\s*)?([A-Z])(?=\s+\d{1,3})/gm,
      captureGroup: 1,
      priority: 55
    },

    // NUMERIC OPERANDS — Usually 3-digit addresses
    {
      class: 'number',
      pattern: /\b\d{1,4}\b/g,
      priority: 25
    },

    // SYMBOLIC VARIABLES — Single letters not used as opcodes
    {
      class: 'variable',
      pattern: /(?<=\s)[A-Z](?=\s|$)/g,
      priority: 15
    }
  ]
});
