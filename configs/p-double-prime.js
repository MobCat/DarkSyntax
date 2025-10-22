// darkSyntax/configs/p-double-prime.js - P′′ configuration
// ===========================================================
// P′′ (P double prime) (1964)
// Minimalist Turing-complete language based on string rewriting
//
// Configs
// =======================
// ALIASES: ['p-double-prime', 'p′′', 'p-prime-prime', 'pp', 'p2']
// File extensions: .pp, .p2
//
// .pp - P′′ source files
// .p2 - Alternative extension
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Invented by Corrado Böhm (1964) - Italian computer scientist and mathematician
// - Theoretical language proving Turing completeness with minimal constructs
// - Based on string manipulation and variable substitution
// - Influenced theoretical computer science and programming language theory
// - Part of the study of primitive recursive functions
// - Demonstrates computational universality with minimal syntax
// - One of the earliest esoteric/theoretical programming languages
// - Böhm also created Böhm trees and contributed to lambda calculus
//
// INFLUENCED
// ----------
// - Brainfuck (1993) - Minimalist esoteric language philosophy
// - FRACTRAN (1996) - John Conway's minimal Turing-complete language
// - Unlambda (1999) - Functional esoteric language
// - Iota and Jot - Minimal combinator calculi
// - Theoretical computer science research on minimal computation models
//
// USED FOR
// --------
// - Theoretical computer science research
// - Proof of Turing completeness with minimal operations
// - Educational demonstrations of computation fundamentals
// - Esoteric programming challenges
// - Study of string rewriting systems
//
// KEY FEATURES
// ------------
// - Only one data type: strings
// - Six basic operations (though minimal subset is Turing-complete)
// - No numbers, no arrays, no functions - just string manipulation
// - Variables are single uppercase letters
// - Programs are sequences of statements separated by spaces
// - Extremely minimal syntax
//
// CORE SYNTAX
// -----------
// The language has 6 operations:
//
// 1. R - Read a character from input, append to program string
// 2. λ - Empty string constant (lambda)
// 3. () - Grouping and string concatenation
// 4. # - End of program marker
// 5. Variable assignment: (XY) means "assign Y to variable X"
// 6. Variable reference: X (where X is uppercase letter)
//
// Basic constructs:
// - λ : Empty string
// - (AB) : Concatenate A and B, or if A is variable, assign B to A
// - A : Variable reference (uppercase letter)
// - # : Program terminator
// - R : Read input
//
// QUIRKS
// ------
// - **Only uppercase letters are variables**:
//   * A-Z can be variables
//   * Lowercase letters and symbols are literals
//   * No declaration needed, just use them
// - **Parentheses do double duty**:
//   * (AB) can mean concatenation OR assignment
//   * If A is a variable name alone, it's assignment
//   * Otherwise it's concatenation
//   * Context-dependent interpretation
// - **No explicit loops or conditionals**:
//   * Control flow through string rewriting
//   * Pattern matching and substitution
//   * Recursive definitions
// - **Whitespace separates statements**:
//   * Programs are space-separated sequences
//   * No newlines needed (but allowed for readability)
// - **Everything is a string**:
//   * Even numbers must be encoded as strings
//   * Church numerals or string encoding required
//   * No arithmetic operations built-in
// - **No error handling**:
//   * Undefined behavior on invalid operations
//   * Variables default to empty string
// - **Program is data**:
//   * Programs can modify themselves
//   * Self-modifying code is the norm
//   * R operation appends to program string
// - **Minimal I/O**:
//   * R reads single character
//   * No explicit print operation in minimal version
//   * Output through side effects or halting state
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Proof of Turing completeness with minimal operations" - Böhm's achievement
// - "Everything is a string" - P′′ philosophy
// - "String rewriting is universal computation" - Theoretical insight
//
// NOTES ON P′′ SYNTAX
// -------------------
// - No comments in original specification (we'll allow # as comment when not terminator)
// - Variables: Single uppercase letters A-Z
// - Lambda: λ (empty string)
// - Grouping: ( and )
// - Terminator: #
// - Input: R
// - Whitespace separates statements but is not required


// P′′ SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================
darkSyntax.registerLanguage('p-double-prime', {
  rules: [
    // Comments (not in original spec, but useful for documentation)
    // Only treat # as comment if followed by space or at start of line
    {
      class: 'comment',
      pattern: /#[^\S\n].*$/gm,
      priority: 100
    },

    // Program terminator (standalone #)
    {
      class: 'decorator',
      pattern: /#(?=\s*$)/gm,
      priority: 95
    },

    // Lambda (empty string constant)
    {
      class: 'keyword',
      pattern: /λ/g,
      priority: 50
    },

    // Read operation
    {
      class: 'keyword',
      pattern: /\bR\b/g,
      priority: 50
    },

    // Variables (single uppercase letters, not inside strings)
    {
      class: 'variable',
      pattern: /\b[A-Z]\b/g,
      priority: 40
    },

    // String literals (anything in quotes if we extend the language)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },

    // Parentheses (grouping/concatenation/assignment)
    {
      class: 'operator',
      pattern: /[()]/g,
      priority: 30
    }
  ]
});