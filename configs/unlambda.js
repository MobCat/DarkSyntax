// darkSyntax/configs/unlambda.js - Unlambda configuration
// =========================================================
// Unlambda (1999)
// Obfuscated functional programming language based on combinatory logic
//
// Configs
// =======================
// ALIASES: ['unlambda', 'ul', 'unl']
// File extensions: .unl, .ul
//
// .unl - Unlambda source files
// .ul - Alternative extension
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by David Madore (1999) - French mathematician and computer scientist
// - Based on combinatory logic (SKI calculus)
// - Intentionally obfuscated and minimalist
// - Turing-complete without lambda abstraction (hence "Unlambda")
// - Influenced by lazy functional programming and lambda calculus
// - Part of the esoteric programming language movement
// - Featured in International Obfuscated Code Contest discussions
// - Demonstrates computation without variables or named functions
//
// INFLUENCED
// ----------
// - Lazy K (2002) - Pure lazy functional language with only S, K, I
// - Iota (2004) - Single combinator language
// - Jot (2004) - Binary encoding of combinatory logic
// - Esoteric functional language designs
// - Theoretical work on combinator calculi
//
// USED FOR
// --------
// - Esoteric programming challenges
// - Obfuscated code competitions
// - Educational demonstrations of combinatory logic
// - Theoretical computer science exploration
// - Code golf puzzles
// - Lambda calculus without lambdas (ironically)
//
// KEY FEATURES
// ------------
// - No variables or named bindings
// - Based on SKI combinator calculus
// - Purely functional (no side effects except I/O)
// - Call-by-name evaluation (lazy)
// - First-class functions (combinators)
// - Minimal I/O primitives
// - Church encoding for data structures
//
// CORE SYNTAX
// -----------
// Unlambda uses single-character commands:
//
// **Combinators (core):**
// - ` (backtick) - Function application (prefix)
// - s - S combinator: s x y z = x z (y z)
// - k - K combinator: k x y = x
// - i - I combinator: i x = x
//
// **I/O functions:**
// - . - Print character: .x prints character x
// - r - Read character from input, returns as function
// - c - Call/cc (call with current continuation)
// - d - Delay/promise (lazy evaluation wrapper)
//
// **Output:**
// - .x - Print ASCII character x literally (e.g., .H prints 'H')
//
// **Special:**
// - v - Void/bottom (discards argument, returns itself)
// - e - Exit program
// - @ - Read and print character
// - | - Reprint character
// - ? - Compare character with input
//
// **Comments:**
// - # - Comment to end of line
// - Whitespace is ignored
//
// QUIRKS
// ------
// - **Backtick is function application**:
//   * `ab means apply a to b
//   * Everything is prefix notation
//   * ``abc means (a b) c = apply (apply a to b) to c
//   * Deeply nested applications common
// - **No lambda abstraction**:
//   * Name "Unlambda" is ironic - lambda calculus without λ
//   * Must use combinators (S, K, I) to build functions
//   * Variable binding simulated through combinators
// - **Print requires character after dot**:
//   * .H prints 'H'
//   * Cannot dynamically construct characters easily
//   * ASCII character must follow . literally
// - **Everything is a function**:
//   * No data types except functions
//   * Numbers, booleans, lists all encoded as functions
//   * Church encoding required for data
// - **Call-by-name evaluation**:
//   * Arguments not evaluated until needed
//   * Can cause infinite loops if not careful
//   * Lazy evaluation semantics
// - **Whitespace is completely ignored**:
//   * ``abc same as ` ` a b c
//   * Newlines, spaces, tabs all ignored
//   * Can format however you want
// - **Comments only with #**:
//   * # to end of line
//   * No block comments
// - **Minimal I/O**:
//   * r reads one char, returns as function
//   * .x prints one literal char
//   * @ reads and prints (echo)
//   * That's it for I/O!
// - **Continuation support**:
//   * c is call/cc
//   * Advanced control flow
//   * Can implement exceptions, backtracking
// - **Programs are expressions**:
//   * Entire program is one big expression
//   * Evaluates to a function
//   * Side effects from .x and @ during evaluation
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Lambda calculus without the lambda" - Unlambda's tagline
// - "Obfuscation through purity" - Functional but unreadable
// - "Every program is a combinator" - Unlambda philosophy
// - "Write-only language" - Notoriously hard to read
//
// NOTES ON UNLAMBDA SYNTAX
// -------------------------
// - Comments: # to end of line
// - Application: `ab (apply a to b)
// - Combinators: s, k, i (lowercase)
// - I/O: .x (print x), r (read), @ (echo)
// - Control: c (call/cc), d (delay), v (void), e (exit)
// - All whitespace ignored
// - Case-sensitive (lowercase for most commands)


// UNLAMBDA SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('unlambda', {
  rules: [
    // Comments (# to end of line)
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },

    // Print character (dot followed by any character)
    {
      class: 'string',
      pattern: /\.[^\s`]/g,
      priority: 85
    },

    // Application operator (backtick)
    {
      class: 'operator',
      pattern: /`/g,
      priority: 60
    },

    // Core combinators (S, K, I)
    {
      class: 'keyword',
      pattern: /[ski]/g,
      priority: 50
    },

    // I/O and control flow
    {
      class: 'builtin',
      pattern: /[r@c]/g,
      priority: 45
    },

    // Special functions
    {
      class: 'decorator',
      pattern: /[dev|?]/g,
      priority: 40
    }
  ]
});