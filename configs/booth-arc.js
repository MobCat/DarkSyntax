// darkSyntax/configs/booth-arc.js - Booth's ARC Assembly configuration
// =======================================================================
// Booth's ARC Assembly - Contracted Notation (1947)
// The world's first assembly language
//
// Configs
// =======================
// ALIASES: ['booth-arc', 'arc', 'contracted-notation']
// File extensions: .arc (speculative - original was punch cards)
//
// BOOTH'S ARC ASSEMBLY LANGUAGE SYNTAX NOTES
// ==========================================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - World's first assembly language (1947)
// - Created by Kathleen Booth at Birkbeck College, University of London
// - Introduced in "Coding for A.R.C." (September 1947)
// - Used "Contracted Notation" - mathematical formulas instead of mnemonics
// - Designed for ARC2 (Automatic Relay Calculator 2)
// - Influenced by consultation with John von Neumann at Princeton
// - First stored-program computer code with symbolic representation
// - Co-created first magnetic drum storage (1947)
// - Revolutionary: replaced binary machine code with human-readable symbols
//
// INFLUENCED
// ----------
// - EDSAC assembly (1948) - David Wheeler's one-letter mnemonics
// - Assembly language as a concept - established symbolic coding
// - All subsequent assembly languages (1948-present)
// - Compiler theory - "program that translates programs"
// - APE(X)C assembly (1951) - Booth's later system
//
// USED FOR
// --------
// - X-ray crystallography calculations (original purpose)
// - Fourier synthesis computations
// - Early stored-program computing
// - Mathematical computations on relay-based computers
// - Research computing at Birkbeck College
//
// KEY FEATURES
// ------------
// - Mathematical notation: M -> cR (Memory to clear Register)
// - Arrow operators (->): Indicate data flow direction
// - Register modifiers: c (clear), others unknown from historical records
// - Formula-based syntax: Reads like mathematical equations
// - Single address architecture: ARC2 was a relay-based computer
// - No word-based mnemonics: Used symbols and mathematical notation
// - Paper tape input: Programs loaded from punched paper tape
//
// CORE SYNTAX
// -----------
// Contracted Notation used mathematical formulas with arrows:
//
// Data Transfer:
//   M -> cR        ; Clear R and load from memory M
//   M -> R         ; Load from memory M to register R
//   R -> M         ; Store register R to memory M
//
// Arithmetic (speculative, based on era):
//   R + M -> R     ; Add memory M to register R
//   R - M -> R     ; Subtract memory M from register R
//
// Control Flow (speculative):
//   -> n           ; Jump to instruction n
//   R = 0 -> n     ; Conditional jump if R equals 0
//
// Register Modifiers:
//   c  - Clear (documented: "clear and transfer")
//   s  - Shift (speculative, common in relay computers)
//
// QUIRKS
// ------
// - **Mathematical notation**: Uses -> instead of MOV or LD
//   * M -> cR means "move M to R, clearing R first"
//   * Completely different from later assembly languages
// - **No standard mnemonics**: Each formula describes the operation
//   * 10011 (binary) = M -> cR (contracted notation)
//   * Direct mapping from machine code to mathematical expression
// - **Limited documentation**: Only fragments survive from 1947
//   * Full instruction set lost to history
//   * Most code was on paper tape, now destroyed
// - **Relay-based constraints**: Very limited instruction set
//   * ARC2 used 600 relays, 100 vacuum tubes
//   * Slow execution compared to later electronic computers
// - **Single accumulator**: Likely had one main register (R)
//   * Similar to other computers of the era
// - **No input/output syntax**: Punch card/paper tape handled externally
// - **Pioneering notation**: Established the concept of assembly language
//   * Before this: program by toggling switches or rewiring
//   * After this: symbolic programming became standard
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "The first assembly code in which a language is used to represent machine code instructions is found in Kathleen and Andrew Donald Booth's 1947 work, Coding for A.R.C." - Wikipedia
// - "Kathleen Booth is credited with inventing assembly language" - IEEE Computer Society
// - "Before Kathleen Booth, you programmed computers by rewiring them" - Hackaday
// - "She called it Contracted Notation - we call it assembly language" - The Register
//
// NOTES ON BOOTH'S ARC SYNTAX
// ----------------------------
// - Arrow notation (->) indicates data flow direction
// - Register R is the primary accumulator
// - M represents memory address/location
// - Modifiers like 'c' change operation behavior
// - Mathematical formulas replace word mnemonics
// - Original used mathematical typesetting, not ASCII
// - Punch cards used for program storage
//
// SAMPLE CODE
// -----------
// Note: Limited examples due to sparse historical documentation.
// These are based on the documented M -> cR instruction and
// reasonable extrapolations from relay computer architecture.


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================

darkSyntax.registerLanguage('booth-arc', {
  rules: [
    // PRIORITY 100: Comments (using 'comment' keyword, ALGOL-style from era)
    {
      class: 'comment',
      pattern: /\bcomment\b.*$/gmi,
      priority: 100
    },
    
    // PRIORITY 95: Labels (memory addresses or symbolic labels)
    {
      class: 'function',
      pattern: /^[a-zA-Z_][a-zA-Z0-9_]*:/gm,
      priority: 95
    },
    
    // PRIORITY 90: Arrow operators (the defining feature of Contracted Notation)
    {
      class: 'keyword',
      pattern: /->/g,
      priority: 90
    },
    
    // PRIORITY 85: Register with modifiers (cR, sR, etc.)
    {
      class: 'builtin',
      pattern: /\b[cs]R\b/g,
      priority: 85
    },
    
    // PRIORITY 80: Register R (main accumulator)
    {
      class: 'builtin',
      pattern: /\bR\b/g,
      priority: 80
    },
    
    // PRIORITY 75: Memory reference M
    {
      class: 'builtin',
      pattern: /\bM\b/g,
      priority: 75
    },
    
    // PRIORITY 70: Arithmetic operators
    {
      class: 'keyword',
      pattern: /[+\-*\/]/g,
      priority: 70
    },
    
    // PRIORITY 65: Comparison operators (speculative for conditionals)
    {
      class: 'keyword',
      pattern: /[=<>]/g,
      priority: 65
    },
    
    // PRIORITY 60: Memory addresses and numbers
    {
      class: 'number',
      pattern: /\b\d+\b/g,
      priority: 60
    },
    
    // PRIORITY 50: Symbolic addresses (A, B, C, etc. for arrays/variables)
    {
      class: 'variable',
      pattern: /\b[A-Z]\b/g,
      priority: 50
    },
    
    // PRIORITY 40: Identifiers (loop labels, variable names)
    {
      class: 'variable',
      pattern: /\b[a-z_][a-z0-9_]*\b/g,
      priority: 40
    }
  ]
});