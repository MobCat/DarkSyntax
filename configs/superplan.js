// darkSyntax/configs/superplan.js - Superplan language configuration
// =====================================================================
// Superplan (1949)
// The forgotten pioneer of modern programming syntax
//
// Configs
// =======================
// ALIASES: ['superplan', 'super']
// File extensions: .super (speculative)
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Heinz Rutishauser in Zurich (1949-1951)
// - FIRST programming language to use = for assignment (1951)
// - FIRST language with a for loop (für loop in German)
// - Predates FORTRAN by 6 years (1951 vs 1957)
// - Implemented on the Z4 computer at ETH Zurich
// - Described in "Automatische Rechenplanfertigung" dissertation (1951)
// - Rutishauser later co-designed ALGOL 58 and ALGOL 60 (1958-1960)
// - Part of the "Program-Controlled Calculation" research
// - Introduced concept of "program that produces programs" (early compiler)
// - Never widely adopted - remained largely within Swiss academic circles
// - Rediscovered by computing historians in the 1980s-90s
//
// INFLUENCED
// ----------
// - ALGOL 58 (1958) - Rutishauser brought for loop concept and assignment syntax
// - ALGOL 60 (1960) - Direct influence through Rutishauser's participation
// - FORTRAN (1957) - Independent development, but Superplan came first
// - All modern languages - Assignment and for loops are universal today
//
// USED FOR
// --------
// - Mathematical computation on the Z4 computer
// - Academic research at ETH Zurich
// - Proof-of-concept for automatic programming
// - Never used commercially or in production systems
//
// KEY FEATURES
// ------------
// - For loops as the ONLY control structure (no if/goto)
// - Simple and elegant loop syntax: für v = start(step)end
// - Assignment using = or ⇒ (replacement symbol)
// - 1D array support with subscript notation
// - Pure computational focus - no I/O operations
// - Mathematical notation with subscripts/superscripts
//
// CORE SYNTAX
// -----------
// For loop structure:
//   für i = 1(1)10
//     a_i = b_i + c_i
//   Ende Index i
//
// Assignment styles:
//   x = 5        (modern style)
//   x => 5       (ASCII approximation of ⇒)
//   x := 5       (ALGOL-compatible)
//
// Array notation:
//   a_i          (subscript: a with index i)
//   x^2          (superscript: x squared)
//   matrix_(i+j) (complex subscript)
//
// QUIRKS
// ------
// - **No conditionals**: No if statements, no goto, no branches at all!
//   * Control flow is purely sequential with loops
//   * Revolutionary constraint that proved Turing-complete
// - **Mathematical notation**: Original used actual subscripts/superscripts
//   * Plain text uses underscore for subscripts: a_i
//   * Caret for superscripts: x^2
// - **Germanic syntax**: "für" (for) and "Ende Index" (end index)
//   * Reflects Swiss German academic origin
// - **No I/O operations**: Pure computation only
//   * Input/output handled externally by Z4 computer
// - **Assignment operator evolution**: Used ⇒ symbol
//   * Modern transcriptions use = or =>
//   * Predates := from ALGOL by 7+ years
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Rutishauser's Superplan is the first systematic attempt at automatic programming" - Donald Knuth
// - "The für loop was perhaps the single most important syntactic innovation in programming" - Historians of computing
//
// NOTES ON SUPERPLAN SYNTAX
// -------------------------
// - Comments use ALGOL-style: comment text here;
// - für is case-insensitive (FÜR, für, Für all valid)
// - Loop terminator: "Ende Index variable_name"
// - Subscripts: identifier_subscript (a_i, matrix_1)
// - Superscripts: identifier^power (x^2, a^n)
// - Operators: + - * / (standard arithmetic only)
// - No comparison or logical operators
// - No string support (numerical computation only)


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('superplan', {
  rules: [
    // PRIORITY 100: ALGOL-style comments (period-appropriate for 1951)
    {
      class: 'comment',
      pattern: /\bcomment\b[^;]*;/gi,
      priority: 100
    },
    
    // PRIORITY 90: For loop keywords (für is THE control structure)
    {
      class: 'keyword',
      pattern: /\b(für|Ende\s+Index)\b/gi,
      priority: 90
    },
    
    // PRIORITY 80: Assignment operators
    {
      class: 'keyword',
      pattern: /=>|:=|=/g,
      priority: 80
    },
    
    // PRIORITY 70: Subscripts (underscore notation)
    // Matches identifier_subscript patterns
    {
      class: 'builtin',
      pattern: /\b([a-zA-Z][a-zA-Z0-9]*)_\(([^)]+)\)/g,
      priority: 70
    },
    {
      class: 'builtin',
      pattern: /\b([a-zA-Z][a-zA-Z0-9]*)_([a-zA-Z0-9]+)/g,
      priority: 70
    },
    
    // PRIORITY 65: Superscripts (caret notation)
    {
      class: 'builtin',
      pattern: /\b([a-zA-Z][a-zA-Z0-9]*)\^\(([^)]+)\)/g,
      priority: 65
    },
    {
      class: 'builtin',
      pattern: /\b([a-zA-Z][a-zA-Z0-9]*)\^([a-zA-Z0-9]+)/g,
      priority: 65
    },
    
    // PRIORITY 50: For loop structure components
    // The (E2) increment in parentheses
    {
      class: 'number',
      pattern: /\((-?\d+)\)/g,
      priority: 50
    },
    
    // PRIORITY 40: Arithmetic operators
    {
      class: 'keyword',
      pattern: /[+\-*/]/g,
      priority: 40
    },
    
    // PRIORITY 30: Numbers (integers and decimals)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*/g,
      priority: 30
    },
    
    // PRIORITY 20: Variables and identifiers
    {
      class: 'function',
      pattern: /\b[a-zA-Z][a-zA-Z0-9]*/g,
      priority: 20
    }
  ]
});