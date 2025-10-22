// darkSyntax/configs/algol60.js - ALGOL 60 language configuration
// =================================================================
// ALGOL 60 (1960)
//
// Configs
// =======================
// ALIASES: ['algol60', 'algol', 'alg']
// File extensions: .alg, .algol
//
// ALGOL 60 SYNTAX NOTES
// =====================
//
// ALGOL 60 (Algorithmic Language 1960) was one of the most influential
// programming languages ever created. It introduced many concepts that
// became standard in modern languages.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - First language with block structure (begin/end)
// - First with nested function definitions
// - Introduced lexical scoping
// - First language defined with BNF (Backus-Naur Form)
// - Influenced: Pascal, C, C++, Java, and almost every modern language
// - Published 1960, revised 1963
//
// CASE SENSITIVITY
// ----------------
// ALGOL 60 keywords are typically lowercase (begin, end, if, then, else)
// though implementations varied. We'll support both cases.
//
// BLOCK STRUCTURE
// ---------------
// begin
//   statement;
//   statement;
//   statement
// end
//
// Note: Last statement before 'end' has NO semicolon!
// Semicolons are statement SEPARATORS, not terminators.
//
// DECLARATIONS
// ------------
// Types: integer, real, Boolean, string
// Arrays: array A[1:10], B[1:5, 1:5]
// Procedures: procedure name(parameters); statement
// Functions: real procedure name(parameters); statement
//
// CONTROL STRUCTURES
// ------------------
// if condition then statement
// if condition then statement else statement
// for variable := expr step expr until expr do statement
// for variable := expr while condition do statement
// goto label
//
// OPERATORS
// ---------
// Arithmetic: +, -, *, /, ^, ÷ (integer division)
// Relational: =, ≠, <, ≤, >, ≥ (also written as <>, <=, >=)
// Logical: ∧ (AND), ∨ (OR), ¬ (NOT), ⊃ (IMPLIES)
// Assignment: :=
//
// COMMENTS
// --------
// comment This is a comment;
// Can appear anywhere a statement can appear
//
// PUBLICATION SYNTAX
// ------------------
// ALGOL 60 had three syntaxes: reference, publication, and hardware.
// Publication syntax used mathematical symbols: ×, ÷, ≤, ≥, ≠, ¬, ∧, ∨
// Hardware syntax used ASCII: *, /, <=, >=, <>, NOT, AND, OR
// We support both!

darkSyntax.registerLanguage('algol60', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /\bcomment\b[^;]*;/gi,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 90
    },
    
    // PRIORITY 60: Keywords (control flow)
    {
      class: 'keyword',
      pattern: /\b(begin|end|if|then|else|for|do|while|until|step|goto|switch|case|label)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: Declaration keywords
    {
      class: 'keyword',
      pattern: /\b(integer|real|Boolean|string|array|procedure|value|own|switch)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Logical operators (both symbolic and word forms)
    {
      class: 'keyword',
      pattern: /\b(AND|OR|NOT|IMPLY|EQUIV)\b|[∧∨¬⊃≡]/gi,
      priority: 50
    },
    
    // PRIORITY 45: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(abs|sign|sqrt|sin|cos|arctan|ln|exp|entier|outinteger|outreal|outstring|ininteger|inreal)\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Assignment operator
    {
      class: 'keyword',
      pattern: /:=/g,
      priority: 40
    },
    
    // PRIORITY 35: Relational operators (both symbolic and ASCII)
    {
      class: 'keyword',
      pattern: /[=≠<≤>≥]|<>|<=|>=/g,
      priority: 35
    },
    
    // PRIORITY 30: Function/procedure calls (identifier before parenthesis)
    {
      class: 'function',
      pattern: /\b([a-zA-Z][a-zA-Z0-9]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Labels (identifier followed by colon)
    {
      class: 'decorator',
      pattern: /\b([a-zA-Z][a-zA-Z0-9]*)\s*:/g,
      priority: 25,
      captureGroup: 1
    },
    
    // PRIORITY 20: Numbers (including scientific notation)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?/g,
      priority: 20
    },
    
    // PRIORITY 15: Boolean literals
    {
      class: 'boolean',
      pattern: /\b(true|false)\b/gi,
      priority: 15
    }
  ]
});