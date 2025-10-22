// darkSyntax/configs/scheme.js - Scheme language configuration
// ==============================================================
// Scheme (1975)
//
// Configs
// =======================
// ALIASES: ['scheme', 'scm', 'ss', 'rkt', 'racket']
// File extensions: .scm, .ss, .rkt
//
// SCHEME SYNTAX NOTES
// ===================
//
// Scheme is a minimalist dialect of Lisp created by Guy Steele and Gerald
// Sussman at MIT in 1975. It proved that a powerful language doesn't need
// to be complex - the original implementation was about a page of code.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Guy L. Steele Jr. and Gerald Jay Sussman at MIT (1975)
// - Originally named "Schemer", shortened to "Scheme" for 6-char filename limit
// - First Lisp dialect with lexical scoping (not dynamic)
// - First to require tail-call optimization
// - Influenced: Common Lisp, JavaScript, Ruby, Clojure, Racket, R language
// - Documented in famous "Lambda Papers" (1975-1980)
// - Used in SICP (Structure and Interpretation of Computer Programs)
//
// THE ACCIDENTAL DESIGN
// ---------------------
// Sussman and Steele, 1998: "We were actually trying to build something
// complicated and discovered, serendipitously, that we had accidentally
// designed something that met all our goals but was much simpler than we
// had intended."
//
// They were trying to understand Carl Hewitt's Actor model, implemented it
// in a "tiny Lisp", and discovered actors and closures were the same thing!
//
// INFLUENCED
// ----------
// - Common Lisp (1984) - Steele helped design it
// - JavaScript (1995) - Brendan Eich: "I was trying to make Scheme for the browser"
// - Ruby (1995) - Blocks inspired by Scheme
// - Clojure (2007) - Modern Lisp with Scheme influences
// - Racket (1994) - Scheme descendant for education/research
// - R language (1993) - Statistical language based on Scheme
//
// USED FOR
// --------
// - Teaching programming (SICP at MIT)
// - Programming language research
// - Theorem proving and symbolic computation
// - Emacs Lisp (influenced by Scheme)
// - Scripting and extension languages
//
// MINIMAL CORE
// ------------
// Scheme is incredibly minimal:
// - Everything is an expression (no statements)
// - Only 5-6 special forms needed: lambda, if, define, set!, quote
// - Rest is library functions built on top
// - Parentheses everywhere (Lisp syntax)
//
// KEY FEATURES
// ------------
// 1. Lexical scoping (not dynamic like early Lisps)
// 2. First-class functions (lambda)
// 3. Tail-call optimization (required!)
// 4. First-class continuations (call/cc)
// 5. Hygienic macros (later versions)
// 6. Exact and inexact numbers
//
// SYNTAX (Polish/Prefix Notation)
// --------------------------------
// (function arg1 arg2 ...)
// (+ 1 2)          ; 3
// (* (+ 2 3) 4)    ; 20, not 2 + 3 * 4
// Everything is in prefix form!
//
// SPECIAL FORMS
// -------------
// (define name value)           - Define variable
// (define (name args) body)     - Define function
// (lambda (args) body)          - Anonymous function
// (if test then else)           - Conditional
// (cond (test expr) ...)        - Multi-way conditional
// (let ((var val) ...) body)    - Local binding
// (set! var value)              - Assignment (mutation)
// (quote expr) or 'expr         - Literal data
//
// DATA TYPES
// ----------
// Numbers: 42, 3.14, 1/3 (exact rational), 2+3i (complex)
// Booleans: #t #f (true/false)
// Symbols: 'foo, 'x, '+
// Strings: "hello"
// Characters: #\a, #\space
// Lists: '(1 2 3), (list 1 2 3)
// Pairs: (cons 1 2) -> (1 . 2)
// Vectors: #(1 2 3)
//
// COMMENTS
// --------
// ; Single-line comment
// #| Multi-line
//    comment |#

darkSyntax.registerLanguage('scheme', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /;.*$/gm,
      priority: 100
    },
    {
      class: 'comment',
      pattern: /#\|[\s\S]*?\|#/g,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 85: Characters
    {
      class: 'string',
      pattern: /#\\[a-zA-Z]+|#\\./g,
      priority: 85
    },
    
    // PRIORITY 80: Special forms (core language)
    {
      class: 'keyword',
      pattern: /\b(define|lambda|if|cond|case|let|let\*|letrec|begin|do|delay|set!|quote|quasiquote|unquote|unquote-splicing|define-syntax|syntax-rules|and|or|not)\b/g,
      priority: 80
    },
    
    // PRIORITY 75: Built-in predicates
    {
      class: 'builtin',
      pattern: /\b(null\?|pair\?|list\?|symbol\?|number\?|string\?|char\?|vector\?|procedure\?|boolean\?|eq\?|eqv\?|equal\?|zero\?|positive\?|negative\?|odd\?|even\?)\b/g,
      priority: 75
    },
    
    // PRIORITY 70: List operations (with word boundaries to not match prefixes)
    {
      class: 'builtin',
      pattern: /\b(cons|car|cdr|caar|cadr|cdar|cddr|list|length|append|reverse|map|for-each|filter|fold-left|fold-right|member|assoc|assq|assv)(?=\s|\)|$)/g,
      priority: 70
    },
    
    // PRIORITY 65: Arithmetic operators (higher than general symbols!)
    {
      class: 'builtin',
      pattern: /(?<=\(|\s)(\+|\-|\*|\/)(?=\s|\))/g,
      priority: 65
    },
    
    // PRIORITY 65: Math functions
    {
      class: 'builtin',
      pattern: /\b(max|min|abs|quotient|remainder|modulo|gcd|lcm|floor|ceiling|truncate|round|sqrt|expt|exp|log|sin|cos|tan|asin|acos|atan|exact->inexact|inexact->exact)\b/g,
      priority: 65
    },
    {
      class: 'keyword',
      pattern: /[=<>]=?|<=|>=/g,
      priority: 65
    },
    
    // PRIORITY 60: I/O operations
    {
      class: 'builtin',
      pattern: /\b(display|write|newline|read|read-char|peek-char|eof-object\?|open-input-file|open-output-file|close-input-port|close-output-port|call-with-input-file|call-with-output-file|with-input-from-file|with-output-to-file)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Advanced features
    {
      class: 'builtin',
      pattern: /\b(apply|call\/cc|call-with-current-continuation|values|call-with-values|dynamic-wind|eval|load|force)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: String and vector operations
    {
      class: 'builtin',
      pattern: /\b(string|string\?|make-string|string-length|string-ref|string-set!|string-append|substring|string->list|list->string|string->number|number->string|string->symbol|symbol->string|vector|vector\?|make-vector|vector-length|vector-ref|vector-set!|vector->list|list->vector)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Quote shorthand
    {
      class: 'keyword',
      pattern: /['`]/g,
      priority: 45
    },
    
    // PRIORITY 40: Booleans
    {
      class: 'boolean',
      pattern: /#[tf]\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Numbers (including rationals, complex, exact/inexact)
    {
      class: 'number',
      pattern: /#[bB][01]+|#[oO][0-7]+|#[dD]\d+|#[xX][0-9a-fA-F]+|#[eEiI][\d.]+|\b\d+\/\d+|\b\d+\.?\d*([eE][+-]?\d+)?|\b\d+[+-]\d+i\b/g,
      priority: 35
    },
    
    // PRIORITY 30: Symbols (identifiers)
    {
      class: 'function',
      pattern: /\b[a-zA-Z_!$%&*+\-/<=>?@^~][a-zA-Z0-9_!$%&*+\-/<=>?@^~]*/g,
      priority: 30
    }
  ]
});