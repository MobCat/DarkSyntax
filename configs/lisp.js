// darkSyntax/configs/lisp.js - Lisp (1960) language configuration
// ============================================================
// Lisp (1960)
// Lisp - LISt Processing, a family of computer programming languages with a long history and a distinctive, fully parenthesized prefix notation. Developed primarily by John McCarthy and his colleagues at MIT.
//
// Configs
// =======================
// ALIASES: ['lisp', 'cl', 'el', 'l', 'lsp']
// File extensions: .lisp, .cl, .el, .l, .lsp
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Predominantly uses s-expressions (symbolic expressions) enclosed in parentheses.
// - Functions and data are treated uniformly (code as data, data as code).
// - Comments typically start with a semicolon (`;`).
// - Strings are double-quoted.
// - Keywords often follow a distinct pattern (e.g., `DE`, `LAMBDA`).
// - Built-in functions are numerous.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - 1958: John McCarthy conceives Lisp.
// - 1960: First working Lisp implementation (Lisp 1.5 Manual published in 1962).
// - 1960s: Lisp becomes the language of choice for AI research, largely due to its symbolic processing capabilities and dynamic nature.
// - Pioneered many concepts in computer science, including tree data structures, automatic storage management (garbage collection), dynamic typing, and higher-order functions.
//
// INFLUENCED
// ----------
// - Scheme (1975) - Lexical scoping, clean semantic core.
// - Smalltalk (1970s) - Dynamic typing, garbage collection, image-based development.
// - Prolog (1972) - Symbolic computation, AI programming.
// - ML (1970s) - Functional programming paradigms, lambda calculus influence.
// - JavaScript (1995) - First-class functions, anonymous functions (lambdas), dynamic typing, closures (conceptually).
//
// USED FOR
// --------
// - Artificial Intelligence research (expert systems, natural language processing).
// - Symbol manipulation and mathematical theorem proving.
// - Early operating systems and programming environments.
// - Rapid prototyping and metaprogramming.
//
// KEY FEATURES
// ------------
// - **S-expressions**: All code and data are expressed as lists.
// - **Homoeiconicity**: Code has the same structure as the data it manipulates.
// - **Garbage Collection**: Automatic memory management.
// - **Macros**: Powerful metaprogramming capabilities (though more prominent in later Lisp versions, the concept was nascent).
// - **Functional Programming**: Emphasis on immutable data and pure functions.
//
// CORE SYNTAX
// -----------
// `(function-name arg1 arg2 ...)`
// `(DE func (args) (body))` - Function definition
// `(SETQ var value)` - Variable assignment
// `' (a b c)` - Quoted list (data)
// `(COND (test1 expr1) (T expr2))` - Conditional, where `T` is true
//
// QUIRKS
// ------
// - **Excessive Parentheses**: Often referred to as "Lots of Irritating Superfluous Parentheses."
//   * Example: `(COND ((NULL X) Y) (T (CAR X)))`
// - **Prefix Notation**: All operations are prefix, even arithmetic.
//   * Example: `(+ 1 2)` instead of `1 + 2`
// - **`EVAL` function**: The ability to evaluate arbitrary Lisp code as data at runtime, a powerful but sometimes confusing feature.
//   * Example: `(EVAL '(+ 1 2))`
// - **Dynamic Scoping (in early Lisp)**: Variables were looked up in the call stack rather than where they were defined, leading to potential confusion. (Lexical scoping became standard later).
// - **Truth Value `T`**: Unlike many languages with dedicated boolean literals `true`/`false`, Lisp uses `T` for true and `NIL` (or an empty list `()`) for false.
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Lisp isn't a language, it's a building material." - Alan Kay
// - "The world is moving toward Lisp." - Paul Graham (referring to features like GC, dynamic typing, etc.)
// - "Those who don't know Lisp are doomed to reinvent it poorly." - Henry Spencer (paraphrased)
//
// NOTES ON LANGUAGE SYNTAX
// -------------------------
// - `DE` for defining functions (early Lisp). Later `DEFUN`.
// - `SETQ` for setting values of symbols. `SET` for special variables.
// - `LAMBDA` for anonymous functions.
// - `QUOTE` or `'` for preventing evaluation of an S-expression.
// - `T` is the canonical true value. `NIL` (or `()`) is the false value.
//


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('lisp', {
  rules: [
    // COMMENTS (Highest priority)
    {
      class: 'comment',
      pattern: /;.*$/gm,
      priority: 100
    },

    // STRINGS
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g, // Handles escaped quotes, stops at newlines implicitly
      priority: 90
    },

    // BOOLEAN/NIL (True/False often represented by T/NIL or symbols)
    {
      class: 'boolean',
      pattern: /\b(T|NIL)\b/g, // T for true, NIL for false/empty list
      priority: 25
    },

    // NUMBERS (Integers, basic decimals)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g, // Matches 42, 3.14
      priority: 20
    },

    // KEYWORDS / SPECIAL FORMS (e.g., DE, SETQ, COND, QUOTE, LAMBDA)
    // These are fundamental operators/forms in Lisp.
    {
      class: 'keyword',
      pattern: /\b(DE|SETQ|SET|LAMBDA|QUOTE|FUNCTION|COND|ATOM|EQUAL|ZEROP|NULL|CAR|CDR|CONS|RPLACA|RPLACD|APPEND|NCONC|PROG|GO|RETURN|CATCH|THROW|TAGBODY|BLOCK|LET|MULTIPLE-VALUE-BIND|DEFUN|DEFMACRO|DEFMETHOD|DEFCLASS)\b/g,
      priority: 50
    },

    // BUILT-IN FUNCTIONS / OPERATORS (e.g., PLUS, DIFFERENCE, TIMES, PRIN1)
    // Common functions that act like operators or built-ins.
    // Lisp treats these as regular functions, but they often appear like operators.
    {
      class: 'builtin',
      pattern: /\b(PLUS|DIFFERENCE|TIMES|QUOTIENT|REMAINDER|MINUS|GREATERP|LESSP|EQUAL|EQ|PRIN1|PRINT|READ|TERPRI|LOAD|APPLY|EVAL)\b/g,
      priority: 45
    },

    // QUOTE MARK (Special syntax for literal data)
    {
      class: 'decorator', // Using decorator as it modifies interpretation of the following form
      pattern: /'/g,
      priority: 70
    },

    // FUNCTIONS (Generic function names - must be lower priority than keywords/builtins)
    // This will catch user-defined functions and other symbols not caught by earlier rules.
    // Lisp's "functions" are often just symbols that resolve to a function object.
    {
      class: 'function',
      pattern: /\b[a-zA-Z_!$?\-*\/%&+=<>\.]+\b/g, // Catches most valid Lisp symbols, including common operator names
      priority: 10 // Very low priority to let more specific rules win
    },

    // OPERATORS (Parentheses - structural, not typically highlighted but important)
    // This is primarily for structural highlighting, if a theme supports it.
    // Lisp uses parentheses as fundamental syntax, not just operators.
    {
      class: 'operator',
      pattern: /[()]/g,
      priority: 58 // Higher than keywords/functions to ensure structural prominence
    }
  ]
});