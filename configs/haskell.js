// darkSyntax/configs/haskell.js - Haskell configuration
// =======================================================
// Haskell (1990)
// Pure functional programming language with strong static typing and lazy evaluation
//
// Configs
// =======================
// ALIASES: ['haskell', 'hs']
// File extensions: .hs, .lhs
//
// .hs - Haskell source files
// .lhs - Literate Haskell (code mixed with documentation)
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Committee created language (1990) - Unified functional programming research
// - Haskell 98 (1998) - First stable specification, widely adopted standard
// - Haskell 2010 (2010) - Current stable standard with minor revisions
// - GHC (Glasgow Haskell Compiler) - De facto standard compiler, continuous innovation
// - Influenced type systems in mainstream languages (TypeScript, Swift, Rust)
// - Monads paper (1992) - Philip Wadler formalized monads for I/O
// - Named after logician Haskell Curry
//
// INFLUENCED
// ----------
// - Rust (2010) - Type classes inspired traits, algebraic data types
// - Swift (2014) - Optional chaining, pattern matching, guard clauses
// - Scala (2004) - Functional programming features, type inference
// - F# (2005) - Pattern matching, discriminated unions, type inference
// - TypeScript (2012) - Union types, type inference improvements
// - PureScript (2013) - Haskell syntax for JavaScript compilation
// - Elm (2012) - Pure functional for web, Haskell-inspired
// - Idris (2007) - Dependent types, Haskell-like syntax
//
// USED FOR
// --------
// - Academic research and education in type theory
// - Financial systems (Standard Chartered, Barclays)
// - Compiler development (GHC itself, Purescript, Elm)
// - Formal verification and proof systems
// - Blockchain and cryptocurrency (Cardano uses Haskell)
// - Web frameworks (Yesod, Servant, IHP)
// - Parsing and DSL construction
//
// KEY FEATURES
// ------------
// - Pure functions - no side effects (except in IO monad)
// - Lazy evaluation - expressions evaluated only when needed
// - Strong static typing with type inference
// - Algebraic data types and pattern matching
// - Type classes for ad-hoc polymorphism
// - Monads for managing effects (IO, State, Maybe)
// - Immutability by default
// - Higher-order functions and currying
//
// CORE SYNTAX
// -----------
// - Function definition: f x y = x + y
// - Type signatures: f :: Int -> Int -> Int
// - Lists: [1, 2, 3], list comprehensions: [x*2 | x <- [1..10]]
// - Tuples: (1, "hello", True)
// - Pattern matching in function definitions
// - Case expressions: case x of ...
// - Let bindings: let x = 5 in x + 10
// - Where clauses: f x = y + z where y = x * 2; z = x + 1
// - Lambda functions: \x -> x * 2
// - Function composition: (f . g) x = f (g x)
// - Infix operators: 2 `div` 3, x `elem` list
//
// QUIRKS
// ------
// - **Indentation is significant (like Python)**:
//   * Layout rule determines scope based on indentation
//   * Can use explicit braces and semicolons but rarely done
//   * Mixing tabs and spaces causes parse errors
// - **Function application has highest precedence**:
//   * f x + 1 means (f x) + 1, not f (x + 1)
//   * Must use parentheses: f (x + 1)
//   * Very common source of beginner errors
// - **Lists are homogeneous**:
//   * [1, 2, "three"] is a type error
//   * All elements must be same type
//   * Unlike dynamically typed languages (Python, JavaScript)
// - **Lazy evaluation can cause space leaks**:
//   * sum [1..10000000] can accumulate huge thunks
//   * Need strict evaluation (!) or seq to force evaluation
//   * Performance tuning often involves strictness annotations
// - **Strings are lists of characters**:
//   * "hello" is actually [Char], which is ['h','e','l','l','o']
//   * Inefficient for large strings
//   * Text type from text package is recommended
// - **No return statement**:
//   * Last expression is automatically returned
//   * return is actually a monadic operation, not a keyword!
//   * return x in IO monad wraps x, doesn't exit function
// - **Operators are functions**:
//   * (+) 2 3 is same as 2 + 3
//   * Can define custom operators: x <+> y
//   * Any function can be infix with backticks: 10 `div` 3
// - **Type variables start lowercase**:
//   * a, b, foo are type variables (generic)
//   * Int, String, MyType are concrete types (start uppercase)
//   * Opposite of most languages!
// - **Partial application everywhere**:
//   * map (+1) [1,2,3] - (+1) is a partially applied function
//   * (2+) means \x -> 2 + x
//   * (+2) means \x -> x + 2
// - **Bottom value (⊥) and undefined**:
//   * undefined is a valid value of any type (crashes when evaluated)
//   * Used as placeholder or for non-terminating computations
//   * error "message" also has type ∀a. a
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Avoid success at all costs" - Haskell committee's unofficial motto (freedom to experiment)
// - "If it compiles, it works" - Strong type system catches many bugs
// - "A monad is just a monoid in the category of endofunctors, what's the problem?" - Infamous joke
// - "Learn Haskell and become a better programmer in any language" - Common wisdom
//
// NOTES ON HASKELL SYNTAX
// -------------------------
// - Comments: -- for single line, {- -} for multi-line
// - Type signatures use :: (double colon)
// - Function types use -> arrow
// - Type constraints use => (fat arrow): Eq a => a -> Bool
// - List comprehensions: [expression | generators, guards]
// - Lambda syntax: \param -> body (backslash looks like λ)
// - Case-sensitive language
// - Function and variable names: lowercase or start with lowercase
// - Type and constructor names: start with uppercase
// - Operators: symbolic names like +, *, >>=, <$>


// HASKELL SYNTAX CONFIGURATION FOR DARKSYNTAX
// ============================================
darkSyntax.registerLanguage('haskell', {
  rules: [
    // Multi-line comments {- -}
    {
      class: 'comment',
      pattern: /\{-[\s\S]*?-\}/g,
      priority: 100
    },

    // Single-line comments
    {
      class: 'comment',
      pattern: /--.*$/gm,
      priority: 100
    },

    // Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"/g,
      priority: 90
    },

    // Character literals
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Pragma directives {-# #-}
    {
      class: 'decorator',
      pattern: /\{-#[\s\S]*?#-\}/g,
      priority: 75
    },

    // Type signatures (name :: Type)
    {
      class: 'function',
      pattern: /\b([a-z_][a-zA-Z0-9_']*)\s*::/g,
      captureGroup: 1,
      priority: 70
    },

    // Data type definitions
    {
      class: 'class',
      pattern: /\b(data|newtype|type)\s+([A-Z][a-zA-Z0-9_']*)/g,
      captureGroup: 2,
      priority: 66
    },

    // Type class definitions
    {
      class: 'class',
      pattern: /\b(class)\s+([A-Z][a-zA-Z0-9_']*)/g,
      captureGroup: 2,
      priority: 66
    },

    // Type constructors and types (uppercase identifiers)
    {
      class: 'class',
      pattern: /\b[A-Z][a-zA-Z0-9_']*/g,
      priority: 40
    },

    // Keywords
    {
      class: 'keyword',
      pattern: /\b(case|class|data|default|deriving|do|else|if|import|in|infix|infixl|infixr|instance|let|module|newtype|of|then|type|where)\b/g,
      priority: 50
    },

    // Special keywords (qualified imports, hiding, as)
    {
      class: 'keyword',
      pattern: /\b(qualified|hiding|as)\b/g,
      priority: 50
    },

    // Built-in types and type classes (must not match if followed by ')
    {
      class: 'builtin',
      pattern: /\b(Int|Integer|Float|Double|Char|String|Bool|Maybe|Either|IO|Ord|Eq|Show|Read|Functor|Applicative|Monad|Foldable|Traversable)(?!')\b/g,
      priority: 45
    },

    // Built-in functions (must not match if followed by ')
    {
      class: 'builtin',
      pattern: /\b(map|filter|foldr|foldl|head|tail|init|last|take|drop|sum|product|length|reverse|concat|zip|unzip|replicate|print|putStrLn|getLine|return|pure|undefined|error|otherwise)(?!')\b/g,
      priority: 45
    },

    // Function definitions (lowercase at start of line)
    {
      class: 'function',
      pattern: /^([a-z_][a-zA-Z0-9_']*)\s+/gm,
      captureGroup: 1,
      priority: 35
    },

    // Boolean values and wildcard
    {
      class: 'boolean',
      pattern: /\b(True|False|_)\b/g,
      priority: 25
    },

    // Numbers (including hex, octal, floating point, scientific notation)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0[oO][0-7]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?\b/g,
      priority: 21
    },

    // Operators (Haskell has many!)
    {
      class: 'operator',
      pattern: /(?:<-|->|=>|::|\||\\|@|~|<=|>=|==|\/=|\+\+|!!|\$|&&|\|\||<\*>|<\$>|>>=|>>|=<<|<\*|<\|>|\*>|<&>|\.\.|\.|[+\-*\/%<>=!&|^])/g,
      priority: 58
    }
  ]
});