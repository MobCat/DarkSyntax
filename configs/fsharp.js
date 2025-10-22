// darkSyntax/configs/fsharp.js - F# language configuration
// ===========================================================
// F# (2005)
// F Sharp - Functional-first, multi-paradigm language for .NET
//
// Configs
// =======================
// ALIASES: ['fsharp', 'fs', 'f#', 'fsi', 'fsx']
// File extensions: .fs (source), .fsi (signature/interface), .fsx (script)
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Don Syme at Microsoft Research Cambridge (2002-2005)
// - Project initiated in 2002 as "Caml.NET" to bring OCaml to .NET
// - F# 1.0 released May 2005 by Microsoft Research
// - Presented at ML Workshop 2005 (September 4, 2004 internal presentation)
// - F# 2.0 moved from Microsoft Research to product team (2010)
// - F# became fully supported in Visual Studio 2010
// - Open-sourced and cross-platform with F# Software Foundation (2012)
// - F# 3.0 (2012) - Type providers for data-rich programming
// - F# 4.0 (2015) - Constructors as first-class functions
// - F# 5.0 (2020) - String interpolation, nameof operator
// - F# 6.0 (2021) - Task computation expressions, pipelines
// - F# 7.0 (2022) - Required properties, static abstract members
// - F# 8.0 (2023) - Shorthand lambda syntax, extended collection expressions
// - F# 9.0 (2024) - Nullable reference types, discriminated union improvements
// - Don Syme: "F is for Fun" - team motto
// - Andrew Kennedy contributed units of measure feature
// - Inspired by OCaml, influenced by C#, Python, Haskell, Scala
// - First major ML-family language for .NET/CLR
// - Active Patterns (2006) - Pattern matching abstraction
// - Computation Expressions - Unified monad/workflow syntax
//
// INFLUENCED
// ----------
// - Scala (2004) - Functional programming on JVM, influenced by ML family
// - Elm (2012) - Functional web programming, similar union types and pattern matching
// - PureScript (2013) - Haskell-like language for JavaScript, ML influence
// - ReasonML (2016) - Facebook's OCaml-to-JavaScript, similar to F# goals
// - TypeScript (2012) - Union types and pattern matching proposals inspired by F#
// - C# (ongoing) - Pattern matching (C# 7+), records (C# 9), discriminated unions proposals
// - Rust (2010) - Pattern matching and algebraic data types influenced by ML family
// - Swift (2014) - Optionals, pattern matching, functional features
// - Kotlin (2011) - Sealed classes, when expressions similar to F# patterns
// - Fable (2016) - F# to JavaScript compiler
// - WebSharper (2007) - F# to JavaScript for web development
//
// USED FOR
// --------
// - Financial modeling and quantitative analysis
// - Data science and machine learning (with .NET ML)
// - Web development (ASP.NET Core, Giraffe, Saturn, Suave)
// - Domain modeling and business logic
// - Compiler and language tool development
// - Cloud applications (Azure Functions)
// - Scientific computing and simulations
// - Parallel and concurrent programming
// - Type providers for external data access
// - Full-stack web apps with Fable (F# to JavaScript)
// - Desktop applications (Avalonia, WPF)
// - Cross-platform mobile (MAUI, Xamarin)
//
// KEY FEATURES
// ------------
// - Functional-first with immutable data by default
// - Powerful type inference (minimal type annotations)
// - Algebraic data types (discriminated unions, records)
// - Pattern matching with exhaustiveness checking
// - Computation expressions (async, seq, custom workflows)
// - Type providers for accessing external data sources
// - Units of measure for numerical safety
// - Pipe operator |> for functional composition
// - Active patterns for custom pattern matching
// - Railway-oriented programming with Result type
// - First-class functions and currying
// - Interoperability with all .NET languages
// - REPL (F# Interactive - fsi)
//
// CORE SYNTAX
// -----------
// Functions:
//   let add x y = x + y
//   let result = add 5 10
//
// Pipe operator:
//   [1; 2; 3]
//   |> List.map (fun x -> x * 2)
//   |> List.sum
//
// Pattern matching:
//   match value with
//   | Some x -> printfn "Found %d" x
//   | None -> printfn "Nothing"
//
// Records:
//   type Person = { Name: string; Age: int }
//   let person = { Name = "Alice"; Age = 30 }
//
// Discriminated unions:
//   type Shape =
//     | Circle of radius: float
//     | Rectangle of width: float * height: float
//
// QUIRKS
// ------
// - **Case sensitivity**: Unlike F77 but like OCaml, F# is case-sensitive
//   * `myFunc` and `MyFunc` are different identifiers
//   * Convention: camelCase for values, PascalCase for types
//
// - **Significant whitespace**: Indentation matters for scope
//   * Like Python, blocks defined by indentation
//   * let bindings and match branches must align
//   * Can use explicit begin/end or { } for clarity
//
// - **Expression-based**: Everything is an expression
//   * if/then/else returns a value
//   * No statements, only expressions
//   * Unit type () represents "no value"
//
// - **Semicolons in lists**: Use semicolons, not commas
//   * Lists: [1; 2; 3] not [1, 2, 3]
//   * Arrays: [|1; 2; 3|]
//   * Tuples use commas: (1, 2, 3)
//
// - **Forward pipe vs backward pipe**: Two composition directions
//   * |> pipes result forward: x |> f |> g = g(f(x))
//   * <| pipes argument backward: f <| g <| x = f(g(x))
//   * >> composes functions forward: (f >> g) x = g(f(x))
//   * << composes backward: (f << g) x = f(g(x))
//
// - **Mutable keyword required**: Immutable by default
//   * let x = 1 creates immutable binding
//   * let mutable x = 1 allows mutation with <-
//   * x <- 2 is assignment, not comparison
//
// - **Type inference limitations**: Sometimes needs hints
//   * Inference works left-to-right, top-to-bottom
//   * Generic functions may need type annotations
//   * Use : to annotate: (x: int)
//
// - **No implicit conversions**: Must be explicit
//   * 1 + 1.0 is an error (int vs float)
//   * Use float 1 + 1.0 for conversion
//   * Prevents accidental type mismatches
//
// - **Function application**: No parentheses needed
//   * f x y not f(x, y) for curried functions
//   * f(x, y) is tuple argument, not two args
//   * Partial application: let add5 = (+) 5
//
// - **Option vs null**: No null by default
//   * Use Option<'T> (Some x or None)
//   * Safer than null references
//   * Interop with .NET may expose nulls
//
// - **Computation expressions**: Special syntax for workflows
//   * async { }, seq { }, task { } builders
//   * let! for bind, return! for monadic return
//   * Custom builders for domain-specific syntax
//
// - **Units of measure**: Compile-time dimensional analysis
//   * [<Measure>] type kg, type m
//   * let weight: float<kg> = 75.0<kg>
//   * Prevents mixing incompatible units
//
// - **Quotations**: Code as data
//   * <@ expr @> creates quoted expression
//   * Used for metaprogramming and code generation
//   * Similar to Lisp's quasiquoting
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "F is for Fun" - Don Syme and F# team motto
// - "Make illegal states unrepresentable" - Yaron Minsky (popularized in F# community)
// - "Railway-oriented programming" - Scott Wlaschin's error handling pattern
// - "The early conception of F# was simple: to bring the benefits of OCaml to .NET and .NET to OCaml" - Don Syme
// - "F# enables users to write simple code to solve complex problems" - Don Syme
// - "My research vision is about making typed functional programming work in practice" - Don Syme
// - "F# really does make many programming tasks simpler, and our users have consistently reported that they've found using the language enjoyable" - Don Syme
//
// NOTES ON F# SYNTAX
// ------------------
// - Case-sensitive language
// - Significant whitespace (indentation-based blocks)
// - Comments: // single-line, (* multi-line *), /// XML docs
// - Strings: "text", @"verbatim", """triple-quoted"""
// - Lists: [1; 2; 3], cons operator ::
// - Arrays: [|1; 2; 3|]
// - Tuples: (1, "hello", true) - use commas
// - Pipe operators: |> forward, <| backward
// - Composition: >> forward, << backward
// - Pattern matching: match x with | pattern -> expr
// - Function keyword: function | pattern -> expr (shorthand)
// - Type parameters: 'a, 'T, 'Key (generic types)
// - Attributes: [<Attribute>] above declarations
// - Computation expressions: async { }, seq { }, custom { }
// - Units of measure: float<kg>, int<m/s>
// - Active patterns: (|Pattern|_|) for custom matching
// - Mutable: let mutable x = 1; x <- 2
// - No implicit conversions between types


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('fsharp', {
  rules: [
    // PRIORITY 100: Multi-line comments
    {
      class: 'comment',
      pattern: /\(\*[\s\S]*?\*\)/g,
      priority: 100
    },
    
    // PRIORITY 95: XML documentation comments
    {
      class: 'comment',
      pattern: /\/\/\/.*$/gm,
      priority: 95
    },
    
    // PRIORITY 100: Single-line comments (after XML docs to not conflict)
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Triple-quoted strings (verbatim)
    {
      class: 'string',
      pattern: /"""[\s\S]*?"""/g,
      priority: 90
    },
    
    // PRIORITY 90: Verbatim strings (@"...")
    {
      class: 'string',
      pattern: /@"(?:[^"]|"")*"/g,
      priority: 90
    },
    
    // PRIORITY 90: Regular strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 90: Character literals
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)'/g,
      priority: 90
    },
    
    // PRIORITY 85: Attributes
    {
      class: 'decorator',
      pattern: /\[<[^\]>]+>\]/g,
      priority: 85
    },
    
    // PRIORITY 60: Operators (pipe, composition, cons, etc.)
    {
      class: 'keyword',
      pattern: /(\|>|<\||>>|<<|::|\.\.|<>)/g,
      priority: 60
    },
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(abstract|and|as|assert|base|begin|class|default|delegate|do|done|downcast|downto|elif|else|end|exception|extern|finally|for|fun|function|global|if|in|inherit|inline|interface|internal|lazy|let|match|member|module|mutable|namespace|new|not|of|open|or|override|private|public|rec|return|sig|static|struct|then|to|try|type|upcast|use|val|void|when|while|with|yield)\b/g,
      priority: 50
    },
    
    // PRIORITY 50: Special keywords (async, seq, etc.)
    {
      class: 'keyword',
      pattern: /\b(async|seq|query|task|computation)\b/g,
      priority: 50
    },
    
    // PRIORITY 40: Built-in types
    {
      class: 'builtin',
      pattern: /\b(int|float|double|decimal|string|char|bool|byte|sbyte|int16|uint16|int32|uint32|int64|uint64|nativeint|unativeint|bigint|unit|obj|exn|list|array|option|ref|Some|None|Ok|Error)\b/g,
      priority: 40
    },
    
    // PRIORITY 40: Common modules and namespaces
    {
      class: 'builtin',
      pattern: /\b(System|List|Array|Seq|Map|Set|Option|Result|Async|String|Math|Console|DateTime|TimeSpan)\b/g,
      priority: 40
    },
    
    // PRIORITY 30: Function definitions (let bindings)
    {
      class: 'function',
      pattern: /\blet\s+(rec\s+)?([a-zA-Z_][a-zA-Z0-9_']*)/g,
      priority: 30,
      captureGroup: 2
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_']*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Type parameters ('a, 'T, etc.)
    {
      class: 'variable',
      pattern: /'[a-zA-Z][a-zA-Z0-9_]*/g,
      priority: 25
    },
    
    // PRIORITY 20: Numbers (including hex, binary, octal, floats)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+[LlUu]?\b|\b0[oO][0-7]+[LlUu]?\b|\b0[bB][01]+[LlUu]?\b|\b\d+\.?\d*([eE][+-]?\d+)?[fFmM]?\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Booleans
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 15
    }
  ]
});