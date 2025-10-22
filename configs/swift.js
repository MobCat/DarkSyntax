// darkSyntax/configs/swift.js - Swift configuration
// ===================================================
// Swift (2014)
// Modern, safe, and fast programming language for Apple platforms
//
// Configs
// =======================
// ALIASES: ['swift']
// File extensions: .swift
//
// .swift - Swift source files
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Announced at WWDC (2014) by Chris Lattner and Apple
// - Open-sourced (2015) - Swift.org launched, Linux support added
// - Swift 2 (2015) - Protocol extensions, error handling with try/catch
// - Swift 3 (2016) - Major API redesign, naming convention changes
// - Swift 4 (2017) - String improvements, Codable protocol
// - Swift 5 (2019) - ABI stability, module stability
// - Swift 5.5 (2021) - Async/await and actors for concurrency
// - Swift 6 (2024) - Data race safety, strict concurrency checking
// - Designed to replace Objective-C for Apple ecosystem
//
// INFLUENCED
// ----------
// - Rust (2010) - Ownership concepts inspired Swift's value semantics
// - Kotlin (2011) - Null safety, extension functions
// - C# (2000) - Properties, LINQ-like features
// - Python (1991) - Clean syntax, REPL, readability
// - Ruby (1995) - Optional chaining, guard clauses
// - Haskell (1990) - Pattern matching, optionals as Maybe monad
// - ML family - Type inference, algebraic data types
//
// USED FOR
// --------
// - iOS app development (primary language)
// - macOS application development
// - watchOS and tvOS applications
// - Server-side Swift (Vapor, Kitura frameworks)
// - Command-line tools and scripts
// - Cross-platform development (with Swift on Linux/Windows)
// - Swift Playgrounds for education
//
// KEY FEATURES
// ------------
// - Type-safe with strong type inference
// - Optional types (no null pointer crashes)
// - Value types (structs, enums) preferred over classes
// - Protocol-oriented programming
// - Automatic Reference Counting (ARC) for memory management
// - First-class functions and closures
// - Generics with associated types
// - Pattern matching with switch statements
// - Modern async/await for concurrency
//
// CORE SYNTAX
// -----------
// - Variable declarations: var x = 5, let constant = 10
// - Type annotations: var name: String = "Alice"
// - Optionals: String?, Int?
// - Optional binding: if let value = optional { }
// - Guard statements: guard let x = optional else { return }
// - Nil coalescing: value ?? defaultValue
// - String interpolation: "Hello \(name)"
// - Closures: { (x: Int) -> Int in return x * 2 }
// - Trailing closure syntax: numbers.map { $0 * 2 }
// - Extensions: extension String { }
// - Protocols: protocol MyProtocol { }
//
// QUIRKS
// ------
// - **Optionals are NOT null/nil**:
//   * Optional<Int> is an enum: .some(value) or .none
//   * Int? is syntactic sugar for Optional<Int>
//   * Must explicitly unwrap: value!, value?, if let, guard let
//   * Prevents null pointer exceptions at compile time
// - **Value types vs reference types**:
//   * Structs and enums are value types (copied on assignment)
//   * Classes are reference types (shared references)
//   * Array, Dictionary, String are structs (value types!)
//   * Very different from most OOP languages where collections are references
// - **Implicit returns in single-expression functions**:
//   * func double(_ x: Int) -> Int { x * 2 } // no return needed
//   * Closures too: numbers.map { $0 * 2 }
// - **Trailing closures look weird at first**:
//   * UIView.animate(withDuration: 1.0) { view.alpha = 0 }
//   * Last parameter can be outside parentheses if it's a closure
// - **Argument labels vs parameter names**:
//   * func greet(person name: String) -> String
//   * 'person' is external label, 'name' is internal parameter
//   * Called as: greet(person: "Alice")
//   * Can use _ to omit label: func greet(_ name: String)
// - **String interpolation requires backslash**:
//   * "Hello \(name)" not "Hello $(name)" or "Hello ${name}"
//   * Easy to forget the backslash
// - **Switch must be exhaustive**:
//   * Must cover all cases or use default
//   * No fallthrough (opposite of C/Java)
//   * To fallthrough: use 'fallthrough' keyword explicitly
// - **let vs var is strictly enforced**:
//   * let creates immutable bindings (compiler error if you try to mutate)
//   * Swift encourages immutability
//   * Very different from JavaScript where const is shallow
// - **No implicit conversions**:
//   * let x: Int = 5; let y: Double = x // ERROR
//   * Must explicitly convert: let y: Double = Double(x)
//   * Even Int and Double don't implicitly convert
// - **Underscores in numbers for readability**:
//   * let million = 1_000_000
//   * let hex = 0xFF_FF_FF
// - **Multiple return values with tuples**:
//   * func minMax(array: [Int]) -> (min: Int, max: Int)
//   * let bounds = minMax(array: [1, 2, 3])
//   * Access: bounds.min, bounds.max
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Protocol-oriented programming is better than object-oriented programming" - Apple at WWDC 2015
// - "Swift is Objective-C without the C" - Chris Lattner
// - "If it compiles, it works" - Similar to Haskell, due to strong type system
// - "Crusty old Objective-C developers" - Self-deprecating humor from Apple presenters
//
// NOTES ON SWIFT SYNTAX
// -------------------------
// - Comments: // for single line, /* */ for multi-line
// - Nested multi-line comments are allowed: /* outer /* inner */ outer */
// - Type inference is strong but explicit types encouraged for clarity
// - CamelCase for types, lowerCamelCase for variables/functions
// - Semicolons optional (rarely used)
// - Parentheses optional in conditionals: if x > 5 { }
// - Range operators: 0...10 (inclusive), 0..<10 (half-open)
// - Attributes use @ prefix: @available, @discardableResult
// - Property wrappers: @State, @Binding (SwiftUI)


// SWIFT SYNTAX CONFIGURATION FOR DARKSYNTAX
// ==========================================
darkSyntax.registerLanguage('swift', {
  rules: [
    // Multi-line comments (including nested)
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },

    // Single-line comments
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },

    // String interpolation (tricky - need to handle \(expr))
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\(?:[^\n(]|\([^)]*\)))*"/g,
      priority: 90
    },

    // Multi-line strings
    {
      class: 'string',
      pattern: /"""[\s\S]*?"""/g,
      priority: 91
    },

    // Attributes (@available, @discardableResult, etc.)
    {
      class: 'decorator',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 75
    },

    // Property wrappers and SwiftUI attributes
    {
      class: 'decorator',
      pattern: /@(State|Binding|Published|ObservedObject|StateObject|Environment|EnvironmentObject|AppStorage|SceneStorage|FetchRequest|UIApplicationDelegateAdaptor)\b/g,
      priority: 76
    },

    // Class, struct, enum, protocol definitions
    {
      class: 'class',
      pattern: /\b(class|struct|enum|protocol|actor|extension)\s+([A-Z][a-zA-Z0-9_]*)/g,
      captureGroup: 2,
      priority: 65
    },

    // Type names (uppercase identifiers)
    {
      class: 'class',
      pattern: /\b[A-Z][a-zA-Z0-9_]*/g,
      priority: 40
    },

    // Keywords (avoid matching argument labels with :)
    {
      class: 'keyword',
      pattern: /\b(get|set|associatedtype|break|case|catch|class|continue|default|defer|deinit|do|else|enum|extension|fallthrough|for|func|guard|if|import|in|init|inout|is|let|operator|protocol|repeat|return|self|Self|static|struct|subscript|switch|throw|throws|try|var|where|while|actor|async|await|some|any)(?!\s*:)\b/g,
      priority: 50
    },

    // Access control and modifiers (avoid matching argument labels with :)
    {
      class: 'keyword',
      pattern: /\b(public|private|fileprivate|internal|open|final|lazy|mutating|nonmutating|override|required|weak|unowned|convenience|dynamic|indirect|optional)(?!\s*:)\b/g,
      priority: 50
    },

    // Built-in types
    {
      class: 'builtin',
      pattern: /\b(Int|Int8|Int16|Int32|Int64|UInt|UInt8|UInt16|UInt32|UInt64|Float|Double|Bool|String|Character|Array|Dictionary|Set|Optional|Result|Range|ClosedRange|Any|AnyObject|Never|Void)\b/g,
      priority: 45
    },

    // Built-in functions and common methods
    {
      class: 'builtin',
      pattern: /\b(print|debugPrint|dump|readLine|abs|min|max|assert|precondition|fatalError|type|sizeof|stride)\b/g,
      priority: 45
    },

    // Function definitions
    {
      class: 'function',
      pattern: /\bfunc\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 60
    },

    // Function and method calls
    {
      class: 'function',
      pattern: /\b([a-z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },

    // Method calls with dot notation
    {
      class: 'function',
      pattern: /\.([a-z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      captureGroup: 1,
      priority: 34
    },

    // Boolean literals
    {
      class: 'boolean',
      pattern: /\b(true|false|nil)\b/g,
      priority: 25
    },

    // Numbers with underscores, hex, binary, octal
    {
      class: 'number',
      pattern: /\b0x[0-9a-fA-F_]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0o[0-7_]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0b[01_]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d[0-9_]*\.?[0-9_]*([eE][+-]?\d[0-9_]*)?\b/g,
      priority: 21
    },

    // Operators (Swift has many!)
    {
      class: 'operator',
      pattern: /(?:\?\?|\.\.\.|\.\.<|===|!==|&&|\|\||<<|>>|&\+|&-|&\*|~=|->|=>|[+\-*\/%=<>!&|^~?.])/g,
      priority: 58
    }
  ]
});