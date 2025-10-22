// darkSyntax/configs/scala.js - Scala configuration
// ===================================================
// Scala (2004)
// Scalable language combining object-oriented and functional programming on JVM
//
// Configs
// =======================
// ALIASES: ['scala', 'sc']
// File extensions: .scala, .sc
//
// .scala - Scala source files
// .sc - Scala script files (Ammonite, Scala CLI)
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Martin Odersky at EPFL (2004) - Previously worked on Java generics
// - Scala 2.0 (2006) - First stable release with pattern matching
// - Akka framework released (2009) - Actor model for concurrency
// - Twitter migration to Scala (2009) - Major validation of production readiness
// - Scala 2.10 (2013) - String interpolation, implicit classes, futures
// - Scala 2.11-2.13 (2014-2019) - Performance improvements, collections redesign
// - Scala 3 (Dotty) released (2021) - Complete redesign with new syntax, metaprogramming
// - Apache Spark (2014) - Written in Scala, dominates big data processing
// - Name means "scalable language" - from small scripts to large systems
//
// INFLUENCED
// ----------
// - Kotlin (2011) - Extension functions, type inference, functional features
// - Swift (2014) - Pattern matching, optionals, protocol-oriented design
// - Rust (2010) - Trait system inspired by Scala traits
// - TypeScript (2012) - Type inference, union types
// - Java (1995) - Generics design (Odersky's work influenced Java 5)
// - F# (2005) - Functional programming on .NET platform
// - Ceylon (2011) - Type system features
//
// USED FOR
// --------
// - Big data processing (Apache Spark, Apache Flink, Apache Kafka)
// - Distributed systems (Akka actors, clustering)
// - Web backends (Play Framework, http4s, ZIO)
// - Financial services (Goldman Sachs, Morgan Stanley)
// - Streaming data pipelines (LinkedIn, Netflix)
// - Compiler development (Scala compiler itself)
// - Type-level programming and DSLs
//
// KEY FEATURES
// ------------
// - Unified type system (everything is an object, including functions)
// - Advanced type inference (local type inference)
// - Pattern matching with case classes
// - Traits (like interfaces with implementation)
// - Implicits for type classes and conversions
// - For-comprehensions (monadic composition)
// - Higher-kinded types
// - Call-by-name parameters
// - Immutable collections by default
//
// CORE SYNTAX
// -----------
// - Variable declarations: var mutable = 5, val immutable = 10
// - Type annotations: val name: String = "Alice"
// - Function definitions: def add(x: Int, y: Int): Int = x + y
// - Anonymous functions: (x: Int) => x * 2 or _ * 2
// - Case classes: case class Person(name: String, age: Int)
// - Pattern matching: x match { case 1 => "one"; case _ => "other" }
// - For-comprehension: for (x <- list) yield x * 2
// - String interpolation: s"Hello $name" or f"Pi is $pi%.2f"
// - Option type: Some(value) or None
// - Traits: trait MyTrait { def method: String }
//
// QUIRKS
// ------
// - **Everything is an expression**:
//   * if/else returns a value: val x = if (condition) 1 else 2
//   * try/catch returns a value
//   * Even { } blocks return last expression
//   * No void type, use Unit (like Kotlin)
// - **Parentheses are optional for zero-arg methods**:
//   * list.length or list.length() both work
//   * list.reverse or list.reverse() both valid
//   * Convention: no parens for pure functions, parens for side effects
// - **Semicolons are optional (and usually omitted)**:
//   * Only needed for multiple statements on one line
//   * Scala infers statement boundaries
// - **return is discouraged**:
//   * Last expression is automatically returned
//   * Using 'return' is non-idiomatic and breaks type inference in lambdas
//   * Early returns work but considered code smell
// - **Dot and parentheses optional for infix notation**:
//   * 1.+(2) same as 1 + 2
//   * list.map(f) same as list map f
//   * Can define operators as methods: def +(other: Int)
// - **Underscores have many meanings**:
//   * Placeholder in lambdas: _ * 2
//   * Wildcard in imports: import scala.collection._
//   * Partial function application: add(5, _)
//   * Existential types: List[_]
//   * Pattern matching: case _ => default
// - **Call-by-name parameters**:
//   * def time(block: => Unit) - block evaluated each time used
//   * Different from call-by-value
//   * Used for creating control structures
// - **Implicit parameters are magic (and confusing)**:
//   * def show[T](x: T)(implicit s: Show[T])
//   * Compiler automatically provides implicit values
//   * Type class pattern but can be hard to debug
//   * Scala 3 replaces with 'given' and 'using'
// - **For-comprehensions desugar to flatMap/map**:
//   * for { x <- a; y <- b } yield x + y
//   * Becomes: a.flatMap(x => b.map(y => x + y))
//   * Works with any type that has flatMap/map
// - **No 'new' for case classes**:
//   * case class Person(name: String)
//   * val p = Person("Alice") not new Person("Alice")
//   * Regular classes still need 'new'
// - **Colon position indicates parameter vs return type**:
//   * def add(x: Int): Int - x is parameter, Int is return type
//   * Position of colon matters for reading
// - **Type variance is complex**:
//   * List[+A] covariant, List[-A] contravariant, List[A] invariant
//   * Affects subtyping relationships
//   * More complex than most languages
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Scala: the good parts" - Twitter's internal guide (most features considered complex)
// - "A scalable language" - From small scripts to large systems
// - "Functional programming for the JVM" - Early marketing pitch
// - "With great power comes great complexity" - Community acknowledgment
//
// NOTES ON SCALA SYNTAX
// -------------------------
// - Comments: // for single line, /* */ for multi-line
// - Type annotations come after identifier: val name: String
// - Function syntax: def name(param: Type): ReturnType = body
// - Lambda syntax: (x: Int) => x * 2 or x => x * 2
// - Placeholder syntax: _ + 1 means (x => x + 1)
// - String interpolation: s"$name", f"$value%.2f", raw"no\nescapes"
// - Generics use square brackets: List[Int], Map[K, V]
// - Traits use 'with' for multiple: class C extends A with B with C
// - Operators are methods: 1 + 2 is 1.+(2)


// SCALA SYNTAX CONFIGURATION FOR DARKSYNTAX
// ==========================================
darkSyntax.registerLanguage('scala', {
  rules: [
    // Multi-line comments
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

    // Triple-quoted strings (raw strings)
    {
      class: 'string',
      pattern: /"""[\s\S]*?"""/g,
      priority: 91
    },

    // String interpolation (s"", f"", raw"")
    {
      class: 'string',
      pattern: /[sfr]?"(?:[^"\\$\n]|\\.|(?:\$(?!\{))|(?:\$\{[^}]*\}))*"/g,
      priority: 90
    },

    // Character literals
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Symbols (deprecated in Scala 3 but still valid in Scala 2)
    {
      class: 'variable',
      pattern: /'[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 73
    },

    // Annotations
    {
      class: 'decorator',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 75
    },

    // Case class, sealed trait definitions
    {
      class: 'class',
      pattern: /\b(case class|case object|sealed trait|sealed abstract class|class|trait|object|enum)\s+([A-Z][a-zA-Z0-9_]*)/g,
      captureGroup: 2,
      priority: 65
    },

    // Type names (uppercase identifiers)
    {
      class: 'class',
      pattern: /\b[A-Z][a-zA-Z0-9_]*/g,
      priority: 40
    },

    // Keywords
    {
      class: 'keyword',
      pattern: /\b(abstract|case|catch|class|def|do|else|extends|final|finally|for|forSome|if|implicit|import|lazy|match|new|object|override|package|private|protected|return|sealed|super|this|throw|trait|try|type|val|var|while|with|yield)\b/g,
      priority: 50
    },

    // Scala 3 keywords
    {
      class: 'keyword',
      pattern: /\b(enum|export|given|then|using|extension|inline|opaque|open|transparent|derives|end)\b/g,
      priority: 50
    },

    // Built-in types
    {
      class: 'builtin',
      pattern: /\b(Unit|Boolean|Byte|Short|Int|Long|Float|Double|Char|String|Any|AnyRef|AnyVal|Nothing|Null|Option|Some|None|Either|Left|Right|List|Seq|Vector|Set|Map|Array|Tuple|Future|Try|Success|Failure)\b/g,
      priority: 45
    },

    // Built-in functions and common methods
    {
      class: 'builtin',
      pattern: /\b(println|print|require|assert|assume|ensuring|identity|implicitly|locally|classOf|summon)\b/g,
      priority: 45
    },

    // Function definitions
    {
      class: 'function',
      pattern: /\bdef\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
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

    // Boolean literals and null
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 25
    },

    // Numbers (hex, long suffix, float suffix, underscores)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F_]+[Ll]?\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d[0-9_]*\.?[0-9_]*([eE][+-]?\d[0-9_]*)?[fFdDlL]?\b/g,
      priority: 21
    },

    // Operators (including symbolic method names)
    {
      class: 'operator',
      pattern: /(?:=>|<-|->|<:|>:|:::|\+\+|::|&&|\|\||==|!=|<=|>=|<<|>>|>>>|[+\-*\/%=<>!&|^~?:.])/g,
      priority: 58
    }
  ]
});