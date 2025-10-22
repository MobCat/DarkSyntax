// darkSyntax/configs/kotlin.js - Kotlin configuration
// ====================================================
// Kotlin (2011)
// Modern, concise, and safe programming language for JVM, Android, and multiplatform
//
// Configs
// =======================
// ALIASES: ['kotlin', 'kt', 'kts']
// File extensions: .kt, .kts
//
// .kt - Kotlin source files
// .kts - Kotlin script files
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Announced by JetBrains (2011) - Named after Kotlin Island near St. Petersburg
// - Kotlin 1.0 released (2016) - Production-ready for JVM
// - Google announced first-class Android support (2017) - I/O conference
// - Kotlin 1.3 (2018) - Coroutines became stable, multiplatform preview
// - Kotlin 1.4 (2020) - New compiler, performance improvements
// - Kotlin 1.5 (2021) - JVM IR backend stable, sealed interfaces
// - Kotlin 1.6-1.9 (2021-2023) - K2 compiler, context receivers, data objects
// - Android official language (2019) - Kotlin-first for Android development
//
// INFLUENCED
// ----------
// - Swift (2014) - Null safety, extension functions, trailing lambdas
// - Dart (2011) - Null safety operators (?., ?:, !!)
// - TypeScript (2012) - Gradual typing, nullable types
// - Scala (2004) - Functional programming features, type inference
// - C# (2000) - Properties, LINQ-style collections
// - Groovy (2003) - DSL capabilities, builder patterns
//
// USED FOR
// --------
// - Android app development (primary language since 2019)
// - Server-side development (Spring Boot, Ktor)
// - Multiplatform mobile (Kotlin Multiplatform Mobile - KMM)
// - Web development (Kotlin/JS)
// - Desktop applications (Compose Multiplatform)
// - Data science (Kotlin Notebooks)
// - Scripting (replacing Gradle Groovy scripts)
//
// KEY FEATURES
// ------------
// - Null safety built into type system
// - Extension functions
// - Data classes (automatic equals, hashCode, toString, copy)
// - Coroutines for asynchronous programming
// - Smart casts after type checks
// - Sealed classes for restricted hierarchies
// - Companion objects instead of static members
// - Top-level functions (no need for class wrapper)
//
// CORE SYNTAX
// -----------
// - Variable declarations: var mutable = 5, val immutable = 10
// - Type inference: val name = "Alice" (type inferred as String)
// - Nullable types: String?, Int?
// - Safe call: person?.name
// - Elvis operator: value ?: defaultValue
// - Not-null assertion: value!!
// - String templates: "Hello $name" or "Result: ${x + y}"
// - When expression: when (x) { 1 -> "one"; else -> "other" }
// - Lambda syntax: { x: Int -> x * 2 } or { it * 2 }
// - Function types: (Int) -> String
// - Extension functions: fun String.isPalindrome() = this == this.reversed()
//
// QUIRKS
// ------
// - **No 'new' keyword**:
//   * val person = Person("Alice") not new Person("Alice")
//   * Constructors called like functions
//   * Coming from Java/C++, this looks wrong at first
// - **Semicolons are optional (and discouraged)**:
//   * val x = 5; val y = 10; // Valid but frowned upon
//   * Kotlin style guide says no semicolons
// - **Everything is public by default**:
//   * Opposite of Java where package-private is default
//   * Must explicitly use 'private', 'internal', 'protected'
// - **No primitive types (at runtime they're optimized)**:
//   * Int, Boolean, Char are objects
//   * Compiler optimizes to Java primitives when possible
//   * val x: Int? can be null (boxed), val x: Int cannot
// - **Unit instead of void**:
//   * fun doSomething(): Unit { } equivalent to void
//   * Unit is an actual type with single value
//   * Return type Unit can be omitted
// - **Arrays are invariant**:
//   * Array<String> is NOT a subtype of Array<Any>
//   * Prevents runtime errors from Java's covariant arrays
// - **== checks equality, === checks reference**:
//   * == calls .equals() automatically
//   * === checks if same object reference
//   * Opposite mental model from Java where == is reference
// - **Smart casts are magical**:
//   * if (obj is String) { obj.length } // obj auto-cast to String
//   * No explicit casting needed after type check
//   * Compiler tracks type flow
// - **when is expression, not statement**:
//   * val result = when (x) { 1 -> "one"; else -> "other" }
//   * Must be exhaustive (cover all cases or have else)
//   * More powerful than Java switch
// - **Ranges are inclusive by default**:
//   * 1..10 includes 10 (1 to 10)
//   * 1 until 10 excludes 10 (1 to 9)
//   * for (i in 1..10) includes 10
// - **Companion objects, not static**:
//   * companion object { fun create() = MyClass() }
//   * Called like static: MyClass.create()
//   * Actually singleton object, can implement interfaces
// - **it is implicit lambda parameter**:
//   * list.map { it * 2 } instead of list.map { x -> x * 2 }
//   * Only works for single-parameter lambdas
//   * Can shadow 'it' from outer scope
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Kotlin is what Java should have been" - Common developer sentiment
// - "100% interoperable with Java" - JetBrains marketing (mostly true)
// - "Make Android development faster and more fun" - Google's Kotlin pitch
// - "Concise, safe, pragmatic" - Kotlin's design principles
//
// NOTES ON KOTLIN SYNTAX
// -------------------------
// - Comments: // for single line, /* */ for multi-line
// - Type annotations come after identifier: val name: String
// - Function syntax: fun name(param: Type): ReturnType { }
// - Lambdas: { param -> body } or { body } (using 'it')
// - Nullable types use ? suffix: String?, Int?
// - Generics use angle brackets: List<String>, Map<K, V>
// - Annotations use @ prefix: @Override, @Deprecated
// - Labels use @ suffix: break@loop, return@forEach
// - Backticks for reserved words: val `is` = true


// KOTLIN SYNTAX CONFIGURATION FOR DARKSYNTAX
// ===========================================
darkSyntax.registerLanguage('kotlin', {
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

    // String templates (double quotes with $)
    {
      class: 'string',
      pattern: /"(?:[^"\\$\n]|\\.|(?:\$(?!\{))|(?:\$\{[^}]*\}))*"/g,
      priority: 90
    },

    // Character literals
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Annotations
    {
      class: 'decorator',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 75
    },

    // Class, interface, object, enum definitions
    {
      class: 'class',
      pattern: /\b(class|interface|object|enum class|sealed class|sealed interface|data class|data object|value class|annotation class)\s+([A-Z][a-zA-Z0-9_]*)/g,
      captureGroup: 2,
      priority: 65
    },

    // Type names (uppercase identifiers)
    {
      class: 'class',
      pattern: /\b[A-Z][a-zA-Z0-9_]*/g,
      priority: 40
    },

    // Keywords (avoid matching when followed by :)
    {
      class: 'keyword',
      pattern: /\b(abstract|actual|annotation|as|break|by|catch|class|companion|const|constructor|continue|crossinline|data|do|dynamic|else|enum|expect|external|final|finally|for|fun|get|if|import|in|infix|init|inline|inner|interface|internal|is|lateinit|noinline|object|open|operator|out|override|package|private|protected|public|reified|return|sealed|set|super|suspend|tailrec|this|throw|try|typealias|typeof|val|var|vararg|when|where|while)(?!\s*:)\b/g,
      priority: 50
    },

    // Built-in types
    {
      class: 'builtin',
      pattern: /\b(Any|Nothing|Unit|Boolean|Byte|Short|Int|Long|Float|Double|Char|String|Array|List|MutableList|Set|MutableSet|Map|MutableMap|Pair|Triple|Sequence|Iterable|Collection|CharSequence|Number|Comparable|Throwable|Exception|Error)\b/g,
      priority: 45
    },

    // Built-in functions
    {
      class: 'builtin',
      pattern: /\b(println|print|readLine|readln|require|requireNotNull|check|checkNotNull|error|TODO|repeat|lazy|let|also|apply|run|with|takeIf|takeUnless|to|listOf|setOf|mapOf|mutableListOf|mutableSetOf|mutableMapOf|arrayOf|emptyList|emptySet|emptyMap)\b/g,
      priority: 45
    },

    // Function definitions
    {
      class: 'function',
      pattern: /\bfun\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
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

    // Numbers (hex, binary, long suffix, float suffix, underscores)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F_]+[Ll]?\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0[bB][01_]+[Ll]?\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d[0-9_]*\.?[0-9_]*([eE][+-]?\d[0-9_]*)?[fFdDlL]?\b/g,
      priority: 21
    },

    // Operators
    {
      class: 'operator',
      pattern: /(?:\?\.|!!|\.\.|\?:|===|!==|==|!=|<=|>=|&&|\|\||<<|>>|>>>|\+\+|--|[+\-*\/%=<>!&|^~?.])/g,
      priority: 58
    }
  ]
});