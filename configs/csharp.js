// darkSyntax/configs/csharp.js - C# language configuration
// ============================================================
// C# (2002)
// C Sharp - Modern, type-safe object-oriented language for .NET
//
// Configs
// =======================
// ALIASES: ['csharp', 'cs', 'c#']
// File extensions: .cs, .csx
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - C# 1.0 released with .NET Framework 1.0 (2002) - Microsoft's answer to Java
// - C# 2.0 (2005) - Introduced generics, transforming type safety and reusability
// - C# 3.0 (2007) - LINQ revolutionized data querying with lambda expressions
// - C# 4.0 (2010) - Added dynamic typing and improved COM interoperability
// - C# 5.0 (2012) - Async/await made asynchronous programming mainstream
// - C# 6.0 (2015) - Expression-bodied members and null-conditional operators
// - C# 7.0 (2017) - Pattern matching, tuples, and local functions
// - C# 8.0 (2019) - Nullable reference types for safer null handling
// - C# 9.0 (2020) - Records and top-level statements reduced boilerplate
// - C# 10.0 (2021) - Global usings and file-scoped namespaces
// - C# 11.0 (2022) - Raw string literals and generic math
// - C# 12.0 (2023) - Primary constructors and collection expressions
// - C# 13.0 (2024) - params collections and new lock semantics
// - C# 14.0 (2025) - Extension members and field-backed properties
// - Originally codenamed "COOL" (C-style Object Oriented Language) in 1999
// - Mascot "Andy" (named after Anders Hejlsberg) retired in 2004
// - ECMA-334 and ISO/IEC 23270 standardized (2002-2003)
// - Open-sourced Roslyn compiler (2014) written in C# itself
// - .NET Framework evolved into cross-platform .NET Core/.NET 5+
//
// INFLUENCED
// ----------
// - Java (1995) - Type safety, garbage collection, exception handling model
// - C++ (1985) - Syntax, operators, unsafe code, and performance features
// - Delphi (1995) - Properties, events, and component-based design
// - Visual Basic (1991) - Event-driven programming, simplified syntax goals
// - TypeScript (2012) - Anders Hejlsberg's later work, modern language features
// - Kotlin (2011) - Null safety, extension methods, and modern OOP patterns
// - F# (2005) - Functional programming features, LINQ inspiration
// - Swift (2014) - Modern syntax, optionals, and safety features
// - Rust (2010) - Memory safety concepts and pattern matching
// - Scala (2004) - Type inference and functional programming constructs
// - Haskell (1990) - LINQ's functional foundations and lambda expressions
//
// USED FOR
// --------
// - Enterprise web applications (ASP.NET, Blazor)
// - Desktop applications (WPF, WinForms, MAUI)
// - Game development (Unity engine uses C# as primary language)
// - Cloud services and microservices (Azure, AWS Lambda)
// - Mobile apps (Xamarin, .NET MAUI)
// - Backend APIs and services (ASP.NET Core Web API)
// - Windows system utilities and tools
// - Cross-platform console applications
// - IoT and embedded systems (.NET nanoFramework)
//
// KEY FEATURES
// ------------
// - Strong static typing with type inference (var, implicit types)
// - Automatic memory management via garbage collection
// - First-class async/await for asynchronous programming
// - LINQ (Language Integrated Query) for data manipulation
// - Properties with get/set accessors instead of getter/setter methods
// - Events and delegates for callback mechanisms
// - Extension methods to add functionality to existing types
// - Pattern matching with switch expressions
// - Nullable reference types for null safety (C# 8+)
// - Records for immutable data structures
// - Top-level statements for minimal program boilerplate
// - Native JSON handling and serialization
// - Cross-platform support via .NET 5+
//
// CORE SYNTAX
// -----------
// Basic structure:
//   using System;
//   
//   class Program {
//       static void Main(string[] args) {
//           Console.WriteLine("Hello World");
//       }
//   }
//
// Modern structure (C# 9+):
//   using System;
//   Console.WriteLine("Hello World");
//
// Properties:
//   public string Name { get; set; }
//   public int Age { get; init; }
//
// LINQ:
//   var adults = people.Where(p => p.Age >= 18)
//                      .OrderBy(p => p.Name);
//
// Async/Await:
//   public async Task<string> GetDataAsync() {
//       return await httpClient.GetStringAsync(url);
//   }
//
// Pattern Matching:
//   var result = obj switch {
//       null => "null",
//       string s => $"string: {s}",
//       int i when i > 0 => "positive",
//       _ => "other"
//   };
//
// QUIRKS
// ------
// - **Case sensitivity**: Unlike VB.NET, C# is case-sensitive
//   * `MyVar` and `myvar` are different identifiers
//   * Convention: PascalCase for public members, camelCase for private
//
// - **Verbatim strings**: Prefix with @ to escape special characters
//   * `@"C:\Windows\System32"` avoids needing `"C:\\Windows\\System32"`
//   * Line breaks are preserved in verbatim strings
//
// - **String interpolation evolution**: Multiple syntaxes available
//   * Regular: `$"Hello {name}"`
//   * Verbatim interpolated: `$@"Path: {path}\newline"`
//   * Raw string literals (C# 11+): `"""This is a "quoted" text"""`
//
// - **var keyword**: Not dynamic typing, just type inference
//   * Type is determined at compile time: `var x = 5;` is `int x = 5;`
//   * Cannot change type: `x = "hello";` is a compile error
//
// - **Boxing and unboxing**: Value types can be converted to objects
//   * Boxing: `object o = 42;` (int to object)
//   * Unboxing: `int i = (int)o;` (object back to int)
//   * Performance cost when boxing/unboxing in loops
//
// - **Nullable types**: Two different nullable systems
//   * Nullable value types: `int? x = null;` (shorthand for `Nullable<int>`)
//   * Nullable reference types (C# 8+): Warnings for potential null references
//
// - **Async void**: Special case, generally avoided
//   * `async Task` is preferred over `async void`
//   * `async void` only for event handlers: `async void Button_Click(...)`
//
// - **Extension methods**: Add methods to types you don't own
//   * Defined as static methods in static classes
//   * First parameter with `this` keyword: `static void Print(this string s)`
//   * Called like instance methods: `"hello".Print();`
//
// - **Operator precedence surprises**: Bitwise vs logical operators
//   * `&` and `|` have different precedence than `&&` and `||`
//   * `a & b == c` is `a & (b == c)`, not `(a & b) == c`
//
// - **Partial classes**: Class definition split across multiple files
//   * Used extensively by designers and code generators
//   * All parts must use `partial` keyword and same access modifiers
//
// - **Anonymous types**: Type name generated by compiler
//   * `var person = new { Name = "John", Age = 30 };`
//   * Only usable within method scope, cannot be returned
//
// - **Multiple Main methods**: Can have multiple entry points
//   * Use `-main:ClassName` compiler flag to specify which to use
//   * Top-level statements (C# 9+) simplified this
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "C# is 'not a Java clone' and is 'much closer to C++' in its design" - Anders Hejlsberg (2000)
// - "When you build software today, you start small, but you end up big. And when you get big, if you don't have types, you're going to suffer" - Anders Hejlsberg
// - "The goal was to create a language that was as productive as Visual Basic, as powerful as C++, and as safe as Java" - Anders Hejlsberg
// - "LINQ changes the game. It's not just about querying databases - it's about having a unified way to work with any collection of data" - Scott Guthrie
// - "Async/await is one of the most important features we've ever added to C#" - Eric Lippert
// - "C# is designed to make the difficult things possible and the easy things easy" - Microsoft C# Team
//
// NOTES ON C# SYNTAX
// ------------------
// - Case-sensitive language (unlike Visual Basic)
// - Statements end with semicolon, blocks use curly braces
// - Comments: // single-line, /* multi-line */, /// XML documentation
// - String types: "regular", @"verbatim", $"interpolated", $@"both", """raw"""
// - Access modifiers: public, private, protected, internal, protected internal
// - Type system: Value types (structs) vs Reference types (classes)
// - Properties preferred over public fields for encapsulation
// - LINQ syntax: Query syntax (SQL-like) or Method syntax (fluent)
// - Lambda expressions: parameter => expression or (param) => { statements }
// - Pattern matching: switch expressions, property patterns, type patterns
// - Nullable annotations require <Nullable>enable</Nullable> in project file
// - Attributes in square brackets: [Serializable], [Obsolete]
// - Preprocessor directives: #if, #define, #region, #warning
// - unsafe code requires explicit permission and unsafe keyword


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('csharp', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\/\/\/.*$/gm,  // XML documentation comments
      priority: 100
    },
    
    // PRIORITY 95: Verbatim and interpolated strings
    {
      class: 'string',
      pattern: /@"(?:""|[^"])*"/g,  // Verbatim strings @"..."
      priority: 95
    },
    {
      class: 'string',
      pattern: /\$@"(?:""|[^"])*"/g,  // Verbatim interpolated $@"..."
      priority: 95
    },
    {
      class: 'string',
      pattern: /\$"(?:[^"\\]|\\.)*"/g,  // Interpolated strings $"..."
      priority: 95
    },
    
    // PRIORITY 90: Regular strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,  // Character literals
      priority: 90
    },
    
    // PRIORITY 70: Attributes
    {
      class: 'decorator',
      pattern: /\[[^\]]+\]/g,
      priority: 70
    },
    
    // PRIORITY 65: Preprocessor directives
    {
      class: 'decorator',
      pattern: /#(if|else|elif|endif|define|undef|warning|error|line|region|endregion|pragma)\b/g,
      priority: 65
    },
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(abstract|as|base|break|case|catch|checked|class|const|continue|default|delegate|do|else|enum|event|explicit|extern|false|finally|fixed|for|foreach|goto|if|implicit|in|interface|internal|is|lock|namespace|new|null|operator|out|override|params|private|protected|public|readonly|ref|return|sealed|sizeof|stackalloc|static|struct|switch|this|throw|true|try|typeof|unchecked|unsafe|using|virtual|void|volatile|while)\b/g,
      priority: 50
    },
    
    // PRIORITY 48: Contextual keywords (async/await, var, dynamic, etc.)
    {
      class: 'keyword',
      pattern: /\b(abstract|add|alias|ascending|async|await|by|descending|dynamic|equals|from|get|global|group|init|into|join|let|nameof|nint|not|notnull|nuint|on|orderby|partial|record|remove|required|select|set|unmanaged|value|var|when|where|with|yield)\b/g,
      priority: 48
    },
    
    // PRIORITY 45: Built-in types
    {
      class: 'builtin',
      pattern: /\b(bool|byte|sbyte|char|decimal|double|float|int|uint|long|ulong|short|ushort|object|string|dynamic|var)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Common .NET types and namespaces
    {
      class: 'builtin',
      pattern: /\b(System|Console|String|Int32|Int64|Boolean|Object|List|Dictionary|Array|Tuple|Task|Action|Func|IEnumerable|ICollection|IList|IDictionary|DateTime|TimeSpan|Exception|ArgumentException|NullReferenceException|InvalidOperationException|NotImplementedException|Math|Convert|Enum|Type|Attribute)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: LINQ keywords
    {
      class: 'keyword',
      pattern: /\b(from|where|select|group|into|orderby|join|let|in|on|equals|by|ascending|descending)\b/g,
      priority: 35
    },
    
    // PRIORITY 32: Method/function declarations
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\s*\()/g,
      priority: 32
    },
    
    // PRIORITY 30: Property/method access
    {
      class: 'function',
      pattern: /\.([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 30
    },
    
    // PRIORITY 25: Numeric literals
    {
      class: 'number',
      pattern: /\b0x[0-9A-Fa-f]+[ULul]?\b/g,  // Hexadecimal
      priority: 25
    },
    {
      class: 'number',
      pattern: /\b0b[01]+[ULul]?\b/g,  // Binary
      priority: 25
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?[fFdDmM]?\b/g,  // Decimal, scientific, with suffixes
      priority: 20
    },
    
    // PRIORITY 20: Boolean and null
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 20
    }
  ]
});