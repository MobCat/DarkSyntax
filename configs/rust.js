// darkSyntax/configs/rust.js - Rust language configuration
// ==========================================================
// Rust (2010)
// Rust - Systems programming language focused on safety and performance
//
// Configs
// =======================
// ALIASES: ['rust', 'rs']
// File extensions: .rs
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Graydon Hoare at Mozilla Research (2006-2010)
// - First announced publicly (July 7, 2010)
// - Rust 0.1 released (January 20, 2012)
// - Rust 1.0 stable release (May 15, 2015)
// - Designed to solve memory safety issues in systems programming
// - Named after rust fungi (fast-growing, distributed, resilient)
// - Mozilla sponsored development (2009-2020)
// - Rust Foundation formed (February 8, 2021)
// - Founding members: Mozilla, AWS, Google, Microsoft, Huawei
// - "Most loved language" on Stack Overflow (2016-2023, 7 years straight)
// - Linux kernel approved Rust for development (December 2022)
// - Android Open Source Project adopted Rust (April 2021)
// - Windows components rewritten in Rust (Microsoft initiative)
// - Servo browser engine written in Rust (Mozilla)
// - Compiler moved from OCaml to self-hosted Rust (2011)
// - Borrow checker enforces memory safety at compile time
// - Zero-cost abstractions philosophy
// - Fearless concurrency without data races
// - No garbage collector needed
// - Influenced by ML, Haskell, C++, Cyclone
// - Cargo package manager and build system (2014)
// - Crates.io package registry launched (2014)
// - Edition system for backward-compatible evolution (2018)
//
// INFLUENCED
// ----------
// - C++ (1985) - Systems programming, RAII, zero-cost abstractions
// - ML family (1973+) - Type inference, algebraic data types, pattern matching
// - Haskell (1990) - Traits inspired by type classes, immutability default
// - Cyclone (2001) - Region-based memory management influence
// - OCaml (1996) - Original compiler implementation language
// - Erlang (1986) - Concurrency model inspiration
// - C (1972) - Low-level control, systems programming domain
// - Swift (2014) - Influenced by Rust's ownership concepts
// - Nim (2008) - Memory management ideas
// - Go (2009) - Simplicity goals (though approaches differ)
//
// USED FOR
// --------
// - Systems programming (operating systems, kernels)
// - Web browsers (Firefox Servo engine)
// - Game engines and high-performance games
// - Embedded systems and IoT devices
// - Command-line tools and utilities
// - Web servers and APIs (Actix, Rocket, Axum)
// - WebAssembly compilation target
// - Blockchain and cryptocurrency (Solana, Polkadot)
// - Network programming and infrastructure
// - Database systems (TiKV, Firecracker)
// - Container runtimes and orchestration
// - File systems and storage engines
// - Audio and video processing
// - Machine learning inference engines
// - Security-critical applications
// - Cross-platform desktop applications
// - Cloud infrastructure (AWS Firecracker, Cloudflare Workers)
// - Cryptography libraries
//
// KEY FEATURES
// ------------
// - Ownership system prevents memory errors at compile time
// - Borrowing and lifetimes for safe references
// - No null pointers (Option<T> for optional values)
// - Pattern matching with exhaustiveness checking
// - Zero-cost abstractions (no runtime overhead)
// - Trait-based generics (similar to type classes)
// - Memory safety without garbage collection
// - Thread safety enforced by type system
// - Immutable by default (mut for mutability)
// - Algebraic data types (enum with variants)
// - Result<T, E> for explicit error handling
// - Cargo package manager and build tool
// - Strong static typing with type inference
// - Macros for metaprogramming
// - Async/await for asynchronous programming
// - Foreign Function Interface (FFI) for C interop
// - Compiler provides helpful error messages
// - Edition system for evolution without breaking changes
// - No data races at compile time
// - Fearless concurrency guarantees
//
// CORE SYNTAX
// -----------
// Basic program:
//   fn main() {
//       println!("Hello, World!");
//   }
//
// Variables:
//   let x = 5;              // Immutable
//   let mut y = 10;         // Mutable
//   const MAX: i32 = 100;   // Constant
//
// Functions:
//   fn add(a: i32, b: i32) -> i32 {
//       a + b  // No semicolon = return
//   }
//
// Structs:
//   struct Point {
//       x: f64,
//       y: f64,
//   }
//
// Enums:
//   enum Option<T> {
//       Some(T),
//       None,
//   }
//
// Pattern matching:
//   match value {
//       Some(x) => println!("{}", x),
//       None => println!("Nothing"),
//   }
//
// QUIRKS
// ------
// - **Ownership rules**: Unique to Rust
//   * Each value has exactly one owner
//   * When owner goes out of scope, value is dropped
//   * Value can be moved (ownership transferred)
//   * Or borrowed (&T immutable, &mut T mutable)
//   * Prevents use-after-free and double-free
//
// - **Borrow checker**: Compile-time enforcement
//   * Cannot have mutable reference while immutable exists
//   * Cannot have multiple mutable references
//   * Prevents data races at compile time
//   * "Fighting the borrow checker" learning phase
//   * Error messages guide you to correct code
//
// - **Lifetimes**: Explicit lifetime annotations
//   * 'a, 'b lifetime parameters
//   * Compiler infers most lifetimes
//   * Explicit when references ambiguous
//   * fn longest<'a>(x: &'a str, y: &'a str) -> &'a str
//   * 'static for entire program duration
//
// - **No null**: Option<T> instead
//   * Some(value) or None
//   * Must explicitly handle None case
//   * Cannot accidentally dereference null
//   * Eliminates billion-dollar mistake
//
// - **Expression-based**: Almost everything is expression
//   * if, match, loop all return values
//   * Last expression in block is return value
//   * Semicolon suppresses return: x + 5; vs x + 5
//   * let x = if cond { 1 } else { 2 };
//
// - **Immutable by default**: Must opt-in to mutability
//   * let x = 5; cannot be changed
//   * let mut y = 10; can be changed
//   * Philosophy: mutation is special
//
// - **Move semantics**: Default for most types
//   * let x = vec![1, 2, 3]; let y = x;
//   * x is now invalid (moved to y)
//   * Copy trait for stack types (i32, bool)
//   * Clone trait for explicit deep copy
//
// - **Result<T, E>**: Explicit error handling
//   * Ok(value) or Err(error)
//   * Must handle errors explicitly
//   * ? operator for propagation
//   * No exceptions (error handling at compile time)
//
// - **Macros use !**: Not functions
//   * println!(), vec![], format!()
//   * Exclamation mark distinguishes from functions
//   * Compile-time code generation
//   * Can accept variable arguments
//
// - **Traits not classes**: Composition over inheritance
//   * trait Display { fn fmt(&self) -> String; }
//   * impl Display for MyType { ... }
//   * Multiple traits per type
//   * No traditional inheritance
//
// - **String vs str**: Two string types
//   * String owned, heap-allocated, growable
//   * &str borrowed, immutable, slice
//   * "literal" is &str
//   * Convert: .to_string() or String::from()
//
// - **Unsafe keyword**: Opt into unsafety
//   * unsafe { raw_pointer_deref() }
//   * Compiler cannot verify safety
//   * Needed for FFI, certain optimizations
//   * Isolated and explicit
//
// - **No implicit conversions**: Must be explicit
//   * Cannot mix i32 and i64 without cast
//   * let x: i64 = y as i64;
//   * Prevents subtle bugs
//
// - **Shadowing**: Redeclare same variable name
//   * let x = 5; let x = x + 1;
//   * Creates new binding
//   * Can change type: let x = 5; let x = "hello";
//
// - **Cargo conventions**: Strong community standards
//   * Snake_case for functions, variables
//   * PascalCase for types, traits
//   * SCREAMING_SNAKE_CASE for constants
//   * Cargo.toml for dependencies
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Rust: A language empowering everyone to build reliable and efficient software" - Official tagline
// - "Fearless concurrency" - Rust's promise on thread safety
// - "If it compiles, it works" - Community saying about correctness
// - "Fighting the borrow checker" - Learning curve experience
// - "Rewrite it in Rust" - Meme about rewriting C/C++ projects
// - "Zero-cost abstractions: You don't pay for what you don't use" - Core principle
// - "Rust makes you think differently about memory" - Developer experience
// - "The compiler is your friend, even when it feels like your enemy" - Learning wisdom
// - "Memory safety without garbage collection" - Key selling point
// - "Most loved programming language" - Stack Overflow surveys 2016-2023
//
// NOTES ON RUST SYNTAX
// --------------------
// - Case-sensitive language
// - Comments: //, ///, /* */
// - Variables: let x = value; (immutable), let mut x = value; (mutable)
// - Constants: const NAME: Type = value;
// - Functions: fn name(params) -> ReturnType { }
// - Structs: struct Name { field: Type }
// - Enums: enum Name { Variant1, Variant2(Type) }
// - Pattern matching: match value { Pattern => result }
// - Ownership: values have unique owner, moved or borrowed
// - Borrowing: &T immutable reference, &mut T mutable reference
// - Lifetimes: 'a lifetime annotations for references
// - Traits: trait Name { fn method(&self); }
// - Implementations: impl TraitName for TypeName { }
// - Generics: fn name<T>(param: T) -> T
// - Macros: macro_name!()
// - Attributes: #[derive(Debug)], #![feature]
// - Result type: Result<T, E> with Ok(T) or Err(E)
// - Option type: Option<T> with Some(T) or None
// - String types: String (owned), &str (borrowed)
// - Vectors: Vec<T> dynamic arrays
// - Arrays: [T; N] fixed-size arrays
// - Tuples: (T1, T2, T3)
// - Closures: |x| x + 1
// - Async/await: async fn, .await
// - Type inference: compiler infers most types
// - Explicit types: let x: i32 = 5;
// - Raw pointers: *const T, *mut T (unsafe)
// - Smart pointers: Box<T>, Rc<T>, Arc<T>
// - Interior mutability: RefCell<T>, Cell<T>


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('rust', {
  rules: [
    // PRIORITY 100: Comments must come first
    // Single-line comments
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    
    // Doc comments (///)
    {
      class: 'comment',
      pattern: /\/\/\/.*$/gm,
      priority: 100
    },
    
    // Multi-line comments
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    // Raw strings (r"..." or r#"..."#)
    {
      class: 'string',
      pattern: /r#*"(?:[^"]|"")*"#*/g,
      priority: 90
    },
    
    // Regular strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // Byte strings (b"...")
    {
      class: 'string',
      pattern: /b"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // Character literals
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)'/g,
      priority: 90
    },
    
    // Byte literals (b'x')
    {
      class: 'string',
      pattern: /b'(?:[^'\\]|\\.)'/g,
      priority: 90
    },
    
    // PRIORITY 80: Attributes and derive macros
    {
      class: 'decorator',
      pattern: /#!?\[[\s\S]*?\]/g,
      priority: 80
    },
    
    // PRIORITY 70: Macros (ends with !)
    {
      class: 'class',
      pattern: /\b([a-z_][a-zA-Z0-9_]*!)(?=\s*[(\[{])/g,
      priority: 70
    },
    
    // PRIORITY 65: Lifetimes (starts with ')
    {
      class: 'decorator',
      pattern: /'[a-z_][a-zA-Z0-9_]*\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Keywords - Control flow
    {
      class: 'keyword',
      pattern: /\b(if|else|match|loop|while|for|break|continue|return|yield|await|async)\b/g,
      priority: 60
    },
    
    // PRIORITY 58: Operators
    {
      class: 'operator',
      pattern: /[+\-*\/%=<>!&|^~?:]+/g,
      priority: 58
    },
    
    // PRIORITY 55: Keywords - Declarations
    {
      class: 'keyword',
      pattern: /\b(fn|let|const|static|struct|enum|trait|impl|type|use|mod|pub|crate|self|Self|super|unsafe|extern|dyn|where|as|move|ref|in)\b/g,
      priority: 55
    },
    
    // PRIORITY 54: Keyword modifiers (different from main keywords)
    {
      class: 'decorator',
      pattern: /\b(mut)\b/g,
      priority: 54
    },
    
    // PRIORITY 52: Variable/constant names in declarations
    // Matches: let name, const NAME, static NAME
    {
      class: 'variable',
      pattern: /\b(?:let|const|static)\s+(?:mut\s+)?([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 52
    },
    
    // PRIORITY 51: Function names in definitions
    // Matches: fn function_name
    {
      class: 'function',
      pattern: /\bfn\s+([a-z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 51
    },
    
    // PRIORITY 50: Primitive types
    {
      class: 'builtin',
      pattern: /\b(i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize|f32|f64|bool|char|str)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Common standard library types
    {
      class: 'builtin',
      pattern: /\b(String|Vec|Box|Rc|Arc|RefCell|Cell|Option|Some|None|Result|Ok|Err|HashMap|HashSet|BTreeMap|BTreeSet|Mutex|RwLock|Path|PathBuf|File|Iterator)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Type names (PascalCase)
    {
      class: 'class',
      pattern: /\b([A-Z][a-zA-Z0-9]*)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Function and method calls
    {
      class: 'function',
      pattern: /\b([a-z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },
    
    // PRIORITY 30: Numbers with suffixes
    {
      class: 'number',
      pattern: /\b\d+\.?\d*(?:_\d+)*(?:[eE][+-]?\d+)?(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize|f32|f64)?\b/g,
      priority: 30
    },
    
    // Hexadecimal
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F_]+(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?\b/g,
      priority: 30
    },
    
    // Binary
    {
      class: 'number',
      pattern: /\b0[bB][01_]+(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?\b/g,
      priority: 30
    },
    
    // Octal
    {
      class: 'number',
      pattern: /\b0[oO][0-7_]+(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?\b/g,
      priority: 30
    },
    
    // PRIORITY 20: Booleans
    {
      class: 'boolean',
      pattern: /\b(true|false)\b/g,
      priority: 20
    }
  ]
});