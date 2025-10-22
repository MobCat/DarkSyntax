// darkSyntax/configs/golang.js - Go language configuration
// =========================================================
// Go (2009)
// Go - Statically typed, compiled language designed at Google
//
// Configs
// =======================
// ALIASES: ['golang', 'go']
// File extensions: .go
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created at Google by Robert Griesemer, Rob Pike, and Ken Thompson (2007)
// - Publicly announced (November 10, 2009)
// - Version 1.0 released (March 28, 2012)
// - Born from frustration with C++ compilation times and complexity
// - Initial design discussions began on September 21, 2007
// - Named "Go" for its simplicity and speed goals
// - Mascot: Gordon the Gopher, designed by Renée French (2009)
// - Self-hosting compiler completed (2008)
// - Go 1 compatibility promise established (2012) - no breaking changes
// - Generics added after 13 years of deliberation (Go 1.18, March 2022)
// - Designed for multicore processors and networked systems
// - Built-in concurrency primitives from day one
// - Response to modern software engineering challenges at scale
// - Influenced Google's infrastructure and cloud services
// - Go modules for dependency management (Go 1.11, 2018)
// - Rapid adoption in cloud-native and DevOps tooling
//
// INFLUENCED
// ----------
// - Docker (2013) - Container platform written in Go
// - Kubernetes (2014) - Container orchestration system
// - Terraform (2014) - Infrastructure as code tool
// - Prometheus (2012) - Monitoring and alerting system
// - etcd (2013) - Distributed key-value store
// - CockroachDB (2014) - Distributed SQL database
// - Hugo (2013) - Static site generator
// - Caddy (2015) - Web server with automatic HTTPS
// - InfluxDB (2013) - Time series database
// - Vault (2015) - Secrets management by HashiCorp
// - Consul (2014) - Service mesh and discovery
// - gRPC (2015) - High-performance RPC framework
// - Cobra (2013) - CLI application framework
// - Gin (2014) - Web framework
// - Echo (2015) - High-performance web framework
// - Rust language design (2010+) - Influenced by Go's simplicity philosophy
// - Swift language (2014) - Type inference and error handling patterns
// - Crystal language (2014) - Go-like syntax with Ruby influence
//
// USED FOR
// --------
// - Cloud infrastructure and services
// - Microservices and distributed systems
// - Container orchestration platforms
// - DevOps and SRE tooling
// - Network servers and proxies
// - Command-line tools and utilities
// - Backend web services and APIs
// - Real-time data processing
// - System administration tools
// - Database systems
// - Monitoring and observability tools
// - CI/CD pipelines and automation
// - Blockchain and cryptocurrency platforms
//
// KEY FEATURES
// ------------
// - Fast compilation times (seconds for large codebases)
// - Built-in concurrency with goroutines and channels
// - Garbage collection with low latency
// - Static typing with type inference
// - Simple, minimal syntax (25 keywords)
// - No classes, inheritance, or generics (until 1.18)
// - Interface-based polymorphism
// - Package-based code organization
// - Cross-platform compilation built-in
// - Comprehensive standard library
// - Opinionated formatting (gofmt)
// - Built-in testing framework
// - First-class functions and closures
// - Multiple return values (especially for error handling)
// - Deferred function execution
// - Composition over inheritance
// - No pointer arithmetic (memory safety)
//
// CORE SYNTAX
// -----------
// Basic program structure:
//   package main
//   
//   import "fmt"
//   
//   func main() {
//       fmt.Println("Hello, World!")
//   }
//
// Goroutines (lightweight threads):
//   go func() {
//       // runs concurrently
//   }()
//
// Channels for communication:
//   ch := make(chan int)
//   ch <- 42          // send
//   value := <-ch     // receive
//
// Error handling:
//   result, err := doSomething()
//   if err != nil {
//       return err
//   }
//
// Structs and methods:
//   type Person struct {
//       Name string
//       Age  int
//   }
//   
//   func (p Person) Greet() string {
//       return "Hello, " + p.Name
//   }
//
// Interfaces:
//   type Writer interface {
//       Write([]byte) (int, error)
//   }
//
// QUIRKS
// ------
// - **Unused imports/variables are compile errors**: Enforces clean code
//   * Cannot leave `import "fmt"` if not used
//   * Cannot declare variable `x` if never referenced
//   * Use `_` blank identifier to ignore values
//
// - **No semicolons required**: Automatic insertion by compiler
//   * Newlines become statement terminators
//   * Opening braces must be on same line as declaration
//   * `if x { }` not `if x\n{ }`
//
// - **Multiple return values**: Especially for error handling
//   * `func div(a, b int) (int, error)`
//   * Idiomatic Go: return result and error
//   * No exceptions, explicit error handling
//
// - **Exported vs unexported**: Determined by capitalization
//   * `PublicFunc` is exported (uppercase first letter)
//   * `privateFunc` is unexported (lowercase first letter)
//   * Applies to functions, types, variables, struct fields
//
// - **Zero values**: All types have default initialization
//   * `int` → 0, `string` → "", `bool` → false
//   * Pointers, slices, maps, channels → nil
//   * Structs: all fields get their zero values
//
// - **No implicit type conversion**: Must be explicit
//   * Cannot mix int and int64 without casting
//   * `var x int = 5; var y int64 = x` → compile error
//   * Must write: `var y int64 = int64(x)`
//
// - **Short variable declaration**: `:=` operator
//   * `x := 5` instead of `var x int = 5`
//   * Only works inside functions
//   * At least one new variable required on left side
//
// - **Defer statement**: Function executes after surrounding function returns
//   * `defer file.Close()` ensures cleanup
//   * Multiple defers execute in LIFO order
//   * Evaluated immediately, executed later
//
// - **No pointer arithmetic**: Unlike C/C++
//   * Cannot do `ptr++` or `ptr + 5`
//   * Pointers for passing references only
//   * Memory safety without sacrificing performance
//
// - **Slice vs array**: Similar syntax, different behavior
//   * Arrays: `[3]int` fixed size, value type
//   * Slices: `[]int` dynamic size, reference type
//   * Slices backed by arrays internally
//
// - **Map initialization**: Must use make or literal
//   * `var m map[string]int` creates nil map (cannot add to it)
//   * `m := make(map[string]int)` creates usable map
//   * Writing to nil map causes panic
//
// - **Goroutines are not threads**: Extremely lightweight
//   * Can have millions of goroutines
//   * Multiplexed onto OS threads by runtime
//   * Stack starts at 2KB, grows as needed
//
// - **No generics until Go 1.18**: Controversial design decision
//   * Required interface{} and type assertions
//   * Code duplication for type-safe collections
//   * Finally added with type parameters (2022)
//
// - **Error is a value**: Not an exception
//   * `if err != nil { }` pattern everywhere
//   * Verbose but explicit
//   * Errors are just values that can be returned
//
// - **Package naming**: Single word, lowercase
//   * Package name matches last element of import path
//   * `import "encoding/json"` → package json
//   * Don't use underscores or mixedCaps
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Go is an attempt to combine the safety and performance of a statically typed compiled language with the expressiveness and convenience of a dynamically typed interpreted language" - Rob Pike
// - "Don't communicate by sharing memory; share memory by communicating" - Go proverb
// - "Errors are values" - Rob Pike, Go Blog (2015)
// - "The bigger the interface, the weaker the abstraction" - Rob Pike
// - "Gofmt's style is no one's favorite, yet gofmt is everyone's favorite" - Rob Pike
// - "Clear is better than clever" - Go proverb
// - "A little copying is better than a little dependency" - Go proverb
// - "Make the zero value useful" - Go proverb
// - "Simplicity is complicated" - Rob Pike
// - "Go will be the server language of the future" - Tobias Lütke, Shopify CEO
//
// NOTES ON GO SYNTAX
// ------------------
// - Case-sensitive language
// - Comments: // (single-line), /* */ (multi-line)
// - Semicolons automatically inserted by compiler
// - Curly braces required for all blocks
// - Package declaration must be first statement
// - Import statements follow package declaration
// - Exported identifiers start with uppercase letter
// - Variable declaration: var name type = value
// - Short declaration: name := value (inside functions only)
// - Multiple return values common
// - Error handling via returned error values
// - Defer for cleanup operations
// - Go routines: go funcName()
// - Channels: chan type, <-ch (receive), ch<- (send)
// - Pointers: &var (address), *ptr (dereference)
// - No pointer arithmetic allowed
// - Interfaces defined by methods only
// - Type assertion: value.(Type)
// - Type switch: switch v := x.(type)
// - Range keyword for iteration
// - Raw string literals use backticks
// - No implicit type conversions


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('golang', {
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
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /`[^`]*`/g,  // Raw strings (backticks)
      priority: 90
    },
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,  // Rune literals
      priority: 90
    },
    
    // PRIORITY 60: Package and import statements
    {
      class: 'keyword',
      pattern: /\b(package|import)\b/g,
      priority: 60
    },
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(break|case|chan|const|continue|default|defer|else|fallthrough|for|func|go|goto|if|interface|map|range|return|select|struct|switch|type|var)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Built-in types
    {
      class: 'builtin',
      pattern: /\b(bool|byte|complex64|complex128|error|float32|float64|int|int8|int16|int32|int64|rune|string|uint|uint8|uint16|uint32|uint64|uintptr|any|comparable)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(append|cap|close|complex|copy|delete|imag|len|make|new|panic|print|println|real|recover)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Constants
    {
      class: 'boolean',
      pattern: /\b(true|false|nil|iota)\b/g,
      priority: 35
    },
    
    // PRIORITY 30: Function declarations and calls
    {
      class: 'function',
      pattern: /\bfunc\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 32
    },
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Method calls (with dot notation)
    {
      class: 'function',
      pattern: /\.([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      captureGroup: 1,
      priority: 25
    },
    
    // PRIORITY 20: Numbers
    {
      class: 'number',
      pattern: /\b0x[0-9A-Fa-f]+\b/g,  // Hexadecimal
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0o[0-7]+\b/g,  // Octal
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0b[01]+\b/g,  // Binary
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?[i]?\b/g,  // Decimal, scientific, imaginary
      priority: 20
    }
  ]
});