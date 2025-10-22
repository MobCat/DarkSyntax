// darkSyntax/configs/zig.js - Zig language configuration
// ============================================================
// Zig (2016)
// Zig - A general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software. Designed by Andrew Kelley.
//
// Configs
// =======================
// ALIASES: ['zig']
// File extensions: .zig
//
// LANGUAGE SYNTAX NOTES
// =====================
// - Explicit error handling (`!T`, `catch`, `try`).
// - Compile-time execution (`comptime`).
// - Structs, enums, unions, interfaces.
// - Pointers (`*`, `*const`, `*mut`).
// - Built-in functions prefixed with `@`.
// - Uses `snake_case` for variables/functions, `PascalCase` for types.
// - Supports raw string literals (e.g., `\\abc`, `c"abc"`).
// - Single-line comments (`//`), multi-line comments (`// NOTE:`).
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - 2016: Andrew Kelley begins development of Zig.
// - Focus on being an "alternative to C" with modern features and safety.
// - Its build system is also a language, allowing for powerful build scripts in Zig itself.
// - Gained traction for its unique approach to C interoperability and explicit control.
// - Designed for bootstrapping, aiming to be entirely self-hosted.
//
// INFLUENCED
// ----------
// - Rust (2010) - Focus on systems programming, compile-time guarantees, though Zig takes a different approach to safety.
// - C (1972) - Direct interoperability, low-level control.
// - Go (2009) - Simplicity and clarity of syntax in some aspects.
// - Forth (1970) - Compile-time execution (`comptime`) has parallels to Forth's extensibility at compile time.
//
// USED FOR
// --------
// - Systems programming (operating systems, embedded systems).
// - Game development.
// - Cross-platform development.
// - Tooling and build systems (especially for C/C++ projects).
// - WebAssembly development.
//
// KEY FEATURES
// ------------
// - **`comptime`**: Powerful compile-time execution for generics, reflection, and code generation.
// - **Explicit Error Handling**: Errors are values, enforced at compile-time.
// - **Optional Types (`?T`)**: Nullable types are explicit.
// - **Safety with Control**: Unsafe operations are explicitly marked (`@ptrCast`, `asm`).
// - **C Interoperability**: First-class support for linking with C libraries without FFI overhead.
// - **Allocator Agnosticism**: Standard library doesn't allocate, requiring explicit allocators.
//
// CORE SYNTAX
// -----------
// `const myVar: u32 = 10;`
// `fn add(a: i32, b: i32) i32 { return a + b; }`
// `var x: ?u32 = null;`
// `if (condition) {} else {}`
// `for (array) |item| {}`
// `const MyStruct = struct { a: i32, b: f32 };`
// `const E = error { OutOfMemory, InvalidArg };`
// `@import("foo.zig")`
// `try doSomething() catch |err| logError(err);`
//
// QUIRKS
// ------
// - **Explicit Allocators**: The standard library never allocates; users must provide allocators.
//   * Example: `var gpa = std.heap.GeneralPurposeAllocator(.{}){};`
// - **`comptime` Everywhere**: Many features rely on `comptime` to generate code, which can feel unusual for newcomers.
//   * Example: `const S = comptime std.fmt.format("S is {}", .{"struct"});`
// - **`defer` and `errdefer`**: Ensure resources are cleaned up, but require careful placement.
//   * Example: `defer file.close();`
// - **No Default Integer Sizes**: Integers are explicitly sized (e.g., `u8`, `i32`).
// - **No Null/Nil**: Instead uses `null` for optional types and `undefined` for uninitialized values.
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Zig: The Language that Wants to Replace C, but with a Friendlier Face" - Various tech articles
// - "Optimality, Safety, and Readability" - Core tenets of Zig's design.
// - "Avoid hidden control flow and hidden allocations" - Key design principle.
//
// NOTES ON LANGUAGE SYNTAX
// -------------------------
// - `fn` keyword for function definition.
// - `const` and `var` for immutables and mutables.
// - `struct`, `enum`, `union` for aggregate types.
// - `error` keyword to define error sets.
// - `@` prefix for built-in functions and directives.
// - `!` suffix for error unions (`!T`).
// - `?` prefix for optional types (`?T`).


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('zig', {
  rules: [
    // COMMENTS (Highest priority)
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },

    // STRINGS
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g, // Standard double-quoted string with escapes
      priority: 90
    },
    {
      class: 'string',
      pattern: /c"(?:[^"\\]|\\.)*"/g, // C-compatible string literal
      priority: 90
    },
    // Not directly a string, but raw byte arrays are similar and often green in themes
    {
      class: 'string', // Could also be 'number' or custom. String is reasonable.
      pattern: /\\x[0-9a-fA-F]{2}/g, // Hex byte escapes like \x00
      priority: 90
    },
    {
      class: 'string',
      pattern: /\\u\{[0-9a-fA-F]{1,6}\}/g, // Unicode escapes
      priority: 90
    },

    // DECORATORS / BUILT-IN FUNCTIONS (e.g., @import, @add, @intToPtr)
    {
      class: 'decorator', // Using decorator as it modifies the surrounding code/data
      pattern: /@([a-zA-Z_]\w*)/g, // Catches @import, @add, @ptrCast etc.
      captureGroup: 0, // Highlight the whole thing including @
      priority: 70
    },

    // KEYWORDS (e.g., fn, const, var, if, for, while, return, struct, enum, union, error)
    {
      class: 'keyword',
      pattern: /\b(fn|const|var|pub|extern|linksection|threadlocal|nakedcc|volatile|allowzero|align|asm|break|continue|defer|errdefer|export|callconv|if|else|switch|for|while|return|suspend|resume|try|catch|orelse|comptime|struct|enum|union|error|test|promise|async|await|noasync|undefined|unreachable|and|or|not)\b/g,
      priority: 50
    },

    // TYPE KEYWORDS / PRIMITIVE TYPES (e.g., u8, i32, f64, bool, void, anyerror)
    {
      class: 'class', // Using 'class' as these are fundamental types/declarations
      pattern: /\b(u[1-9][0-9]?|i[1-9][0-9]?|f16|f32|f64|bool|void|anyerror|type|slice|pointer|vector)\b/g,
      priority: 60 // Higher than regular keywords for type clarity
    },

    // BOOLEAN/NULL values (true, false, null)
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 25
    },

    // NUMBERS (Decimal, Hex, Binary, Octal, Scientific Notation)
    {
      class: 'number',
      pattern: /\b0x[0-9a-fA-F_]+\b|\b0b[01_]+\b|\b0o[0-7_]+\b|\b\d[\d_]*(\.[\d_]*)?([eE][+-]?\d[\d_]*)?\b/g,
      priority: 20
    },

    // OPERATORS (Arithmetic, Assignment, Logical, Bitwise, Type Modifiers)
    {
      class: 'operator',
      pattern: /[+\-*\/%=<>&|^~?!:.]+?|!|=|\.\.\.|\.\.|\?\*|->|=>/g, // Catches most operators and type modifiers
      priority: 58
    },
    // Specific for error unions and optional types
    {
      class: 'operator',
      pattern: /(!|\?)[a-zA-Z_]\w*/g, // !MyErrorType, ?MyOptionalType
      captureGroup: 1, // Only highlights the ! or ?
      priority: 59
    },
    {
      class: 'operator',
      pattern: /\*[a-zA-Z_]\w*|\*const|\*mut/g, // Pointers: *T, *const T, *mut T
      captureGroup: 0,
      priority: 59
    },


    // FUNCTIONS (User-defined function names and calls)
    // Needs to be lower priority than keywords/builtins/types.
    // Looks for `foo(`, `.` followed by `foo`.
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g, // Function calls
      captureGroup: 1,
      priority: 30
    },
    {
      class: 'function',
      pattern: /\.([a-zA-Z_][a-zA-Z0-9_]*)\b/g, // Method/field access (obj.method or obj.field)
      captureGroup: 1, // Only highlight the method/field name
      priority: 30
    },
    // This is a complex one, sometimes function names in fn declarations
    {
      class: 'function',
      pattern: /\bfn\s+([a-zA-Z_][a-zA-Z0-9_]*)/g, // Function definition (fn name)
      captureGroup: 1,
      priority: 35
    },

    // CLASS / STRUCT / ENUM / ERROR / UNION DEFINITION NAMES
    // Catches `struct MyStruct {`, `enum MyEnum {`, `error MyError {`
    {
      class: 'class', // Uses 'class' for defining types
      pattern: /\b(struct|enum|union|error|interface)\s+([a-zA-Z_][a-zA-Z0-9_]*)\b/g,
      captureGroup: 2,
      priority: 65 // Higher than `type` class
    },

    // VARIABLES / IDENTIFIERS (General fall-through for names)
    // This should catch anything not covered by more specific rules.
    {
      class: 'variable',
      pattern: /\b[a-zA-Z_][a-zA-Z0-9_]*\b/g,
      priority: 10
    }
  ]
});