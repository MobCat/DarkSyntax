// darkSyntax/configs/cpp.js - C++ language configuration
// =======================================================
// C++ (1985)
// C++ - Object-oriented extension of C
//
// Configs
// =======================
// ALIASES: ['cpp', 'c++', 'cc', 'cxx', 'hpp', 'hxx']
// File extensions: .cpp, .cc, .cxx, .h, .hpp, .hxx
//
// C++ SYNTAX NOTES
// ================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// C++ (1985):
// - Created by Bjarne Stroustrup at Bell Labs
// - Originally called "C with Classes" (1979)
// - First commercial release: 1985
// - Standardized: C++98, C++03, C++11, C++14, C++17, C++20, C++23
// - Backwards compatible with C (mostly)
// - Added OOP, templates, exceptions, STL to C
//
// INFLUENCED
// ----------
// - Java (1995) - Simpler OOP, no manual memory management
// - C# (2000) - Microsoft's answer to Java
// - D (2001) - Modern systems programming
// - Rust (2010) - Memory safety without garbage collection
// - Almost every modern OOP language
//
// USED FOR
// --------
// - Operating systems (Windows, macOS parts)
// - Game engines (Unreal, Unity core)
// - Browsers (Chrome, Firefox)
// - Databases (MySQL, MongoDB)
// - Graphics (Adobe products)
// - High-performance computing
// - Embedded systems
//
// KEY FEATURES
// ------------
// - Object-oriented programming (classes, inheritance, polymorphism)
// - Templates (generic programming)
// - Operator overloading
// - Multiple inheritance
// - RAII (Resource Acquisition Is Initialization)
// - Standard Template Library (STL)
// - Manual memory management (new/delete)
// - References in addition to pointers
// - Function overloading
// - Namespaces
//
// CORE SYNTAX
// -----------
// Classes:
//   class MyClass {
//   public:
//     int value;
//     void method();
//   };
//
// Templates:
//   template<typename T>
//   T max(T a, T b) { return (a > b) ? a : b; }
//
// Namespaces:
//   namespace MyNamespace {
//     void function() { }
//   }
//
// References:
//   int& ref = x;  // Reference to x
//
// Pointers:
//   int* ptr = &x;  // Pointer to x
//
// STL Containers:
//   std::vector<int> v;
//   std::map<string, int> m;
//
// QUIRKS
// ------
// - Header files (.h/.hpp) separate from implementation (.cpp)
// - Multiple inheritance can cause "diamond problem"
// - No garbage collection - manual memory management
// - Templates generate code at compile time (can bloat binaries)
// - Undefined behavior is common (buffer overflows, null pointers)
// - Compilation is slow compared to modern languages


// C++ SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================
darkSyntax.registerLanguage('cpp', {
  rules: [
    // PRIORITY 100: Comments must come first
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
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 90
    },
    
    // PRIORITY 70: Preprocessor directives
    {
      class: 'decorator',
      pattern: /#\s*[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 70
    },
    
    // PRIORITY 65: Class/type definitions (must be before function definitions)
    {
      class: 'class',
      pattern: /\b(class|struct|enum|union|typename)\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 2,
      priority: 65
    },
    
    // PRIORITY 60: Keywords
    {
      class: 'keyword',
      pattern: /\b(alignas|alignof|and|and_eq|asm|auto|bitand|bitor|break|case|catch|concept|const|consteval|constexpr|constinit|const_cast|continue|co_await|co_return|co_yield|decltype|default|delete|do|dynamic_cast|else|explicit|export|extern|for|friend|goto|if|inline|mutable|namespace|new|noexcept|not|not_eq|operator|or|or_eq|private|protected|public|register|reinterpret_cast|requires|return|signed|sizeof|static|static_assert|static_cast|switch|template|this|thread_local|throw|try|typedef|typeid|union|unsigned|using|virtual|volatile|while|xor|xor_eq|override|final)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Type keywords
    {
      class: 'keyword',
      pattern: /\b(bool|char|char8_t|char16_t|char32_t|wchar_t|short|int|long|float|double|void|class|struct|enum|typename)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Built-in types and standard library
    {
      class: 'builtin',
      pattern: /\b(std|string|vector|map|set|list|queue|stack|deque|array|pair|tuple|cout|cin|cerr|endl|size_t|ptrdiff_t|nullptr_t|uint8_t|uint16_t|uint32_t|uint64_t|int8_t|int16_t|int32_t|int64_t)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Function definitions and calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 45
    },
    
    // PRIORITY 40: Member access (object.method or object->method)
    {
      class: 'function',
      pattern: /(?:\.|->)([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 40
    },
    
    // PRIORITY 30: Numbers
    {
      class: 'number',
      pattern: /\b0[xX][0-9A-Fa-f]+[uUlL]*\b/g,  // Hex
      priority: 30
    },
    {
      class: 'number',
      pattern: /\b0[bB][01]+[uUlL]*\b/g,  // Binary (C++14)
      priority: 30
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?[fFlLuU]*\b/g,  // Decimal
      priority: 25
    },
    
    // PRIORITY 20: Booleans and null
    {
      class: 'boolean',
      pattern: /\b(true|false|nullptr|NULL)\b/g,
      priority: 20
    }
  ]
});