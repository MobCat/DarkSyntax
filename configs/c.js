// darkSyntax/configs/c.js - C language configuration
// ====================================================
// C (1973)
//
// Configs
// =======================
// ALIASES: ['c']
// File extensions: .c, .h
//
// C LANGUAGE SYNTAX NOTES
// =======================
//
// C was created by Dennis Ritchie at Bell Labs between 1969-1973 for developing
// the Unix operating system. It became one of the most influential programming
// languages ever created, forming the foundation for countless modern languages.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Dennis Ritchie at Bell Labs (1972)
// - Developed to rewrite Unix (previously in assembly)
// - K&R C published in "The C Programming Language" (1978)
// - ANSI C (C89/C90) standardized in 1989
// - ISO C99 added major features in 1999
// - C11 added concurrency and safety features in 2011
// - C17/C18 bug fixes in 2017/2018
// - C23 latest standard (2023)
//
// INFLUENCED
// ----------
// - C++ (1985) - Object-oriented extension
// - Objective-C (1984) - OOP for NeXT/macOS
// - Java (1995) - Syntax heavily inspired by C
// - C# (2000) - Microsoft's answer to Java
// - JavaScript (1995) - Named to ride C/Java popularity
// - Go (2009) - Modern systems language
// - Rust (2010) - Memory-safe systems language
// - Almost every modern imperative language
//
// USED TO WRITE
// -------------
// - Unix, Linux, BSD (operating systems)
// - Windows NT kernel
// - macOS/iOS kernel (XNU)
// - Python interpreter (CPython)
// - Git version control
// - Redis, PostgreSQL, MySQL
// - DOOM, Quake (game engines)
// - Most embedded systems and device drivers
//
// CASE SENSITIVITY
// ----------------
// C is case-sensitive! main, Main, and MAIN are different identifiers.
//
// PROGRAM STRUCTURE
// -----------------
// #include <stdio.h>
//
// int main(void) {
//     printf("Hello, World!\n");
//     return 0;
// }
//
// MEMORY MANAGEMENT
// -----------------
// Manual memory management with malloc/calloc/realloc/free
// No garbage collection - programmer must manage memory
// Pointers are powerful but dangerous
//
// DATA TYPES
// ----------
// Basic: char, int, short, long, float, double
// Modifiers: signed, unsigned, const, volatile
// Derived: arrays, pointers, structures, unions, enumerations
// void: represents absence of type
//
// C99 ADDITIONS
// -------------
// - // single-line comments
// - inline functions
// - bool type (via stdbool.h)
// - Variable-length arrays
// - Designated initializers
// - Compound literals
// - Variable declarations anywhere in block
//
// C11 ADDITIONS
// -------------
// - _Atomic for lock-free programming
// - _Generic for type-generic macros
// - _Static_assert for compile-time assertions
// - _Thread_local for thread-local storage
// - Anonymous structures and unions
//
// PREPROCESSOR
// ------------
// #include - Include header files
// #define - Define macros and constants
// #ifdef, #ifndef, #endif - Conditional compilation
// #if, #elif, #else - Complex conditionals
// #pragma - Compiler-specific directives
// ## - Token pasting
// # - Stringification
//
// OPERATORS
// ---------
// Arithmetic: +, -, *, /, %
// Relational: ==, !=, <, <=, >, >=
// Logical: &&, ||, !
// Bitwise: &, |, ^, ~, <<, >>
// Assignment: =, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
// Increment/Decrement: ++, --
// Pointer: *, &, ->
// Sizeof: sizeof
// Ternary: ? :
//
// COMMENTS
// --------
// /* Multi-line comment (C89) */
// // Single-line comment (C99+)
//
// STRINGS
// -------
// "Double quotes for strings"
// 'Single quotes for chars'
// Escape sequences: \n, \t, \r, \\, \", \'


darkSyntax.registerLanguage('c', {
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
    
    // PRIORITY 95: Header file names in includes
    {
      class: 'variable',
      pattern: /(?<=#include\s*)<[^>]+>/g,
      priority: 95
    },
    {
      class: 'variable',
      pattern: /(?<=#include\s*)"[^"]+"/g,
      priority: 95
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
    
    // PRIORITY 60: Preprocessor directives (just the # and keyword)
    {
      class: 'decorator',
      pattern: /#\s*[a-zA-Z]+/g,
      priority: 60
    },
    
    // PRIORITY 50: C Keywords (no C++ keywords like class, namespace, etc.)
    {
      class: 'keyword',
      pattern: /\b(auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|inline|int|long|register|restrict|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while|_Alignas|_Alignof|_Atomic|_Bool|_Complex|_Generic|_Imaginary|_Noreturn|_Static_assert|_Thread_local)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Common typedef'd types (project-specific and common)
    {
      class: 'class',
      pattern: /\b(boolean|byte|BOOL|BYTE|WORD|DWORD|INT8|INT16|INT32|INT64|UINT8|UINT16|UINT32|UINT64)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Standard C library functions and types
    {
      class: 'builtin',
      pattern: /\b(printf|scanf|fprintf|fscanf|sprintf|sscanf|fopen|fclose|fread|fwrite|fgets|fputs|malloc|calloc|realloc|free|memcpy|memset|memmove|strlen|strcpy|strncpy|strcmp|strncmp|strcat|strncat|size_t|FILE|NULL|EOF|stdin|stdout|stderr|int8_t|int16_t|int32_t|int64_t|uint8_t|uint16_t|uint32_t|uint64_t|bool|true|false)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Pointer operator
    {
      class: 'keyword',
      pattern: /\*/g,
      priority: 35
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 20: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[fFlLuU]?\b/g,
      priority: 20
    },
    
    // PRIORITY 20: Boolean-like constants (if using stdbool.h)
    {
      class: 'boolean',
      pattern: /\b(true|false|NULL)\b/g,
      priority: 20
    }
  ]
});