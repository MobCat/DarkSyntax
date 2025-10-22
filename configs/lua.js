// darkSyntax/configs/lua.js - Lua configuration
// ================================================
// Lua (1993)
// Lightweight, embeddable scripting language designed for extension
//
// Configs
// =======================
// ALIASES: ['lua']
// File extensions: .lua
//
// .lua - Lua source files
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created at PUC-Rio, Brazil (1993) by Roberto Ierusalimschy, Waldemar Celes, Luiz Henrique de Figueiredo
// - Lua 5.0 (2003) - Coroutines added, major API redesign
// - Lua 5.1 (2006) - Module system, most widely adopted version
// - LuaJIT created (2005) - One of fastest dynamic language implementations
// - Lua 5.2 (2011) - Environments replaced with _ENV, bitwise operators
// - Lua 5.3 (2015) - Integer subtype, bitwise operators in language
// - Lua 5.4 (2020) - To-be-closed variables, generational GC
// - Most embedded scripting language in games and applications
//
// INFLUENCED
// ----------
// - JavaScript (1995) - Prototype-based objects via tables
// - Io (2002) - Minimal syntax, prototype objects
// - MoonScript (2011) - Compiles to Lua with CoffeeScript-like syntax
// - Terra (2013) - Low-level companion to Lua for systems programming
// - Wren (2013) - Embeddable language inspired by Lua's simplicity
//
// USED FOR
// --------
// - Game scripting (World of Warcraft, Roblox, Angry Birds, Grim Fandango)
// - Embedded systems (routers, set-top boxes, IoT devices)
// - Application extension (Adobe Lightroom, VLC, Wireshark, Neovim)
// - Configuration files (Redis, nginx, Haproxy)
// - Game engines (LÖVE, Corona SDK, Defold)
// - Network programming (OpenResty, Kong API Gateway)
//
// KEY FEATURES
// ------------
// - Extremely small and fast (~280KB compiled)
// - Only one data structure: tables (used for arrays, objects, modules)
// - First-class functions with closures
// - Coroutines for cooperative multitasking
// - Automatic memory management with incremental GC
// - Simple C API for embedding (push/pop stack model)
// - Dynamically typed with no classes (prototype-based via metatables)
//
// CORE SYNTAX
// -----------
// - Tables are everything: arrays, objects, modules, classes
//   * Arrays: {1, 2, 3} (1-indexed!)
//   * Objects: {name = "Alice", age = 30}
//   * Mixed: {10, 20, x = 5, y = 10}
// - Functions: function name(params) ... end
// - Local variables: local x = 5 (default is global!)
// - Multiple assignment: a, b = 1, 2
// - Table access: obj.field or obj["field"]
// - Method calls: obj:method(args) -- syntactic sugar for obj.method(obj, args)
// - Comments: -- single line, --[[ multi-line ]]
// - String concatenation: ".." operator
//
// QUIRKS
// ------
// - **Arrays are 1-indexed, not 0-indexed**:
//   * t = {10, 20, 30} → t[1] == 10, t[0] is nil
//   * Historical decision from Lua's origins
//   * Most controversial Lua feature among programmers from other languages
//   * #t returns length correctly for 1-indexed arrays
// - **Variables are global by default**:
//   * x = 5 creates a global variable (!)
//   * local x = 5 creates a local variable
//   * Easy to accidentally pollute global scope
//   * Best practice: always use 'local' unless you need global
// - **Only nil and false are falsy**:
//   * 0 is truthy (like Ruby, unlike C/JavaScript)
//   * Empty string "" is truthy
//   * Empty table {} is truthy
// - **No continue statement**:
//   * Only break, no continue in loops (until Lua 5.2 added goto)
//   * Must use goto or restructure code
// - **No switch/case statement**:
//   * Must use if/elseif chains or table lookups
// - **Table length operator # is unreliable with holes**:
//   * t = {1, nil, 3} → #t is undefined behavior
//   * Only works correctly on "sequences" (no nil holes)
// - **Module system changed between versions**:
//   * Lua 5.0: globals and tables
//   * Lua 5.1: module() function (now deprecated)
//   * Lua 5.2+: return table from file
// - **No integer type until Lua 5.3**:
//   * All numbers were floats in Lua 5.2 and earlier
//   * Lua 5.3+ has integer subtype (still number type)
// - **Metamethods control object behavior**:
//   * __index, __newindex, __add, __call, etc.
//   * Can make tables behave like objects, arrays, functions
//   * Basis for OOP in Lua
// - **Colon vs dot for methods**:
//   * obj:method(x) is sugar for obj.method(obj, x)
//   * Passes object as implicit 'self' parameter
//   * Easy to forget : vs . and get confusing errors
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Lua is not Lisp" - Common misunderstanding due to similar names
// - "Lua" means "moon" in Portuguese - named after previous language "Sol" (sun)
// - "The power of a language is not in what it lets you do, but in what it helps you avoid doing" - Roberto Ierusalimschy
// - "Tables all the way down" - Everything is tables in Lua
//
// NOTES ON LUA SYNTAX
// -------------------------
// - Comments: -- for single line, --[[ ]] for multi-line
// - Strings: 'single', "double", or [[multi-line]]
// - String concatenation uses .. operator
// - Not equal: ~= (not !=)
// - Length operator: # (e.g., #table, #string)
// - Varargs in functions: ... (three dots)
// - Case-sensitive language
// - Semicolons are optional (rarely used)
// - Blocks end with 'end' keyword


// LUA SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================
darkSyntax.registerLanguage('lua', {
  rules: [
    // Multi-line comments --[[ ]]
    {
      class: 'comment',
      pattern: /--\[\[[\s\S]*?\]\]/g,
      priority: 100
    },

    // Single-line comments
    {
      class: 'comment',
      pattern: /--.*$/gm,
      priority: 100
    },

    // Multi-line strings [[ ]]
    {
      class: 'string',
      pattern: /\[\[[\s\S]*?\]\]/g,
      priority: 91
    },

    // Long bracket strings [=[ ]=] (can have multiple = signs)
    {
      class: 'string',
      pattern: /\[=+\[[\s\S]*?\]=+\]/g,
      priority: 91
    },

    // Double-quoted strings
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"/g,
      priority: 90
    },

    // Single-quoted strings
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Keywords (removed true, false, nil - those are booleans)
    {
      class: 'keyword',
      pattern: /\b(and|break|do|else|elseif|end|for|function|goto|if|in|local|not|or|repeat|return|then|until|while)\b/g,
      priority: 50
    },

    // Built-in functions and libraries
    {
      class: 'builtin',
      pattern: /\b(assert|collectgarbage|dofile|error|getmetatable|ipairs|load|loadfile|next|pairs|pcall|print|rawequal|rawget|rawlen|rawset|require|select|setmetatable|tonumber|tostring|type|xpcall|_G|_VERSION|coroutine|debug|io|math|os|package|string|table|utf8)\b/g,
      priority: 45
    },

    // Function definitions
    {
      class: 'function',
      pattern: /\b(function)\s+([a-zA-Z_][a-zA-Z0-9_]*(?:[.:]?[a-zA-Z_][a-zA-Z0-9_]*)*)/g,
      captureGroup: 2,
      priority: 40
    },

    // Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=[({])/g,
      priority: 35
    },

    // Method calls with : or . (only when followed by parentheses)
    {
      class: 'function',
      pattern: /[:.]([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      captureGroup: 1,
      priority: 34
    },

    // Boolean and nil (already in keywords, but lower priority for emphasis)
    {
      class: 'boolean',
      pattern: /\b(true|false|nil)\b/g,
      priority: 25
    },

    // Numbers (hex, decimal, scientific)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?\b/g,
      priority: 21
    },

    // Operators
    {
      class: 'operator',
      pattern: /(?:\.\.\.?|==|~=|<=|>=|<<|>>|\/\/|[+\-*\/%^#<>=])/g,
      priority: 58
    }
  ]
});