// darkSyntax/configs/typescript.js - TypeScript language configuration
// ======================================================================
// TypeScript (2012)
// JavaScript that scales - Static typing for the dynamic web
//
// Configs
// =======================
// ALIASES: ['typescript', 'ts', 'tsx']
// File extensions: .ts, .tsx
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Anders Hejlsberg at Microsoft (2012)
// - Announced publicly on October 1, 2012
// - Version 1.0 released April 2, 2014
// - Hejlsberg previously created Turbo Pascal (1983) and designed C# (2000)
// - Addressed JavaScript's scalability problems for large applications
// - First major statically-typed superset of JavaScript
// - Compiler written in TypeScript itself (self-hosting from version 0.9)
// - Open-sourced under Apache 2.0 license on CodePlex, later moved to GitHub
// - Angular 2+ adopted TypeScript as primary language (2015)
// - ECMAScript proposal influence: private fields, optional chaining (2019-2020)
// - GitHub Copilot training data heavily weighted TypeScript (2021)
// - Over 50% of npm packages now include TypeScript definitions (2023)
// - Became the 4th most loved language on Stack Overflow (2020-2024)
//
// INFLUENCED
// ----------
// - Flow (2014) - Facebook's competing type system for JavaScript
// - JSDoc (enhanced 2015+) - Type annotations in comments gained TypeScript-compatible syntax
// - Dart (enhanced 2018) - Sound null safety influenced by TypeScript's strictNullChecks
// - Python type hints (2014+) - Optional static typing model inspired by TypeScript
// - PHP (2020) - Union types and mixed type influenced by TypeScript
// - JavaScript/ECMAScript - Private fields (#), optional chaining (?.), nullish coalescing (??)
// - Kotlin (enhanced) - TypeScript's success validated optional typing approaches
// - ReScript (2020) - Type inference patterns from TypeScript ecosystem
//
// USED FOR
// --------
// - Large-scale web applications and SPAs
// - Enterprise JavaScript development
// - React, Angular, Vue.js application development
// - Node.js backend services and APIs
// - Desktop applications (Electron, VS Code)
// - Mobile development (React Native, Ionic)
// - Game development (Phaser, Three.js projects)
// - Browser extensions and Chrome apps
// - CLI tools and build systems
// - Library and framework development with .d.ts type definitions
//
// KEY FEATURES
// ------------
// - Optional static type system (gradually typed)
// - Structural type system (duck typing with types)
// - Type inference - types inferred without explicit annotations
// - Interfaces and type aliases for complex shapes
// - Generics with constraints and defaults
// - Union and intersection types
// - Literal types and template literal types
// - Type guards and type predicates
// - Utility types (Partial, Pick, Omit, Record, etc.)
// - Decorators for metaprogramming
// - Enums for named constants
// - Tuple types with labeled elements
// - Mapped types and conditional types
// - Strict null checking
// - Compiles to clean, readable JavaScript
// - Source maps for debugging
// - Watch mode for rapid development
//
// CORE SYNTAX
// -----------
// Type annotations:
//   let name: string = "Alice";
//   let age: number = 30;
//   let active: boolean = true;
//
// Interfaces:
//   interface User {
//     name: string;
//     age: number;
//     email?: string;  // Optional property
//   }
//
// Type aliases:
//   type ID = string | number;
//   type Point = { x: number; y: number };
//
// Generics:
//   function identity<T>(arg: T): T {
//     return arg;
//   }
//
// Union and intersection:
//   type StringOrNumber = string | number;
//   type Named = { name: string } & { id: number };
//
// Classes with access modifiers:
//   class Person {
//     private age: number;
//     public name: string;
//     protected id: string;
//   }
//
// QUIRKS
// ------
// - **Structural typing**: Types are compatible based on shape, not name
//   * class Dog { bark() {} } and { bark: () => void } are compatible
//   * Different from nominal typing in Java/C#
// - **any type escape hatch**: Disables type checking entirely
//   * let x: any = 5; x.foo.bar.baz(); // No errors!
//   * Necessary for migration but defeats the purpose
// - **Type widening**: Literals widen to their general type
//   * let x = "hello"; // Type is string, not "hello"
//   * Use 'as const' to prevent: let x = "hello" as const;
// - **Bivariance in methods**: Methods are bivariant by default for compatibility
//   * Animal[] can accept Dog[] even though it's unsound
//   * strictFunctionTypes flag makes it safer
// - **Enums are real objects**: Unlike most TypeScript features, enums generate runtime code
//   * enum Color { Red, Green, Blue } becomes actual JavaScript object
//   * Use const enum for compile-time-only enums
// - **No true private**: TypeScript private is compile-time only
//   * private fields accessible via bracket notation at runtime
//   * Use # prefix for true JavaScript private fields
// - **Module resolution complexity**: Classic vs Node vs Bundler strategies
//   * Path mapping with tsconfig.json can be confusing
//   * Different behavior in --module configurations
// - **Declaration merging**: Interfaces with same name merge automatically
//   * interface User { name: string } + interface User { age: number } = both properties
//   * Powerful but can be surprising
// - **Type assertions**: Override type checker with 'as'
//   * let x = foo as string; // "Trust me, I know better"
//   * Dangerous but sometimes necessary for complex types
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "TypeScript is JavaScript that scales" - Anders Hejlsberg
// - "Any application that can be written in JavaScript, will eventually be written in JavaScript. Then rewritten in TypeScript." - Atwood's Law, extended
// - "I used to think TypeScript was just 'JavaScript for people who can't code.' Then I worked on a 100k line codebase." - Common developer revelation
// - "TypeScript: Making JavaScript developers argue about types since 2012" - Community joke
// - "The best thing about TypeScript is IntelliSense. The worst thing about TypeScript is also IntelliSense." - On autocomplete overload
//
// NOTES ON TYPESCRIPT SYNTAX
// --------------------------
// - Superset of JavaScript - all JS code is valid TS
// - Type annotations use colon syntax: variable: Type
// - Interfaces define object shapes
// - Generic types use angle brackets: Array<T>, Promise<string>
// - Optional properties use ?: syntax
// - Non-null assertion operator: value!
// - Type guards with typeof, instanceof, in
// - Union types with | (pipe), intersection with & (ampersand)
// - Decorators use @ symbol (experimental)
// - Triple-slash directives for compiler hints: /// <reference path="..." />
// - JSX/TSX uses .tsx extension
// - Comments: // single-line, /* multi-line */
// - Template literals with backticks for strings
// - Arrow functions: () => {}
// - Async/await for promises


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('typescript', {
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
    
    // PRIORITY 90: Strings (before keywords)
    {
      class: 'string',
      pattern: /`(?:[^`\\]|\\.)*`/g,
      priority: 90
    },
    
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
    
    // PRIORITY 85: Regular expressions
    {
      class: 'string',
      pattern: /\/(?![*/])(?:[^/\\\[\n]|\\.|\[(?:[^\]\\\n]|\\.)*\])+\/[gimyus]*/g,
      priority: 85
    },
    
    // PRIORITY 70: Decorators
    {
      class: 'decorator',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 70
    },
    
    // PRIORITY 65: Class/interface/type definitions
    {
      class: 'class',
      pattern: /\b(?:class|interface|type|enum)\s+([A-Z][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 65
    },
    
    // PRIORITY 60: TypeScript-specific keywords
    {
      class: 'keyword',
      pattern: /\b(interface|type|enum|namespace|module|declare|abstract|implements|extends|public|private|protected|readonly|static|async|await|as|keyof|typeof|infer|is|asserts)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: JavaScript keywords
    {
      class: 'keyword',
      pattern: /\b(break|case|catch|class|const|continue|debugger|default|delete|do|else|export|finally|for|function|if|import|in|instanceof|let|new|return|super|switch|this|throw|try|var|void|while|with|yield)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Operators (modern language)
    {
      class: 'operator',
      pattern: /[+\-*\/%=<>!&|^~?:]+|=>|\.\.\./g,
      priority: 50
    },
    
    // PRIORITY 45: TypeScript built-in types
    {
      class: 'builtin',
      pattern: /\b(string|number|boolean|any|unknown|never|void|null|undefined|object|symbol|bigint)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Built-in objects and utility types
    {
      class: 'builtin',
      pattern: /\b(Array|Promise|Map|Set|WeakMap|WeakSet|Date|RegExp|Error|Object|String|Number|Boolean|Function|Symbol|Math|JSON|console|window|document|Partial|Required|Readonly|Record|Pick|Omit|Exclude|Extract|NonNullable|ReturnType|InstanceType|Parameters|Awaited)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Function definitions
    {
      class: 'function',
      pattern: /\bfunction\s+([a-zA-Z_$][a-zA-Z0-9_$]*)/g,
      captureGroup: 1,
      priority: 35
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_$][a-zA-Z0-9_$]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Numbers (all formats)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[eE]?[+-]?\d*n?\b/g,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+n?\b/g,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b0[bB][01]+n?\b/g,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b0[oO][0-7]+n?\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Booleans and special values
    {
      class: 'boolean',
      pattern: /\b(true|false|null|undefined|NaN|Infinity)\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Type parameters in generics
    {
      class: 'class',
      pattern: /(?<=<)\s*[A-Z][a-zA-Z0-9_]*(?:\s+extends\s+[a-zA-Z0-9_]+)?/g,
      priority: 15
    }
  ]
});