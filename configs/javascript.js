// darkSyntax/configs/javascript.js - JavaScript language configuration
// ======================================================================
// JavaScript (1995)
// JavaScript - The language of the web
//
// Configs
// =======================
// ALIASES: ['javascript', 'js', 'mjs', 'cjs', 'jsx']
// File extensions: .js, .mjs, .cjs
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Brendan Eich at Netscape Communications (May 1995)
// - Developed in just 10 days (May 6-15, 1995)
// - Originally named "Mocha", then "LiveScript", finally "JavaScript"
// - Name chosen to ride Java's popularity wave (marketing decision)
// - First appeared in Netscape Navigator 2.0 beta (September 1995)
// - Microsoft reverse-engineered as "JScript" for IE 3.0 (August 1996)
// - ECMAScript standardization began (November 1996)
// - ECMAScript 1 (ES1) first standard (June 1997)
// - ECMAScript 2 (ES2) editorial changes (June 1998)
// - ECMAScript 3 (ES3) regex, try/catch (December 1999)
// - ECMAScript 4 (ES4) abandoned due to disagreements (2008)
// - ECMAScript 5 (ES5) strict mode, JSON (December 2009)
// - ECMAScript 6 (ES6/ES2015) massive update (June 2015)
// - Yearly releases established: ES2016, ES2017, ES2018, etc.
// - AJAX revolutionized web apps (2005, coined by Jesse James Garrett)
// - jQuery simplified DOM manipulation (2006, John Resig)
// - Node.js brought JavaScript to servers (2009, Ryan Dahl)
// - V8 engine by Google made JS blazing fast (2008)
// - npm package manager launched (2010, Isaac Z. Schlueter)
// - Angular, React, Vue popularized component frameworks (2010s)
// - TypeScript added static typing (2012, Microsoft)
// - Deno runtime as Node.js successor (2020, Ryan Dahl)
// - Bun ultra-fast runtime (2022, Jarred Sumner)
// - Most widely deployed programming language ever
// - Runs in every web browser worldwide
// - GitHub's most popular language by repositories
//
// INFLUENCED
// ----------
// - Java (1995) - Syntax inspiration (marketing naming)
// - Scheme (1975) - First-class functions, closures
// - Self (1987) - Prototype-based inheritance
// - Perl (1987) - Regular expressions
// - Python (1991) - Generator functions, list comprehensions influence
// - Lua (1993) - Lightweight embedding philosophy
// - AWK (1977) - Text processing patterns
// - HyperTalk (1987) - Scripting for non-programmers
//
// USED FOR
// --------
// - Front-end web development (browsers)
// - Back-end server development (Node.js, Deno, Bun)
// - Mobile app development (React Native, Ionic, NativeScript)
// - Desktop applications (Electron, Tauri, NW.js)
// - Game development (Phaser, Three.js, Babylon.js)
// - Browser extensions and add-ons
// - Command-line tools and scripts
// - Serverless functions (AWS Lambda, Cloudflare Workers)
// - IoT and embedded devices (Johnny-Five, Espruino)
// - Machine learning (TensorFlow.js, Brain.js)
// - Data visualization (D3.js, Chart.js, Plotly)
// - Animation and creative coding (p5.js, anime.js)
// - Testing and automation (Puppeteer, Playwright, Cypress)
// - Build tools and bundlers (Webpack, Rollup, Vite, esbuild)
// - Database queries (MongoDB, Firebase)
// - Real-time applications (WebSockets, Socket.io)
//
// KEY FEATURES
// ------------
// - Dynamically typed language (types determined at runtime)
// - Prototype-based object-oriented programming
// - First-class functions (functions as values)
// - Closures for data encapsulation
// - Event-driven and asynchronous programming
// - Promises and async/await for async code
// - Arrow functions for concise syntax
// - Template literals for string interpolation
// - Destructuring for arrays and objects
// - Spread and rest operators
// - Classes with syntactic sugar (ES6+)
// - Modules for code organization (ES6+)
// - Garbage collection (automatic memory management)
// - Dynamic imports for code splitting
// - Optional chaining (?.) and nullish coalescing (??)
// - Private class fields (#privateField)
// - No compilation required (interpreted/JIT)
// - Single-threaded with event loop
// - JSON native support
// - Regular expressions built-in
//
// CORE SYNTAX
// -----------
// Variables and types:
//   let count = 42;
//   const name = "JavaScript";
//   var old = "legacy";
//
// Functions:
//   function greet(name) {
//     return `Hello, ${name}!`;
//   }
//   
//   const arrow = (x) => x * 2;
//
// Objects and arrays:
//   const person = {
//     name: "Alice",
//     age: 30,
//     greet() { console.log("Hi!"); }
//   };
//   
//   const numbers = [1, 2, 3, 4, 5];
//
// Classes (ES6+):
//   class Animal {
//     constructor(name) {
//       this.name = name;
//     }
//     speak() {
//       console.log(`${this.name} makes a sound`);
//     }
//   }
//
// Async/await:
//   async function fetchData() {
//     const response = await fetch(url);
//     return await response.json();
//   }
//
// QUIRKS
// ------
// - **Not Java**: Despite the name
//   * Completely different languages
//   * Named JavaScript for marketing (1995)
//   * "Java is to JavaScript what car is to carpet"
//
// - **Automatic semicolon insertion**: Optional semicolons
//   * JavaScript inserts semicolons automatically
//   * Can cause subtle bugs
//   * Endless style debates (semicolons vs no semicolons)
//
// - **Type coercion**: Implicit type conversion
//   * `"5" + 3` = "53" (string concatenation)
//   * `"5" - 3` = 2 (number subtraction)
//   * `[] + []` = "" (empty string)
//   * `[] + {}` = "[object Object]"
//   * `{} + []` = 0 (in some contexts)
//   * Use `===` instead of `==` for strict equality
//
// - **`this` keyword**: Context-dependent
//   * Value depends on how function is called
//   * Arrow functions don't have own `this`
//   * `.bind()`, `.call()`, `.apply()` change context
//   * Source of countless bugs
//
// - **Truthy and falsy**: Not just boolean
//   * Falsy: false, 0, "", null, undefined, NaN
//   * Everything else is truthy (including "0", "false", [], {})
//   * `if (value)` checks truthiness, not boolean
//
// - **var vs let vs const**: Scope differences
//   * `var` has function scope (hoisted)
//   * `let` and `const` have block scope
//   * `const` doesn't mean immutable (objects can change)
//   * Modern code uses `const` by default
//
// - **Hoisting**: Declarations moved to top
//   * `var` declarations hoisted (not initializations)
//   * Function declarations fully hoisted
//   * `let` and `const` in "temporal dead zone"
//   * Can use function before declaration
//
// - **No block scope for var**: Until let/const
//   * `for (var i = 0; i < 5; i++)` leaks `i`
//   * Classic closure in loop problem
//   * Fixed with `let` in ES6
//
// - **Floating point precision**: Binary math issues
//   * `0.1 + 0.2` = 0.30000000000000004
//   * Common to all IEEE 754 languages
//   * Use libraries for precise decimal math
//
// - **Array holes**: Sparse arrays allowed
//   * `[1, , 3]` has hole at index 1
//   * Different from `[1, undefined, 3]`
//   * .map() skips holes, .forEach() too
//
// - **typeof null**: Returns "object"
//   * `typeof null` = "object" (famous bug)
//   * Too late to fix (would break web)
//   * Use `value === null` to check
//
// - **NaN weirdness**: Not equal to itself
//   * `NaN === NaN` is false
//   * Use `Number.isNaN()` to check
//   * `typeof NaN` = "number"
//
// - **Global scope pollution**: Easy to create globals
//   * Forgot `var`/`let`/`const`? Global variable
//   * `window` object in browsers
//   * `global` in Node.js
//   * Strict mode helps prevent this
//
// - **Event loop**: Single-threaded concurrency
//   * Call stack, callback queue, microtask queue
//   * Promises vs setTimeout priority
//   * Can block UI if code runs too long
//
// - **Callback hell**: Nested callbacks
//   * Pyramid of doom before Promises
//   * Promises improved this (2015)
//   * async/await made it even better (2017)
//
// - **Array/Object methods**: Not available on null/undefined
//   * `null.toString()` throws error
//   * Optional chaining helps: `obj?.method()`
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Any application that can be written in JavaScript, will eventually be written in JavaScript" - Jeff Atwood's Law (2007)
// - "JavaScript: The World's Most Misunderstood Programming Language" - Douglas Crockford
// - "I created JavaScript in 10 days. I had to" - Brendan Eich
// - "JavaScript is the only language that I'm aware of that people feel they don't need to learn before they start using it" - Douglas Crockford
// - "Java is to JavaScript what car is to carpet" - Chris Heilmann
// - "JavaScript is a great language for beginners because it forgives you, and a terrible language for the same reason" - Anonymous
// - "There are only two kinds of languages: the ones people complain about and the ones nobody uses" - Bjarne Stroustrup (applies to JavaScript)
// - "JavaScript is the duct tape of the Internet" - Charlie Campbell
// - "I think JavaScript is the assembly language of the web" - Scott Hanselman
// - "JavaScript: It's not just for annoying popups anymore!" - Developer joke
//
// NOTES ON JAVASCRIPT SYNTAX
// ---------------------------
// - Case-sensitive language
// - Comments: // (single-line), /* */ (multi-line)
// - Semicolons optional (but recommended by some style guides)
// - Variables: var (function scope), let (block scope), const (block scope, immutable binding)
// - Data types: number, string, boolean, object, undefined, null, symbol, bigint
// - Template literals: `Hello ${name}` with backticks
// - Arrow functions: (args) => expression
// - Destructuring: const {x, y} = obj or const [a, b] = arr
// - Spread operator: ...array or ...object
// - Rest parameters: function(...args)
// - Default parameters: function(x = 10)
// - Object shorthand: {name} instead of {name: name}
// - Computed property names: {[key]: value}
// - Optional chaining: obj?.prop?.method?.()
// - Nullish coalescing: value ?? defaultValue
// - Classes: class Name extends Parent { }
// - Modules: import/export (ES6) or require/module.exports (CommonJS)
// - Promises: new Promise((resolve, reject) => { })
// - async/await: async function() { await promise; }
// - Try/catch for error handling
// - Regular expressions: /pattern/flags
// - JSON: JSON.parse() and JSON.stringify()
// - Strict mode: "use strict";
// - Event listeners: addEventListener()
// - Prototypal inheritance: Object.create()
// - Private class fields: #privateField (ES2022)


darkSyntax.registerLanguage('javascript', {
  rules: [
    // PRIORITY 100: Comments (MUST be first)
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
    
    // PRIORITY 95: Regex literals (BEFORE strings!)
    // This is critical - regex patterns like /pattern/g must be caught before strings
    {
      class: 'string',
      pattern: /\/(?![*/])(?:[^\\/\n]|\\.)+\/[gimsuvy]*/g,
      priority: 95
    },
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /`(?:[^`\\]|\\.|\$\{[^}]*\})*`/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,  // Added \n to prevent cross-line matching
      priority: 90
    },
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(const|let|var|function|return|if|else|for|while|do|switch|case|break|continue|class|extends|import|export|from|as|default|async|await|try|catch|finally|throw|new|typeof|instanceof|delete|in|of|yield|static|super|this|void|with|debugger)\b/g,
      priority: 50
    },
    
    // PRIORITY 40: Built-in objects and functions
    {
      class: 'builtin',
      pattern: /\b(console|document|window|Math|Object|Array|String|Number|Boolean|Date|RegExp|Promise|Set|Map|WeakMap|WeakSet|Symbol|Proxy|Reflect|JSON|parseInt|parseFloat|isNaN|isFinite|encodeURI|decodeURI|encodeURIComponent|decodeURIComponent|eval|setTimeout|setInterval|clearTimeout|clearInterval)\b/g,
      priority: 40
    },
    
    // PRIORITY 30: Function names
    {
      class: 'function',
      pattern: /\b([a-zA-Z_$][a-zA-Z0-9_$]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 20: Numbers
    {
      class: 'number',
      pattern: /\b(?:0[xX][0-9a-fA-F]+|0[bB][01]+|0[oO][0-7]+|\d+\.?\d*(?:[eE][+-]?\d+)?)\b/g,
      priority: 20
    },
    
    // PRIORITY 20: Boolean and null
    {
      class: 'boolean',
      pattern: /\b(true|false|null|undefined|NaN|Infinity)\b/g,
      priority: 20
    }
  ]
});