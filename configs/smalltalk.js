// darkSyntax/configs/smalltalk.js - Smalltalk language configuration
// ====================================================================
// Smalltalk (1972)
//
// Configs
// =======================
// ALIASES: ['smalltalk', 'st', 'sm']
// File extensions: .st, .sm
//
// SMALLTALK SYNTAX NOTES
// ======================
//
// Smalltalk is the first pure object-oriented programming language, where
// literally EVERYTHING is an object - even true, false, and classes themselves.
// Created at Xerox PARC, it pioneered concepts that define modern computing.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Alan Kay, Dan Ingalls, Adele Goldberg at Xerox PARC (1972)
// - First PURE object-oriented language (everything is an object)
// - Alan Kay won a bet that he could define "the most powerful language
//   in the world" in "a page of code"
// - Invented: GUI, windowed interfaces, MVC pattern, live coding, debugger,
//   class browser, hot-swapping code
// - Influenced the Mac, Windows, modern IDEs, and OOP languages
// - Possibly the first "white space" language.
//   As your segments are on a new line but tabbed over from the messageName.
//
// INFLUENCED
// ----------
// - Objective-C (1984) - Apple's main language before Swift
// - Ruby (1995) - Took Smalltalk's elegance
// - Java (1995) - Simplified Smalltalk for the masses
// - Python (1991) - Object model inspired by Smalltalk
// - C# (2000) - Many OOP concepts from Smalltalk
// - Modern IDEs and debuggers - All inspired by Smalltalk tools
//
// USED FOR
// --------
// - Education (original purpose - for kids!)
// - Business applications (especially financial systems)
// - Prototyping and rapid development
// - Research in object-oriented design
// - Live programming environments
//
// THE BIG IDEA: MESSAGING
// -----------------------
// Alan Kay: "The big idea is 'messaging' - that is what the kernel of
// Smalltalk is all about."
//
// Everything is done by sending messages to objects:
// - 5 + 3       sends message + with argument 3 to object 5
// - array size  sends message size to array object
// - true ifTrue: [code]  sends message ifTrue: to boolean object true
//
// EVERYTHING IS AN OBJECT
// -----------------------
// - Numbers: 5, 3.14 are objects
// - Booleans: true, false are objects
// - Classes: String, Array are objects (instances of Metaclass)
// - Blocks: [x + 1] are objects (closures)
// - nil: the null object
//
// MESSAGE TYPES
// -------------
// 1. Unary: receiver message
//    Examples: array size, number negated
//
// 2. Binary: receiver symbol argument
//    Examples: 3 + 4, 10 * 5, x @ y (point creation)
//
// 3. Keyword: receiver keyword: arg keyword: arg
//    Examples: array at: 1 put: 'hello'
//
// MESSAGE PRECEDENCE (no arithmetic precedence!)
// -----------------------------------------------
// 1. Unary messages (highest)
// 2. Binary messages
// 3. Keyword messages (lowest)
// 4. Use parentheses to override: 3 + (4 * 5)
//
// Note: 3 + 4 * 5 = 35 in Smalltalk! (not 23 like in math)
// It evaluates left to right: (3 + 4) * 5
//
// SYNTAX ELEMENTS
// ---------------
// := Assignment
// ^ Return
// . Statement separator (period)
// ; Cascade (send multiple messages to same object)
// | | Local variable declaration
// [ ] Block (closure)
// # Symbol literal
// ' ' String literal
// " " Comment
//
// BLOCKS (CLOSURES)
// -----------------
// [code] - A block of code, executable later
// [:x | x + 1] - Block with parameter
// [:x :y | x + y] - Block with multiple parameters
// Blocks are first-class objects!
//
// CONTROL FLOW (via messages!)
// ----------------------------
// true ifTrue: [code]
// false ifFalse: [code]
// condition ifTrue: [code] ifFalse: [code]
// [condition] whileTrue: [code]
// 1 to: 10 do: [:i | code]
//
// CLASSES
// -------
// Object subclass: #ClassName
//   instanceVariableNames: 'var1 var2'
//   classVariableNames: ''
//   category: 'MyCategory'
//
// METHODS
// -------
// messageName
//   "Comment"
//   | temp1 temp2 |
//   statements.
//   ^result

darkSyntax.registerLanguage('smalltalk', {
  rules: [
    // PRIORITY 100: Comments (double quotes)
    {
      class: 'comment',
      pattern: /"[^"]*"/g,
      priority: 100
    },
    
    // PRIORITY 90: Strings (single quotes)
    {
      class: 'string',
      pattern: /'(?:[^']|'')*'/g,
      priority: 90
    },
    
    // PRIORITY 85: Symbols (start with #, like #mySymbol)
    {
      class: 'string',
      pattern: /#[a-zA-Z][a-zA-Z0-9_]*/g,
      priority: 85
    },
    
    // PRIORITY 80: Array literals (#(1 2 3))
    {
      class: 'string',
      pattern: /#\([^)]*\)/g,
      priority: 80
    },
    
    // PRIORITY 75: Return operator
    {
      class: 'keyword',
      pattern: /\^/g,
      priority: 75
    },
    
    // PRIORITY 70: Assignment operator
    {
      class: 'keyword',
      pattern: /:=/g,
      priority: 70
    },
    
    // PRIORITY 65: Block parameters
    {
      class: 'function',
      pattern: /:\s*([a-zA-Z][a-zA-Z0-9_]*)/g,
      priority: 65,
      captureGroup: 1
    },
    
    // PRIORITY 60: Keywords (control flow via messages)
    {
      class: 'keyword',
      pattern: /\b(ifTrue:|ifFalse:|ifNil:|ifNotNil:|whileTrue:|whileFalse:|timesRepeat:|to:do:|do:|select:|collect:|inject:into:|detect:|value:|value:value:|new|self|super|thisContext)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Class definition keywords
    {
      class: 'keyword',
      pattern: /\b(subclass:|instanceVariableNames:|classVariableNames:|category:)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Built-in classes
    {
      class: 'builtin',
      pattern: /\b(Object|Behavior|Class|Metaclass|BlockClosure|Boolean|True|False|UndefinedObject|Number|Integer|Float|Fraction|SmallInteger|LargeInteger|Character|String|Symbol|Collection|Array|OrderedCollection|Set|Dictionary|Interval|Stream|Transcript|Point|Rectangle)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Common messages/methods
    {
      class: 'builtin',
      pattern: /\b(size|at:|at:put:|first|last|isEmpty|notEmpty|includes:|add:|remove:|do:|select:|reject:|collect:|detect:|inject:into:|new|initialize|value|value:|printString|asString|asUppercase|asLowercase|reversed|sqrt|squared|abs|negated|even|odd|cr|show:|nextPutAll:)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Message sends (identifier followed by colon)
    {
      class: 'function',
      pattern: /\b([a-z][a-zA-Z0-9_]*):/g,
      priority: 40,
      captureGroup: 1
    },
    
    // PRIORITY 35: Unary message sends
    {
      class: 'function',
      pattern: /\b([a-z][a-zA-Z0-9_]*)\b(?!\s*:)/g,
      priority: 35
    },
    
    // PRIORITY 30: Class names (start with uppercase)
    {
      class: 'builtin',
      pattern: /\b[A-Z][a-zA-Z0-9_]*/g,
      priority: 30
    },
    
    // PRIORITY 25: Binary operators (Smalltalk style)
    {
      class: 'keyword',
      pattern: /[+\-*\/<>=@|&~,\\]+/g,
      priority: 25
    },
    
    // PRIORITY 20: Numbers
    {
      class: 'number',
      pattern: /\b\d+r[0-9A-Za-z]+|\b\d+\.?\d*([eE][+-]?\d+)?/g,
      priority: 20
    },
    
    // PRIORITY 15: Special literals
    {
      class: 'boolean',
      pattern: /\b(true|false|nil)\b/g,
      priority: 15
    },
    
    // PRIORITY 15: Pseudo-variables
    {
      class: 'keyword',
      pattern: /\b(self|super|thisContext)\b/g,
      priority: 15
    }
  ]
});