// darkSyntax/configs/pascal.js - Pascal language configuration
// =============================================================
// Pascal (1970)
// Pascal - Elegant language for teaching structured programming
//
// Configs
// =======================
// ALIASES: ['pascal', 'pas', 'pp']
// File extensions: .pas, .pascal, .pp
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Niklaus Wirth at ETH Zurich (1968-1970)
// - First compiler completed in 1970 on CDC 6000 mainframe
// - Named after French mathematician Blaise Pascal (1623-1662)
// - Designed explicitly for teaching structured programming
// - Response to perceived complexity of Algol 68
// - User Manual and Report published (1973) - language specification
// - UCSD Pascal system for microcomputers (1978) - revolutionary portability
// - Apple Pascal for Apple II (1979) - brought Pascal to home computers
// - Turbo Pascal by Borland (1983) - blazing fast compilation, IDE innovation
// - Standard Pascal (ISO 7185) ratified (1983)
// - Extended Pascal (ISO 10206) standard (1990)
// - Object Pascal extensions by Apple (1986)
// - Delphi by Borland (1995) - Object Pascal for Windows RAD
// - Free Pascal compiler (1993-present) - open source implementation
// - Lazarus IDE (1999) - open source Delphi alternative
// - Academic standard for CS education (1970s-1990s)
// - Apple Lisa operating system written in Pascal (1983)
// - Early Macintosh development in Pascal (1984)
// - Adobe Photoshop 1.0 written in Pascal (1990)
// - Skype initially written in Delphi (2003)
// - Programming competition standard (USACO, IOI) through 2010s
// - Influenced structured programming methodology worldwide
// - Wirth received Turing Award for Pascal and related work (1984)
// - Declined in popularity post-2000s (replaced by C++, Java in education)
// - Legacy lives on in Delphi/Object Pascal community
//
// INFLUENCED
// ----------
// - Algol 60 (1960) - Syntax foundation, block structure
// - Algol-W (1966) - Wirth's earlier language, direct predecessor
// - PL/I (1964) - Record structures concept
// - Simula 67 (1967) - Early OOP concepts inspiration
// - COBOL (1959) - Readable syntax philosophy
//
// USED FOR
// --------
// - Computer science education and teaching
// - Operating system development (Lisa, parts of early Mac OS)
// - Application development (Turbo Pascal era)
// - Game development (1980s-1990s DOS games)
// - Scientific computing and numerical analysis
// - System utilities and tools
// - Embedded systems (Free Pascal)
// - Windows application development (Delphi)
// - Database applications (Delphi + InterBase/Firebird)
// - Cross-platform desktop apps (Lazarus/Free Pascal)
// - Programming competitions and olympiads
// - Legacy system maintenance
// - Educational software and simulations
// - Business applications (Delphi era)
//
// KEY FEATURES
// ------------
// - Strong static typing with type safety
// - Structured programming constructs (if/then/else, while, for, case)
// - Procedures and functions with local scope
// - Nested procedure declarations
// - Pass by value and pass by reference (var parameters)
// - User-defined types (records, enumerations, subranges, sets)
// - Pointer types with strong typing
// - File handling built into language
// - Block-structured with begin/end delimiters
// - Readable, English-like syntax
// - Compilation to native code (fast execution)
// - No ambiguous grammar (easy to parse)
// - Forward declarations for mutual recursion
// - String handling (implementation-dependent)
// - Set operations (union, intersection, difference)
// - Range checking and overflow detection
// - Units for modular programming (Turbo Pascal+)
// - Object-oriented extensions (Object Pascal, Delphi)
//
// CORE SYNTAX
// -----------
// Basic program structure:
//   program HelloWorld;
//   begin
//     WriteLn('Hello, World!');
//   end.
//
// With declarations:
//   program Example;
//   var
//     x, y: Integer;
//     name: String;
//   begin
//     x := 10;
//     y := 20;
//     WriteLn(x + y);
//   end.
//
// Procedures and functions:
//   procedure Greet(name: String);
//   begin
//     WriteLn('Hello, ', name);
//   end;
//   
//   function Add(a, b: Integer): Integer;
//   begin
//     Add := a + b;
//   end;
//
// Records:
//   type
//     Person = record
//       name: String;
//       age: Integer;
//     end;
//   var
//     p: Person;
//   begin
//     p.name := 'Alice';
//     p.age := 30;
//   end.
//
// QUIRKS
// ------
// - **Case insensitive**: BEGIN = Begin = begin
//   * Convention: PascalCase for types, lowercase for keywords
//   * All keywords work in any case
//   * Identifiers also case-insensitive
//
// - **Assignment operator**: := not =
//   * x := 10 assigns value
//   * = is comparison operator only
//   * Common mistake for C programmers
//
// - **Main program ends with period**: end.
//   * Procedures and functions end with semicolon: end;
//   * Only the main program block gets period
//   * Easy to forget!
//
// - **No return statement**: Assign to function name
//   * function Add(a, b: Integer): Integer;
//   * begin Add := a + b; end;
//   * Must assign result to function name
//   * Different from modern languages
//
// - **Semicolons are separators**: Not terminators
//   * Between statements, not after them
//   * Last statement before end has no semicolon
//   * Though modern Pascal allows it
//
// - **String literals**: Single quotes only
//   * 'This is a string'
//   * "Double quotes not allowed"
//   * Escape single quote by doubling: 'can''t'
//
// - **Strong typing**: No implicit conversions
//   * Cannot mix Integer and Real without conversion
//   * Cannot assign String to Char
//   * Type safety enforced strictly
//
// - **No boolean operators with integers**: Separate types
//   * true and false are Boolean type
//   * Cannot use 0/1 for false/true
//   * Must use actual Boolean values
//
// - **Array indices**: Can start anywhere
//   * array[1..10] starts at 1
//   * array[0..9] starts at 0
//   * array[-5..5] even negative indices
//   * Subrange types enforce bounds
//
// - **Sets are built-in**: First-class data type
//   * type Colors = (Red, Green, Blue);
//   * var ColorSet: set of Colors;
//   * if Red in ColorSet then...
//   * Set operations: +, -, *
//
// - **Pointers use ^**: Dereference and type
//   * type IntPtr = ^Integer;
//   * p^ accesses pointed value
//   * @ takes address
//
// - **Forward declarations**: For mutual recursion
//   * procedure A; forward;
//   * procedure B; begin A; end;
//   * procedure A; begin B; end;
//
// - **Local procedures**: Can nest functions
//   * Procedures inside procedures
//   * Access to outer procedure's variables
//   * Closures before they were cool
//
// - **Ordinal types**: Subrange and enumeration
//   * type Digit = 0..9;
//   * type Day = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
//   * Can use in case statements and for loops
//
// - **With statement**: Controversial feature
//   * with record do field := value;
//   * Avoids repetition of record name
//   * Can cause ambiguity issues
//
// - **No operator overloading**: In standard Pascal
//   * + only works on built-in types
//   * Extended Pascal and Delphi added it
//
// - **File handling built-in**: Part of language
//   * Assign(f, 'file.txt');
//   * Reset(f) opens for reading
//   * Rewrite(f) opens for writing
//   * Read/Write procedures for I/O
//
// - **Comments have two styles**: Curly or paren-star
//   * { comment in braces }
//   * (* comment in parens *)
//   * // single line in modern versions
//   * Cannot nest comments (limitation)
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms" - Alan Perlis, Turing Award lecture (1982)
// - "Pascal is so clean, it's hard to make mistakes" - Niklaus Wirth
// - "The purpose of the language was to teach structured programming" - Niklaus Wirth
// - "Turbo Pascal turned programming into a video game" - Developer describing compilation speed
// - "Real programmers don't use Pascal" - Ed Post satire (1983) - defending Pascal from FORTRAN programmers
// - "Why Pascal is Not My Favorite Programming Language" - Brian Kernighan (1981) - famous critique
// - "Pascal was the language we learned to program in, before we learned C" - Common CS student experience
// - "Borland made Pascal cool again" - Industry sentiment about Turbo Pascal
// - "Delphi was the best kept secret in Windows programming" - Delphi developers
// - "Pascal: where every program looks like it was written by the same person" - Readability observation
//
// NOTES ON PASCAL SYNTAX
// -----------------------
// - Case-insensitive language (BEGIN = begin)
// - Comments: { }, (* *), // (modern)
// - Program structure: program Name; ... begin ... end.
// - Main program ends with end. (period)
// - Procedures/functions end with end; (semicolon)
// - Assignment: := (not =)
// - Comparison: = (not ==)
// - String literals: 'single quotes only'
// - Escape single quote by doubling: 'can''t'
// - Semicolons separate statements (not terminate)
// - Strong static typing
// - No implicit type conversions
// - Block structure with begin/end
// - Declaration sections: const, type, var
// - Procedures: procedure Name(params);
// - Functions: function Name(params): ReturnType;
// - Function result assigned to function name
// - Control structures: if/then/else, case/of, while/do, repeat/until, for/to/do
// - Pointer dereference: p^
// - Address operator: @variable
// - Set membership: element in set
// - Range operator: 1..10
// - Array indices: array[low..high]
// - Record fields: record.field
// - With statement: with record do ...
// - Forward declarations for mutual recursion
// - Nested procedure declarations allowed
// - Units for modular programming (Turbo Pascal+)
// - Compiler directives: {$directive}


darkSyntax.registerLanguage('pascal', {
  rules: [
    // PRIORITY 100: Comments (curly brace style)
    {
      class: 'comment',
      pattern: /\{[^}]*\}/g,
      priority: 100
    },
    
    // PRIORITY 100: Comments (parenthesis-asterisk style)
    {
      class: 'comment',
      pattern: /\(\*[\s\S]*?\*\)/g,
      priority: 100
    },
    
    // PRIORITY 100: Single-line comments (modern Pascal)
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings (single quotes only)
    {
      class: 'string',
      pattern: /'(?:[^']|'')*'/g,
      priority: 90
    },
    
    // PRIORITY 80: Compiler directives
    {
      class: 'decorator',
      pattern: /\{\$[^}]*\}/gi,
      priority: 80
    },
    
    // PRIORITY 60: Program structure keywords
    {
      class: 'keyword',
      pattern: /\b(program|unit|library|uses|interface|implementation|initialization|finalization)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: Declaration keywords
    {
      class: 'keyword',
      pattern: /\b(const|type|var|label|resourcestring|threadvar)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Control flow keywords
    {
      class: 'keyword',
      pattern: /\b(begin|end|if|then|else|case|of|repeat|until|while|do|for|to|downto|with|goto|break|continue|exit)\b/gi,
      priority: 50
    },
    
    // PRIORITY 50: Procedure/function keywords
    {
      class: 'keyword',
      pattern: /\b(procedure|function|constructor|destructor|operator|property)\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: Type keywords
    {
      class: 'builtin',
      pattern: /\b(Integer|Real|Boolean|Char|String|Byte|Word|LongInt|LongWord|Int64|Single|Double|Extended|Currency|Pointer|Text|File|Array|Record|Set|Packed)\b/gi,
      priority: 45
    },
    
    // PRIORITY 45: Modifiers
    {
      class: 'keyword',
      pattern: /\b(private|protected|public|published|automated|strict|forward|external|far|near|absolute|abstract|virtual|override|overload|reintroduce|inline|cdecl|pascal|register|safecall|stdcall)\b/gi,
      priority: 45
    },
    
    // PRIORITY 42: Range operator (must come before relational operators)
    {
      class: 'keyword',
      pattern: /\.\./g,
      priority: 42
    },
    
    // PRIORITY 41: Relational operators
    {
      class: 'keyword',
      pattern: /<>|<=|>=|[=<>]/g,
      priority: 41
    },
    
    // PRIORITY 40: Arithmetic operators
    {
      class: 'keyword',
      pattern: /[+\-*/]/g,
      priority: 40
    },
    
    // PRIORITY 40: Logical operators
    {
      class: 'keyword',
      pattern: /\b(and|or|not|xor|shl|shr|div|mod|in|is|as)\b/gi,
      priority: 40
    },
    
    // PRIORITY 40: Assignment operator
    {
      class: 'keyword',
      pattern: /:=/g,
      priority: 40
    },
    
    // PRIORITY 39: Record field access (dot operator)
    {
      class: 'keyword',
      pattern: /\./g,
      priority: 39
    },
    
    // PRIORITY 35: Built-in functions and procedures
    {
      class: 'builtin',
      pattern: /\b(Write|WriteLn|Read|ReadLn|New|Dispose|Inc|Dec|Succ|Pred|Ord|Chr|Length|Copy|Pos|Val|Str|Abs|Sqr|Sqrt|Sin|Cos|Arctan|Ln|Exp|Trunc|Round|Random|Randomize|Assign|Reset|Rewrite|Close|Eof|Eoln|Get|Put|Seek|FilePos|FileSize|Flush|Erase|Rename|BlockRead|BlockWrite)\b/gi,
      priority: 35
    },
    
    // PRIORITY 30: Function/procedure calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Pointer dereference
    {
      class: 'keyword',
      pattern: /\^/g,
      priority: 25
    },
    
    // PRIORITY 20: Numbers (hex and decimal, but not before range operator)
    {
      class: 'number',
      pattern: /\$[0-9a-fA-F]+|\b\d+\.\d+([eE][+-]?\d+)?|\b\d+(?!\.(?!\.))/g,
      priority: 20
    },
    
    // PRIORITY 15: Boolean literals
    {
      class: 'boolean',
      pattern: /\b(True|False|nil)\b/gi,
      priority: 15
    }
  ]
});