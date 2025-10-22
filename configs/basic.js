// darkSyntax/configs/basic.js - BASIC language configuration
// ===========================================================
// BASIC - Beginner's All-purpose Symbolic Instruction Code (1964)
// Multi-dialect configuration supporting classic BASIC through QuickBASIC
//
// Configs
// =======================
// ALIASES: ['basic', 'bas', 'qbasic', 'quickbasic']
// File extensions: .bas, .basic, .qb
// Supports: Dartmouth BASIC, Microsoft BASIC, QuickBASIC, GW-BASIC
//
// BASIC SYNTAX NOTES
// ==================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by John Kemeny and Thomas Kurtz at Dartmouth College (1964)
// - First run at 4 a.m. on May 1, 1964, in College Hall basement
// - Designed for Dartmouth Time-Sharing System (DTSS) - first successful time-sharing
// - Revolutionary goal: "Every student should have access to a computer"
// - Made computing accessible to non-STEM students for first time
// - By 1968, 80% of Dartmouth undergraduates could write BASIC programs
// - Influenced by FORTRAN but designed to be much simpler
// - Compiler-based at Dartmouth (unlike later interpreted versions)
// - IEEE Milestone: Recognized as transformative innovation in computing (2014)
// - Made free and public domain to encourage widespread adoption
//
// INFLUENCED
// ----------
// - Altair BASIC (1975) - Bill Gates' first Microsoft product
// - Microsoft BASIC (1975-1991) - Dominated home computers
// - Commodore BASIC (1977) - Built into Commodore 64, VIC-20
// - Applesoft BASIC (1977) - Standard on Apple II
// - IBM BASICA (1981) - Shipped with IBM PC
// - GW-BASIC (1983) - Bundled with MS-DOS
// - QuickBASIC (1985) - Compiled BASIC with IDE
// - QBasic (1991) - Interpreter bundled with MS-DOS 5.0+
// - Visual Basic (1991) - GUI development revolution
// - VBA (1993) - Macros in Microsoft Office
// - Visual Basic .NET (2002) - Modern .NET language
// - QB64 (2007) - Modern QBasic compiler
//
// USED FOR
// --------
// - Education: Teaching programming fundamentals since 1964
// - Home computing: Primary language on 1970s-1980s microcomputers
// - Business applications: VB dominated corporate development in 1990s
// - Scientific computing: Early Dartmouth use for calculations
// - Game development: Hobbyist game creation (QuickBASIC era)
// - Office automation: VBA macros still widely used today
// - Rapid prototyping: Quick utility creation
//
// KEY FEATURES
// ------------
// - English-like syntax: PRINT, INPUT, IF...THEN, FOR...NEXT
// - Immediate execution: Type code, press RUN, see results instantly
// - Line numbers (classic): 10 PRINT "HELLO", 20 GOTO 10
// - No line numbers (modern): Structured programming with labels
// - Type suffixes: $ (string), % (integer), # (double), ! (single), & (long)
// - Built-in functions: Math (SIN, COS), strings (LEFT$, MID$)
// - Easy I/O: PRINT and INPUT for simple interaction
// - Case insensitive: PRINT = print = Print
// - Array support: DIM A(100) for arrays
// - File operations: OPEN, CLOSE, READ, WRITE
//
// CORE SYNTAX
// -----------
// Classic BASIC (1964-1970s):
//   10 PRINT "HELLO, WORLD!"
//   20 INPUT "YOUR NAME"; N$
//   30 FOR I = 1 TO 10
//   40 PRINT I * 2
//   50 NEXT I
//   60 GOTO 10
//
// Modern BASIC (QuickBASIC style):
//   PRINT "HELLO, WORLD!"
//   INPUT "YOUR NAME: ", name$
//   FOR i = 1 TO 10
//       PRINT i * 2
//   NEXT i
//
// Structured programming:
//   SUB MySub(x%, y%)
//       PRINT x% + y%
//   END SUB
//
//   FUNCTION Square#(n#)
//       Square# = n# * n#
//   END FUNCTION
//
// QUIRKS
// ------
// - **The GOTO wars**: Dijkstra's famous 1968 paper "Go To Statement Considered Harmful"
//   * Classic BASIC relied heavily on GOTO and line numbers
//   * Later versions added structured programming to avoid this
//   * Still a contentious topic among programmers
// - **Boolean True = -1**: Unlike most languages where True = 1
//   * True is all ones in binary (two's complement -1)
//   * False is all zeros (0)
//   * This allows bitwise operations: NOT 0 = -1 (True)
// - **Type suffix madness**: Variable types determined by last character
//   * A$ is string, A% is integer, A# is double, A! is single, A& is long
//   * Easy to mistype: A$ vs A% are completely different variables!
// - **Implicit variables**: No declaration needed in classic BASIC
//   * Just use a variable name and it exists
//   * Easy for beginners, nightmare for debugging
// - **Line number gaps**: Tradition was to number by 10s (10, 20, 30...)
//   * Left room to insert lines later (15 goes between 10 and 20)
//   * RENUMBER command would fix gaps
// - **Dialect hell**: Hundreds of incompatible BASIC versions
//   * Each computer had its own dialect (Apple, Commodore, Atari, etc.)
//   * Graphics commands, sound, and memory access all different
//   * "Write once, debug everywhere"
// - **The = operator**: Used for both assignment AND comparison
//   * LET X = 5 assigns 5 to X
//   * IF X = 5 THEN... compares X to 5
//   * Context determines meaning
// - **Integer division surprise**: 5/2 = 2.5, but 5\2 = 2 (integer)
//   * \ operator does integer division
//   * / operator does floating-point division
// - **String space allocation**: DIM A$ * 50 allocates 50-character string
//   * Fixed-length strings in some dialects
//   * Variable-length in others
// - **Array base confusion**: Arrays start at 0 or 1?
//   * DIM A(10) gives 11 elements (0-10) in some BASICs
//   * OPTION BASE 0 or OPTION BASE 1 to change
// - **Shared vs local**: Variables are global by default
//   * SUB and FUNCTION can have local variables with DIM
//   * SHARED keyword makes globals accessible
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Our vision was that every student on campus should have access to a computer" - John Kemeny
// - "BASIC is to programming what Lego is to building" - Unknown
// - "Go To Statement Considered Harmful" - Edsger Dijkstra (1968)
// - "Microsoft BASIC had hundreds of thousands of users. How are you going to argue with that?" - Don Estridge (IBM)
// - "It's important for our country's future" - Barack Obama on learning to code (2013, echoing BASIC's mission)
//
// NOTES ON BASIC SYNTAX
// ---------------------
// - Case-insensitive language
// - Line numbers optional in modern BASIC variants
// - REM or ' for comments
// - Type suffixes: $ (string), % (integer), & (long), ! (single), # (double)
// - GOTO/GOSUB for control flow (classic style)
// - IF...THEN...ELSE control structure
// - FOR...NEXT loops
// - Built-in functions often end with $ for string functions
// - Supports hexadecimal (&H) and octal (&O) literals


darkSyntax.registerLanguage('basic', {
  rules: [
    // PRIORITY 100: Comments
    // REM comments (classic BASIC)
    {
      class: 'comment',
      pattern: /\bREM\b.*$/gim,
      priority: 100
    },
    
    // Apostrophe comments (QuickBASIC style)
    {
      class: 'comment',
      pattern: /'.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    // Double-quoted strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 80: Line numbers (classic BASIC)
    {
      class: 'decorator',
      pattern: /^\s*\d+\s+/gm,
      priority: 80
    },
    
    // PRIORITY 70: Labels (for GOTO/GOSUB)
    {
      class: 'decorator',
      pattern: /\b([A-Za-z_][A-Za-z0-9_]*):(?!\=)/g,
      captureGroup: 1,
      priority: 70
    },
    
    // PRIORITY 60: Keywords - Control flow
    {
      class: 'keyword',
      pattern: /\b(IF|THEN|ELSE|ELSEIF|ENDIF|FOR|TO|STEP|NEXT|WHILE|WEND|DO|LOOP|UNTIL|SELECT|CASE|GOTO|GOSUB|RETURN|ON|EXIT|END)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: Keywords - I/O and operations
    {
      class: 'keyword',
      pattern: /\b(PRINT|INPUT|READ|DATA|RESTORE|CLS|LOCATE|COLOR|BEEP|SOUND|PLAY|OPEN|CLOSE|GET|PUT|LINE|CIRCLE|PSET|SCREEN|WIDTH)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Keywords - Declarations and definitions
    {
      class: 'keyword',
      pattern: /\b(DIM|REDIM|COMMON|SHARED|STATIC|CONST|DEF|SUB|FUNCTION|DECLARE|TYPE|AS|LET)\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: Data types
    {
      class: 'builtin',
      pattern: /\b(INTEGER|LONG|SINGLE|DOUBLE|STRING|BYTE|BOOLEAN|VARIANT|AS\s+INTEGER|AS\s+LONG|AS\s+SINGLE|AS\s+DOUBLE|AS\s+STRING)\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(ABS|ASC|ATN|CHR\$|COS|EXP|FIX|INT|LEFT\$|LEN|LOG|MID\$|RIGHT\$|RND|SGN|SIN|SQR|STR\$|TAN|VAL|INKEY\$|TIMER|UCASE\$|LCASE\$|LTRIM\$|RTRIM\$|SPACE\$|STRING\$|INSTR|EOF|LOF|POS|CSRLIN)\b/gi,
      priority: 40
    },
    
    // PRIORITY 35: Function and subroutine calls
    {
      class: 'function',
      pattern: /\b([A-Za-z_][A-Za-z0-9_]*)\s*(?=\()/gi,
      priority: 35
    },
    
    // PRIORITY 30: Variables with type suffixes ($, %, &, !, #)
    {
      class: 'variable',
      pattern: /\b([A-Za-z_][A-Za-z0-9_]*)[$%&!#]/g,
      priority: 30
    },
    
    // PRIORITY 25: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[E][+-]?\d+\b/gi,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 25
    },
    
    // Hexadecimal (&H)
    {
      class: 'number',
      pattern: /&H[0-9A-F]+/gi,
      priority: 25
    },
    
    // Octal (&O)
    {
      class: 'number',
      pattern: /&O[0-7]+/gi,
      priority: 25
    },
    
    // PRIORITY 20: Logical operators and constants
    {
      class: 'keyword',
      pattern: /\b(AND|OR|NOT|XOR|EQV|IMP|MOD)\b/gi,
      priority: 20
    },
    
    // PRIORITY 15: Boolean-like values
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE)\b/gi,
      priority: 15
    }
  ]
});