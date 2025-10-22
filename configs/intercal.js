// darkSyntax/configs/intercal.js - INTERCAL language configuration
// =================================================================
// INTERCAL (1972)
// INTERCAL - Compiler Language With No Pronounceable Acronym
//
// Configs
// =======================
// ALIASES: ['intercal', 'i']
// File extensions: .i
//
// INTERCAL SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// INTERCAL (1972):
// - Created by Don Woods and James M. Lyon at Princeton University
// - Full name: "Compiler Language With No Pronounceable Acronym"
// - Designed as a parody of languages like COBOL, FORTRAN, and ALGOL
// - First esoteric programming language (esolang)
// - Satirizes overly complex and verbose language design
// - Reference manual is deliberately confusing and humorous
//
// INFLUENCED
// ----------
// - Spawned the entire "esoteric programming language" movement
// - Brainfuck (1993) - minimalist parody
// - Malbolge (1998) - intentionally difficult
// - Chef (2002) - programs as recipes
// - Shakespeare (2001) - programs as plays
// - Whitespace (2003) - only whitespace characters
// - LOLCODE (2007) - based on lolcat memes
//
// USED FOR
// --------
// - Satire and humor
// - Programming language theory discussions
// - Code golf and obfuscation contests
// - Teaching about language design (what NOT to do)
// - Entertainment and intellectual challenge
//
// KEY FEATURES
// ------------
// - PLEASE for politeness (compiler rejects if too polite or not polite enough!)
// - COME FROM instead of GOTO (jumps TO you, not you TO it)
// - Operators: mingle (~), select (~), interleave (¢)
// - ABSTAIN FROM and REINSTATE to disable/enable statements
// - IGNORE and REMEMBER for variables
// - GIVE UP to exit program
// - Variable types: spot (16-bit), two-spot (32-bit), tail, hybrid
// - Variable names: .1, :1, ,1, ;1 (punctuation-based)
// - Line numbers required
// - Expressions use reverse Polish notation
//
// CORE SYNTAX
// -----------
// Variables:
//   .1      # 16-bit spot variable
//   :1      # 32-bit two-spot variable
//   ,1      # Tail (array)
//   ;1      # Hybrid (array)
//
// Statements:
//   DO .1 <- #1         # Assignment
//   PLEASE DO .1 <- #1  # Polite assignment
//   DO COME FROM (1)    # Jump here from label 1
//   DO ABSTAIN FROM (1) # Disable line 1
//   DO REINSTATE (1)    # Re-enable line 1
//   PLEASE GIVE UP      # Exit program
//
// Operators:
//   ~  (mingle)   # Interleaves bits of two numbers
//   ¢  (select)   # Selects bits based on mask
//   $  (unary mingle) # SUB or $ prefix
//   V  (unary V)  # OR operation
//   ?  (unary ?)  # XOR operation
//
// Comments:
//   None! No comments allowed in INTERCAL
//   (But the reference manual is full of them)
//
// QUIRKS
// ------
// - Too many PLEASE: compiler says "Program is overly polite"
// - Too few PLEASE: compiler says "Program is insufficiently polite"
// - COME FROM makes control flow impossible to follow
// - No way to add comments in the language itself
// - Error messages are intentionally unhelpful
// - Variable names look like line noise
// - FORGET to pop the stack
// - RESUME to return from NEXT (subroutine call)
// - Line labels in parentheses: (100) DO ...
// - NEXT for computed COME FROM (like GOSUB)
//
// FAMOUS QUOTES FROM THE MANUAL
// ------------------------------
// - "The rules are designed to prevent the programmer from doing anything."
// - "INTERCAL has many features designed to make it as different from 
//    other languages as possible."
// - "It is a well-known and oft-demonstrated fact that a person whose work 
//    is not appreciated will not do their best work."
//
// EXAMPLE PROGRAMS
// ----------------
// Hello World (very simplified concept):
//   PLEASE DO ,1 SUB #1 <- #234
//   DO ,1 SUB #2 <- #112
//   DO ,1 SUB #3 <- #112
//   ...
//   PLEASE READ OUT ,1
//   PLEASE GIVE UP


// INTERCAL SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('intercal', {
  rules: [
    // PRIORITY 90: Line labels in parentheses
    {
      class: 'number',
      pattern: /^\s*\(\d+\)/gm,
      priority: 90
    },
    
    // PRIORITY 85: PLEASE keyword (special emphasis)
    {
      class: 'decorator',
      pattern: /\bPLEASE\b/g,
      priority: 85
    },
    
    // PRIORITY 80: Main keywords
    {
      class: 'keyword',
      pattern: /\b(DO|ABSTAIN|FROM|REINSTATE|FORGET|RESUME|IGNORE|REMEMBER|STASH|RETRIEVE|COME|NEXT|GIVE UP|WRITE IN|READ OUT|NOT)\b/g,
      priority: 80
    },
    
    // PRIORITY 75: Operators (mingle, select, etc.)
    {
      class: 'keyword',
      pattern: /[~¢$V?]/g,
      priority: 75
    },
    
    // PRIORITY 70: Assignment operator
    {
      class: 'keyword',
      pattern: /<-/g,
      priority: 70
    },
    
    // PRIORITY 65: SUB keyword (array subscript)
    {
      class: 'keyword',
      pattern: /\bSUB\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Variable types (spot, two-spot, tail, hybrid)
    {
      class: 'variable',
      pattern: /[.:,;]\d+/g,
      priority: 60
    },
    
    // PRIORITY 50: Numbers (with # prefix)
    {
      class: 'number',
      pattern: /#\d+/g,
      priority: 50
    },
    
    // PRIORITY 45: Bare numbers (line numbers, references)
    {
      class: 'number',
      pattern: /\b\d+\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Parenthesized expressions/labels
    {
      class: 'function',
      pattern: /\(\d+\)/g,
      priority: 40
    }
  ]
});