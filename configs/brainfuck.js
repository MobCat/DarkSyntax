// darkSyntax/configs/brainfuck.js - Brainfuck language configuration
// ===================================================================
// Brainfuck (1993)
// Brainfuck - Minimalist esoteric programming language
//
// Configs
// =======================
// ALIASES: ['brainfuck', 'bf', 'b']
// File extensions: .bf, .b
//
// BRAINFUCK SYNTAX NOTES
// ======================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// Brainfuck (1993):
// - Created by Urban Müller in Switzerland
// - Designed to have a compiler that fits in under 1024 bytes
// - Named to be intentionally provocative/shocking
// - Challenge: smallest possible Turing-complete language
// - Compiler for Amiga was only 240 bytes!
// - Only 8 commands total
//
// INFLUENCED
// ----------
// - Ook! (2002) - Uses only "Ook.", "Ook?", "Ook!"
// - Whitespace (2003) - Only uses spaces, tabs, linefeeds
// - Chicken (2002) - Only the word "chicken"
// - JSFuck (2009) - JavaScript using only 6 characters []()!+
// - Pi (2001) - Uses only π as the command
// - Unary (2009) - Uses only the digit 0
//
// USED FOR
// --------
// - Demonstrating Turing completeness
// - Compiler design education (minimal compiler)
// - Code golf and obfuscation
// - Computer science theory
// - Proving concepts with minimal syntax
// - Entertainment and puzzles
//
// KEY FEATURES
// ------------
// - Only 8 commands (< > + - . , [ ])
// - Turing complete despite simplicity
// - Operates on array of memory cells
// - Single pointer to current cell
// - All other characters are comments
// - No variable names, no functions
// - Pure imperative programming
//
// CORE SYNTAX
// -----------
// Commands:
//   >   Move pointer right (increment pointer)
//   <   Move pointer left (decrement pointer)
//   +   Increment value at pointer
//   -   Decrement value at pointer
//   .   Output value at pointer as ASCII
//   ,   Input one byte, store at pointer
//   [   Jump forward past ] if cell is 0
//   ]   Jump back to [ if cell is non-zero
//
// Comments:
//   Any character that isn't one of the 8 commands
//
// Memory Model:
//   - Array of cells (typically 30,000)
//   - Each cell is one byte (0-255)
//   - Cells wrap: 255 + 1 = 0, 0 - 1 = 255
//   - Pointer starts at cell 0
//
// QUIRKS
// ------
// - No way to directly set a value (must increment from 0)
// - No multiplication or division (must use loops)
// - Extremely difficult to read and write
// - Programs are essentially undebuggable
// - Comments don't exist as a feature (ignored characters)
// - Cell values wrap around (modulo 256)
// - Hello World takes ~100 characters
// - Any complex algorithm is brutally verbose
//
// FAMOUS PROGRAMS
// ---------------
// Set cell to 65 (ASCII 'A') and output:
//   ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.
//
// Print "Hello World!":
//   ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.
//   >---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.


// BRAINFUCK SYNTAX CONFIGURATION FOR DARKSYNTAX
// ==============================================
darkSyntax.registerLanguage('brainfuck', {
  rules: [
    // PRIORITY 100: Loops (brackets) - highest priority
    {
      class: 'keyword',
      pattern: /[\[\]]/g,
      priority: 100
    },
    
    // PRIORITY 90: Pointer movement
    {
      class: 'builtin',
      pattern: /[><]/g,
      priority: 90
    },
    
    // PRIORITY 80: Value operations
    {
      class: 'number',
      pattern: /[\+\-]/g,
      priority: 80
    },
    
    // PRIORITY 70: I/O operations
    {
      class: 'function',
      pattern: /[,.]/g,
      priority: 70
    },
    
    // PRIORITY 10: Everything else is a comment!
    {
      class: 'comment',
      pattern: /[^><+\-.,\[\]]+/g,
      priority: 10
    }
  ]
});