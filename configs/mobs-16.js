// darkSyntax/configs/mobs-16.js - MOBS-16 configuration
// =======================================================
// MOBS-16 (2025)
// MobCat's Own Basic System - Destructive assembly-inspired esoteric language
//
// Configs
// =======================
// ALIASES: ['mobs-16', 'mobs', 'mobs16', 'm16']
// File extensions: .mobs, .m16
//
// .mobs - MOBS-16 source files
// .m16 - Alternative extension
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by MobCat (2025) - "Where buffer overflows are features, not bugs"
// - Inspired by Brainfuck, x86 Assembly, Commodore 64 BASIC, Speedcoding
// - Philosophy: Destructive by default, overflow is intentional
// - Circular execution model - programs loop until explicit termination
// - 16 opcodes, each exactly 4 characters long
// - Hardware-agnostic S register can be "anything that stores an on/off state"
//
// INFLUENCED
// ----------
// - Brainfuck (1993) - Obtuse behavior, minimal operations
// - x86/x64 Assembly - Raw hardware access, register operations
// - Commodore 64 BASIC - PEEK/POKE philosophy
// - Speedcoding (1953) - Rigid language with creative limited functions
//
// USED FOR
// --------
// - Esoteric programming challenges
// - Teaching about buffer overflows and undefined behavior
// - Demonstrating circular execution models over defined loops or GOTO
// - Creative hardware interfacing (S register can be anything!)
// - Code golf with hex
//
// KEY FEATURES
// ------------
// - 4 registers: M, O, B (data), S (screen/stack)
// - All values are hexadecimal nibbles (0-9, A-F)
// - All data is valid data as all data is just hex data
// - Destructive operations by default (except peek)
// - Circular execution - loops until eomf
// - Overflow/underflow wraps intentionally
// - No multiplication or division (use loops)
// - Hardware-agnostic S register
// - Uninitialized registers contain garbage (feature!)
//
// CORE SYNTAX
// -----------
// - 16 opcodes, each exactly 4 characters
// - Comments start with ~ and must be attached to code
// - All numeric values are hexadecimal
// - Registers: M, O, B (8 nibbles each), S (unlimited)
// - Each register has an internal cursor
// - Programs loop automatically unless terminated with eomf
//
// QUIRKS
// ------
// - **Destructive by default**: Most operations modify/destroy source
// - **Overflow is intentional**: Used for backward navigation
// - **Circular execution**: Programs loop until eomf
// - **No guard rails**: Buffer overflows are features
// - **Uninitialized registers**: Contain garbage on boot (free randomness!)
// - **TABS matter**: Each opcode is exactly 4 chars
// - **Comments must anchor to code**: Cannot have floating comment blocks
// - **Only peek is non-destructive**: Everything else modifies data
// - **S register is anything**: Tape, screen, DNA, dominoes, Bad Apple video
//
// You want to jump back 2 lines?
// ifnz O jump FFFFFFFE
// You fell into a common mobs-16 trap. 
// This code will loop around 4294967294 times before getting to where you want to go. 
// if you want to go back 2 lines, then you count up and back around.
// In this case the program is 9 lines long, you are on line 7, 
// and you want to go back 2 to get back to adds B M, then you need to jump forward 7 lines. not 4 billion lines.
// your just wasting compute time. everey line has to be looked at and procuessed, even noop
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Buffer over and underflows are a feature, not a bug"
// - "Simple enough to teach a monkey with a calculator"
// - "You can't have a buffer overflow if the buffer is designed to do that"
// - "There are no errors, only undefined behaviour"
// - "nom nom nom data"
//
// NOTES ON MOBS-16 SYNTAX
// -------------------------
// - Comments: ~ character (must be attached to code)
// - All values are hexadecimal (0-9, A-F)
// - Registers: M, O, B, S (case-sensitive)
// - Each opcode is exactly 4 characters
// - Whitespace between tokens
// - Programs loop until eomf


// MOBS-16 SYNTAX CONFIGURATION FOR DARKSYNTAX
// ============================================
darkSyntax.registerLanguage('mobs-16', {
  rules: [
    // Comments (~ to end of line)
    {
      class: 'comment',
      pattern: /~.*$/gm,
      priority: 100
    },

    // Opcodes (all 16, exactly 4 chars each)
    {
      class: 'keyword',
      pattern: /\b(init|adds|subs|move|dupe|jump|ifeq|ifgt|iflt|ifnz|ifyz|peek|noop|rand|bell|eomf)\b/g,
      priority: 50
    },

    // Register names (M, O, B, S)
    {
      class: 'variable',
      pattern: /\b[MOBS]\b/g,
      priority: 40
    },

    // Hex values (8 nibbles = 4 bytes for M/O/B registers)
    {
      class: 'number',
      pattern: /\b[0-9A-Fa-f]{8}\b/g,
      priority: 30
    },

    // Shorter hex values (for partial register operations)
    {
      class: 'number',
      pattern: /\b[0-9A-Fa-f]{1,7}\b/g,
      priority: 29
    },

    // 'to' keyword (for move/dupe/peek operations)
    {
      class: 'operator',
      pattern: /\bto\b/g,
      priority: 35
    }
  ]
});