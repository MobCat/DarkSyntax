// darkSyntax/configs/flowmatic.js - FLOW-MATIC language configuration
// =====================================================================
// FLOW-MATIC (1955)
// FLOW-MATIC - First English-like business programming language
//
// Configs
// =======================
// ALIASES: ['flowmatic', 'flow-matic', 'b0']
// File extensions: .flow (hypothetical - no standard extension exists)
//
// FLOW-MATIC SYNTAX NOTES
// =======================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// FLOW-MATIC (1955):
// - Created by Grace Hopper and her team at Remington Rand/Sperry
// - Originally called B-0 (Business Language version 0)
// - First programming language to use English-like commands
// - Designed for UNIVAC I and UNIVAC II computers
// - Direct predecessor to COBOL (1959)
// - Proved that programming languages could be readable by non-mathematicians
// - Revolutionary concept: code should be readable by managers and accountants
// - Grace Hopper's vision: "It's much easier to get forgiveness than permission"
//
// INFLUENCED
// ----------
// - COBOL (1959) - Borrowed heavily from FLOW-MATIC's English-like syntax
// - All business-oriented programming languages
// - The entire concept of "natural language programming"
// - English-like SQL syntax (1970s)
// - AppleScript (1993) - Natural language scripting
//
// USED FOR
// --------
// - Business data processing on UNIVAC computers
// - Payroll systems
// - Inventory management
// - Accounting applications
// - Commercial calculations
// - Early business automation
//
// KEY FEATURES
// ------------
// - English-like syntax (revolutionary for 1955!)
// - Compiler (not interpreter) - also revolutionary
// - Focused on business applications, not scientific computing
// - Used words instead of mathematical symbols
// - Variables could have descriptive names
// - Automatic data type conversion
// - Division A (specification) and Division B (operations)
// - Hierarchical record structures
//
// CORE SYNTAX
// -----------
// Program Structure (two divisions):
//   (0) DIVISION A - Data definitions
//   (1) INPUT filename; item-1, item-2, item-3
//   (2) OUTPUT filename; result-1, result-2
//   
//   (3) DIVISION B - Operations
//   (4) TRANSFER payroll-file TO new-file
//   (5) COMPARE item-1 WITH item-2 ; IF GREATER GO TO operation-10
//   (6) SET result-1 TO item-1 PLUS item-2
//
// Commands:
//   INPUT - Define input file and fields
//   OUTPUT - Define output file and fields
//   TRANSFER - Copy from one file to another
//   COMPARE - Compare values (with IF GREATER/EQUAL/LESS)
//   SET - Assign values
//   JUMP TO - Unconditional branch
//   GO TO - Conditional branch
//   STOP - End program
//
// Arithmetic Operations:
//   PLUS, MINUS, TIMES (multiply), DIVIDED BY
//   Example: SET total TO price TIMES quantity
//
// QUIRKS
// ------
// - Extremely verbose (even more than COBOL!)
// - Line numbers required (numbered operations)
// - All operations in UPPER CASE
// - Semicolons separate major clauses
// - Limited loop constructs (mostly GOTOs)
// - No subroutines or functions
// - Variables were defined by position in record
// - Very rigid format and structure
// - Designed for punch cards
// - Reading code feels like reading instructions
//
// FAMOUS GRACE HOPPER QUOTES
// ---------------------------
// - "Humans are allergic to change. They love to say, 'We've always done it this way.' I try to fight that."
// - "A ship in port is safe, but that's not what ships are built for."
// - "The most dangerous phrase in the language is, 'We've always done it this way.'"
// - "If it's a good idea, go ahead and do it. It's much easier to apologize than it is to get permission."


// FLOW-MATIC SYNTAX CONFIGURATION FOR DARKSYNTAX
// ===============================================

darkSyntax.registerLanguage('flowmatic', {
  showLineNumbers: false,
  rules: [
    // PRIORITY 101: Comments (non-empty lines without operation numbers)
    {
      class: 'comment',
      pattern: /^(?!\s*\(\d+\))(?!\s*$).+$/gm,
      priority: 101
    },
    
    // PRIORITY 100: Line (0) - Program title/identifier (special case)
    {
      class: 'class',
      pattern: /^\s*\(0\)\s+(.+)$/gm,
      captureGroup: 1,
      priority: 100
    },
    
    // PRIORITY 95: Line numbers (operation numbers) for numbered lines
    {
      class: 'number',
      pattern: /^\s*\(\d+\)/gm,
      priority: 95
    },
    
    // PRIORITY 90: Division headers
    {
      class: 'decorator',
      pattern: /\bDIVISION\s+[AB]\b/gi,
      priority: 90
    },
    
    // PRIORITY 88: Text after division headers (after the -)
    {
      class: 'variable',
      pattern: /(?<=DIVISION\s+[AB]\s*-\s*)([A-Z\s]+)/gi,
      captureGroup: 1,
      priority: 88
    },
    
    // PRIORITY 80: Major keywords (I/O operations) - include hyphenated forms
    {
      class: 'keyword',
      pattern: /\b(INPUT|OUTPUT|TRANSFER|WRITE-ITEM|WRITE|READ|SPECIFICATIONS)\b/gi,
      priority: 80
    },
    
    // PRIORITY 75: Control flow keywords
    {
      class: 'keyword',
      pattern: /\b(COMPARE|IF|GO TO|JUMP TO|OPERATION|STOP)\b/gi,
      priority: 75
    },
    
    // PRIORITY 70: Comparison operators and connectors
    {
      class: 'keyword',
      pattern: /\b(GREATER|LESS|EQUAL|WITH|THAN|AND|OR)\b/gi,
      priority: 70
    },
    
    // PRIORITY 68: Special keyword RUN (stands out as boolean)
    {
      class: 'boolean',
      pattern: /\bRUN\b/gi,
      priority: 68
    },
    
    // PRIORITY 65: Arithmetic operations
    {
      class: 'keyword',
      pattern: /\b(SET|TO|PLUS|MINUS|TIMES|DIVIDED BY)\b/gi,
      priority: 65
    },
    
    // PRIORITY 60: File and variable names (ALL-CAPS, may have hyphens)
    {
      class: 'variable',
      pattern: /\b[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)*\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Numbers (decimal and integer)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 55
    },
    
    // PRIORITY 40: Strings (if they exist - rare in FLOW-MATIC)
    {
      class: 'string',
      pattern: /"[^"]*"/g,
      priority: 40
    }
  ]
});