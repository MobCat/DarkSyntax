// darkSyntax/configs/speedcoding.js - Speedcoding language configuration
// ========================================================================
// Speedcoding (1953)
//
// Configs
// =======================
// ALIASES: ['speedcoding', 'speedcode', 'speedco']
// Target machine: IBM 701 (1953)
//
// SPEEDCODING SYNTAX NOTES
// ========================
//
// Speedcoding (SpeedCo I) was the FIRST high-level programming language for an
// IBM computer, created by John Backus in 1953 for the IBM 701. This predates
// FORTRAN by 4 years! It was an interpretive system focused on floating-point
// scientific computation.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - First high-level language for IBM (1953)
// - Created by John Backus (later created FORTRAN and BNF notation)
// - Interpreted language (ran 10-20x slower than hand-coded machine code)
// - First to provide automatic decimal<->binary conversion
// - First to use symbolic addresses instead of absolute memory locations
//
// PROGRAM STRUCTURE
// -----------------
// Programs consist of instructions on punched cards, one instruction per card.
// Each instruction has:
// - LOC: Memory location (address) where instruction is stored
// - OP₁: Primary operation (arithmetic, I/O, function)
// - R: R-code for address modification (0-7)
// - A, B, C: Addresses of operands and result
// - OP₂: Secondary operation (control flow, transfers)
// - D: Address for OP₂ operation
// - L: Listing control digit (OPTIONAL - debug output control. 0=False, 1=True)
//
// The instruction itself is:
//   OP₁ R A B C OP₂ D [L]
//
// Example: ADD 0 1013 1013 0950 SETRA 0000
// With L:  ADD 0 1013 1013 0950 SETRA 0000 0
//
// LOC is metadata (tells where to store the instruction in memory 300-1013)
// L is optional debug metadata (controls listing output with operator panel switches)
//
// For text representation, we show LOC at the start for readability.
// Many programs omit L entirely since it's just for debugging.
//
// NUMBER SYSTEM
// -------------
// - Floating-point decimal: F × 10^E
// - Fractional part: 10 decimal digits
// - Exponent range: -236 to +236
// - Internal: Binary floating-point (converted automatically)
//
// OPERATIONS (OP₁)
// ----------------
// Arithmetic:
//   ADD, SUB, MPY, DIV - Basic operations
//   ADDAB, ABADD - Absolute value operations
//   SUBAB, ABSUB - Absolute subtract operations
//   NGMPY, NGDIV - Negative multiply/divide
//
// Mathematical Functions:
//   SQRT, SINE, ARTAN, EXP, LN - Elementary functions
//
// Tape Operations:
//   WRTPJ/K/L/M - Write to tape J/K/L/M
//   RFTPJ/K/L/M - Read forward from tape
//   RBTPJ/K/L/M - Read backward from tape
//   SFTPJ/K/L/M - Skip forward on tape
//   SBTPJ/K/L/M - Skip backward on tape
//   RWTPJ/K/L/M - Rewind tape
//   EFTPJ/K/L/M - End file on tape
//
// Drum Operations:
//   WRDRP/Q - Write to drum P or Q
//   RFDRP/Q - Read forward from drum
//
// I/O:
//   PRINT - Print results
//   NOOP - No operation
//
// CONTROL OPERATIONS (OP₂)
// ------------------------
// Transfers:
//   TR - Unconditional transfer
//   TRPL - Transfer if positive
//   TRMN - Transfer if minus (negative)
//   TRZ - Transfer if zero
//   SNTRP/Q - Sense switch and transfer
//
// Address Modification:
//   TIA, TIB, TIC - Transfer and increase R_A, R_B, R_C
//   TIAB, TIBC, TIAC, TIABC - Transfer and increase combinations
//   TDA, TDB, TDC - Transfer and decrease R_A, R_B, R_C
//   TDAB, TDBC, TDAC, TDABC - Transfer and decrease combinations
//   SETRA, SETRB, SETRC - Set R quantities
//   SKRA, SKRB, SKRC - Skip if R equals D
//
// Address Counter Operations:
//   RADDA, RADDB, RADDC, RADDD - Reset and add address
//   ADDA, ADDB, ADDC, ADDD - Add to address
//   SUBA, SUBB, SUBC, SUBD - Subtract from address
//   STA, STB, STC, STD - Store address
//   SKIP - Skip if address equals counter
//
// Checking:
//   PRCH - Prepare check
//   STCH - Start check
//   ECHTR - End check and transfer
//   STOP - Stop and transfer
//
// STORAGE
// -------
// - Electrostatic: 714 words (addresses 300-1013)
// - Magnetic drums: 2 drums (P, Q) of 1024 words each
// - Magnetic tapes: 4 tapes (J, K, L, M) up to 140,000 words each
// - Word size: 72 bits
//
// ADDRESS MODIFICATION (R-CODES)
// ------------------------------
// R-code controls which addresses get modified by R_A, R_B, R_C:
//   0: No modification
//   1: C + R_C
//   2: B + R_B
//   3: B + R_B, C + R_C
//   4: A + R_A
//   5: A + R_A, C + R_C
//   6: A + R_A, B + R_B
//   7: A + R_A, B + R_B, C + R_C
//
// COMMENTS
// --------
// Speedcoding had NO comment syntax - it was just punched cards!
// For modern text representation:
// - Lines starting with 4-digit LOC are instructions (highlighted)
// - Lines starting with *> are explicit comments (COBOL-style)
// - Everything else is a comment (not highlighted)
// - Inline comments can use *> after the instruction
//
// PRIORITY STRATEGY
// -----------------
// 100: Explicit comments (*> prefix or lines without LOC)
// 50:  Operations and addresses (only on lines starting with LOC)
// 
// Instruction format on valid lines (starting with 4-digit LOC):
//   LOC  OP1    R   A     B     C     OP2    D     L
//   0300 ADD    0  1013  1013  0950  SETRA  0000  0
//
// REGEX NOTES
// -----------
// - All operations are uppercase (machine constraint)
// - Addresses are 4-digit numbers (0000-1013)
// - R-codes are single digits (0-7)
// - Tape/drum letters are single characters
// - Combined operations like WRTPJ are single tokens

darkSyntax.registerLanguage('speedcoding', {
  rules: [
    // PRIORITY 100: Explicit inline comments (*> anywhere in line)
    {
      class: 'comment',
      pattern: /\*>.*$/gm,
      priority: 100
    },
    
    // PRIORITY 95: Comment lines (non-empty lines not starting with 4-digit LOC)
    {
      class: 'comment',
      pattern: /^(?!\d{4}\s)(?!\s*$).+$/gm,
      priority: 95
    },
    
    // PRIORITY 50: LOC (4-digit memory address at start of line)
    {
      class: 'number',
      pattern: /^\d{4}(?=\s)/gm,
      priority: 50
    },
    
    // PRIORITY 45: OP₁ Arithmetic operations
    {
      class: 'keyword',
      pattern: /\b(ADD|SUB|ADDAB|ABADD|SUBAB|ABSUB|MPY|NGMPY|DIV|NGDIV)\b/g,
      priority: 45
    },
    
    // PRIORITY 45: Mathematical functions
    {
      class: 'builtin',
      pattern: /\b(SQRT|SINE|ARTAN|EXP|LN)\b/g,
      priority: 45
    },
    
    // PRIORITY 45: Tape operations
    {
      class: 'function',
      pattern: /\b(WRTPJ|WRTPK|WRTPL|WRTPM|RFTPJ|RFTPK|RFTPL|RFTPM|RBTPJ|RBTPK|RBTPL|RBTPM|SFTPJ|SFTPK|SFTPL|SFTPM|SBTPJ|SBTPK|SBTPL|SBTPM|RWTPJ|RWTPK|RWTPL|RWTPM|EFTPJ|EFTPK|EFTPL|EFTPM)\b/g,
      priority: 45
    },
    
    // PRIORITY 45: Drum operations
    {
      class: 'function',
      pattern: /\b(WRDRP|WRDRQ|RFDRP|RFDRQ)\b/g,
      priority: 45
    },
    
    // PRIORITY 45: I/O operations
    {
      class: 'function',
      pattern: /\b(PRINT|NOOP)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: OP₂ Control operations
    {
      class: 'keyword',
      pattern: /\b(TR|TRPL|TRMN|TRZ|SNTRP|SNTRQ|TIA|TIB|TIC|TIAB|TIBC|TIAC|TIABC|TDA|TDB|TDC|TDAB|TDBC|TDAC|TDABC|SETRA|SETRB|SETRC|SKRA|SKRB|SKRC|RADDA|RADDB|RADDC|RADDD|ADDA|ADDB|ADDC|ADDD|SUBA|SUBB|SUBC|SUBD|STA|STB|STC|STD|SKIP|PRCH|STCH|ECHTR|STOP)\b/g,
      priority: 40
    },
    
    // PRIORITY 30: 4-digit addresses (A, B, C, D memory addresses)
    {
      class: 'number',
      pattern: /\b\d{4}\b/g,
      priority: 30
    },
    
    // PRIORITY 25: R-code (1-7 only - these perform address modification)
    // R=0 does nothing, so we don't highlight it
    // R=8 or R=9 are invalid, so we don't highlight them either
    // This makes active R-codes stand out visually
    {
      class: 'boolean',
      pattern: /(?<=\b(?:ADD|SUB|ADDAB|ABADD|SUBAB|ABSUB|MPY|NGMPY|DIV|NGDIV|SQRT|SINE|ARTAN|EXP|LN|WRTPJ|WRTPK|WRTPL|WRTPM|RFTPJ|RFTPK|RFTPL|RFTPM|RBTPJ|RBTPK|RBTPL|RBTPM|SFTPJ|SFTPK|SFTPL|SFTPM|SBTPJ|SBTPK|SBTPL|SBTPM|RWTPJ|RWTPK|RWTPL|RWTPM|EFTPJ|EFTPK|EFTPL|EFTPM|WRDRP|WRDRQ|RFDRP|RFDRQ|PRINT|NOOP)\s+)[1-7]\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Single digit L value (listing control, appears at end)
    {
      class: 'boolean',
      pattern: /\b[0-9](?=\s*(?:\*>.*)?$)/g,
      priority: 20
    }
  ]
});