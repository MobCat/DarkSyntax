// darkSyntax/configs/fortran.js - Fortran language configuration
// ================================================================
// Fortran (1957)
// FORmula TRANslation - First widely-used high-level programming language
//
// Configs
// =======================
// ALIASES: ['fortran', 'f90', 'f95', 'f03', 'f08', 'f', 'for']
// File extensions: .f (fixed-form), .for, .f90, .f95, .f03, .f08, .f18 (free-form)
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by John Backus and team at IBM (1954-1957)
// - First specification completed November 1954
// - First manual published October 1956
// - First compiler delivered April 1957 for IBM 704
// - Original team: Backus, Goldberg, Best, Herrick, Sheridan, Nutt, Nelson, Ziller, Stern, Haibt, Sayre
// - Fortran (FORmula TRANslation) - named for translating mathematical formulas
// - First optimizing compiler - produced code nearly as fast as hand-written assembly
// - Reduced programming time from weeks to hours
// - "Much of my work has come from being lazy. I didn't like writing programs" - John Backus
// - FORTRAN II (1958) - Added subroutines and COMMON blocks
// - FORTRAN IV (1962) - Explicit type declarations, logical IF statement
// - FORTRAN 66 (1966) - First ANSI standard
// - FORTRAN 77 (1978) - Character strings, IF-THEN-ELSE, DO-WHILE loops
// - Fortran 90 (1990) - Free-form source, modules, pointers, recursion, CASE statement
// - Fortran 95 (1997) - Pure and elemental procedures, FORALL statement
// - Fortran 2003 (2004) - Object-oriented programming, C interoperability
// - Fortran 2008 (2010) - Coarrays for parallel programming, DO CONCURRENT
// - Fortran 2018 (2018) - Further parallelism features, enhanced interoperability
// - Fortran 2023 (2023) - Conditional expressions, unsigned integers
// - Von Neumann famously asked "Why would you want more than machine language?"
// - Changed computing forever - democratized programming for scientists
// - Still used today for high-performance numerical computing
// - NASA, weather forecasting, computational physics rely on Fortran
//
// INFLUENCED
// ----------
// - BASIC (1964) - Simplified syntax inspired by Fortran
// - ALGOL 60 (1960) - Competing scientific language, influenced by Fortran's success
// - PL/I (1964) - IBM's attempt to combine Fortran and COBOL
// - MATLAB (1984) - Matrix operations inspired by Fortran arrays
// - NumPy/SciPy (2006) - Python scientific computing follows Fortran conventions
// - Julia (2012) - Modern scientific computing, Fortran-like performance
// - APL (1966) - Array operations influenced by Fortran
// - C (1972) - Dennis Ritchie was influenced by Fortran's compiler design
// - SPEAKEASY (1964) - Interactive computing system based on Fortran
// - Fortress (2006) - Sun's Fortran successor (discontinued 2012)
//
// USED FOR
// --------
// - Weather forecasting and climate modeling
// - Computational fluid dynamics (CFD)
// - Nuclear physics simulations
// - Quantum chemistry calculations
// - Finite element analysis (engineering)
// - Astrophysics and orbital mechanics
// - High-performance computing (HPC)
// - Seismic data processing
// - Computational biology and bioinformatics
// - Financial modeling and risk analysis
// - Machine learning (some libraries still use Fortran backends)
//
// KEY FEATURES
// ------------
// - Arrays are 1-indexed by default (unlike C's 0-indexing)
// - Column-major array storage (opposite of C's row-major)
// - Native complex number arithmetic
// - Powerful array operations and array slicing
// - DO loops with flexible bounds
// - Intrinsic mathematical functions (SIN, COS, SQRT, etc.)
// - Pass-by-reference parameter passing
// - COMMON blocks for shared data (legacy)
// - Modules for modern code organization (Fortran 90+)
// - Derived types (structs) with type-bound procedures
// - Generic interfaces and operator overloading
// - Coarrays for parallel programming (Fortran 2008+)
// - Interoperability with C (ISO_C_BINDING)
//
// CORE SYNTAX
// -----------
// Fixed-form (Fortran 77):
//   - Columns 1-5: Statement labels
//   - Column 6: Continuation character
//   - Columns 7-72: Code
//   - Columns 73-80: Sequence numbers (ignored)
//
// Free-form (Fortran 90+):
//   - No column restrictions
//   - ! for comments
//   - & for line continuation
//
// Basic Program:
//   PROGRAM hello
//     IMPLICIT NONE
//     PRINT *, "Hello, World!"
//   END PROGRAM hello
//
// Arrays (1-indexed):
//   REAL, DIMENSION(10) :: array
//   array(1) = 3.14  ! First element
//   array(5:8) = 0.0 ! Slice assignment
//
// DO Loop:
//   DO i = 1, 10
//     PRINT *, i
//   END DO
//
// QUIRKS
// ------
// - **Case insensitivity**: PROGRAM, Program, and program are identical
//   * Convention: Uppercase for keywords in old code, lowercase in modern
//   * Variable names are case-insensitive too
//
// - **Array indexing**: Arrays start at 1, not 0
//   * INTEGER :: arr(10)  ! Elements arr(1) to arr(10)
//   * Can specify custom bounds: INTEGER :: arr(0:9)
//   * Array slicing: arr(2:5) selects elements 2, 3, 4, 5
//
// - **Column-major arrays**: Opposite of C/C++
//   * REAL :: matrix(3, 4)  ! 3 rows, 4 columns
//   * First index varies fastest in memory
//   * Critical for performance when calling from C
//
// - **Fixed-form format**: Fortran 77 and earlier
//   * Code must be in columns 7-72
//   * Column 6 for continuation (any non-space character)
//   * C or * in column 1 for comments
//   * Very strict and archaic
//
// - **Implicit typing**: Variables starting with I-N are INTEGER
//   * A-H and O-Z are REAL by default
//   * IMPLICIT NONE disables this (highly recommended!)
//   * Source of countless bugs in old code
//
// - **Significant spaces**: Unlike C, spaces matter
//   * DO 10 I = 1, 10 is a loop
//   * DO 10 I = 1. 10 is an assignment! (period vs comma)
//   * Famous NASA Mariner 1 bug attributed to this
//
// - **Pass by reference**: All arguments passed by reference
//   * CALL sub(x) passes address of x
//   * Cannot pass constants directly: CALL sub(5) is illegal
//   * Use temporary variables or specify INTENT
//
// - **Logical operators**: Use dots, not symbols
//   * .AND., .OR., .NOT. instead of &&, ||, !
//   * .EQ., .NE., .LT., .GT. for comparisons (old style)
//   * Modern: ==, /=, <, <= (but dots still valid)
//
// - **FORMAT statements**: Printf-like but more complex
//   * WRITE(*, 100) x, y, z
//   * 100 FORMAT(F10.2, I5, A)
//   * Modern Fortran allows free-format: WRITE(*, *) x, y, z
//
// - **GOTO and labels**: Statement labels for control flow
//   * 10 CONTINUE marks line 10
//   * GO TO 10 jumps to that line
//   * Source of "spaghetti code" reputation
//
// - **COMMON blocks**: Shared global storage (legacy)
//   * COMMON /block/ var1, var2
//   * Replaced by modules in modern Fortran
//   * Still found in legacy scientific code
//
// - **Hollerith strings**: Ancient string literals
//   * 5HHELLO represents "HELLO" (5 characters)
//   * Replaced by quoted strings in Fortran 77
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Much of my work has come from being lazy. I didn't like writing programs" - John Backus
// - "Why would you want more than machine language?" - John von Neumann (1954), questioning Fortran's value
// - "Hand-to-hand combat with the machine, with the machine often winning" - John Backus on assembly programming
// - "Fortran changed the terms of communication between humans and computers" - J.A.N. Lee, computer historian
// - "Here is a language so far ahead of its time that it was not only an improvement on its predecessors but also on nearly all its successors" - Tony Hoare (about ALGOL, Fortran's competitor)
// - "Real programmers don't use Pascal" - Ed Post (1983), advocating for Fortran
// - "Fortran liberated computers from the exclusive realm of programmers" - IBM History
//
// NOTES ON FORTRAN SYNTAX
// ------------------------
// - Case-insensitive language (all keywords use /gi flag)
// - Comments: ! anywhere (modern), C/* in column 1 (legacy)
// - Fixed-form: columns 7-72 for code, column 6 for continuation
// - Free-form: no column restrictions, & for continuation
// - Arrays are 1-indexed by default
// - Column-major array storage (first index varies fastest)
// - Implicit typing: I-N → INTEGER, others → REAL (use IMPLICIT NONE!)
// - Pass by reference for all subroutine arguments
// - Logical operators: .AND., .OR., .NOT., .EQV., .NEQV.
// - Old comparisons: .EQ., .NE., .LT., .LE., .GT., .GE.
// - Modern comparisons: ==, /=, <, <=, >, >=
// - Power operator: ** (e.g., x**2 for x squared)
// - DO loops: DO i = start, end, step ... END DO
// - IF blocks: IF (condition) THEN ... ELSE ... END IF
// - SELECT CASE for multi-way branches
// - Subroutines: SUBROUTINE name(args) ... END SUBROUTINE
// - Functions: FUNCTION name(args) RESULT(var) ... END FUNCTION
//


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('fortran', {
  rules: [
    // PRIORITY 100: Comments (modern ! style)
    {
      class: 'comment',
      pattern: /!.*$/gm,
      priority: 100
    },
    
    // PRIORITY 100: Comments (legacy C or * in column 1)
    {
      class: 'comment',
      pattern: /^[Cc\*].*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings (both quote styles)
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
    
    // PRIORITY 65: Boolean values (must come before logical operators)
    {
      class: 'boolean',
      pattern: /\.(TRUE|FALSE)\./gi,
      priority: 65
    },
    
    // PRIORITY 60: Logical operators
    {
      class: 'keyword',
      pattern: /\.(AND|OR|NOT|EQV|NEQV|EQ|NE|LT|LE|GT|GE)\./gi,
      priority: 60
    },
    
    // PRIORITY 50: Keywords and control structures
    {
      class: 'keyword',
      pattern: /\b(PROGRAM|END|SUBROUTINE|FUNCTION|MODULE|INTERFACE|USE|CONTAINS|IMPLICIT|NONE|IF|THEN|ELSE|ELSEIF|ENDIF|SELECT|CASE|DEFAULT|DO|WHILE|ENDDO|EXIT|CYCLE|CONTINUE|GO TO|GOTO|RETURN|STOP|PAUSE|WHERE|ELSEWHERE|ENDWHERE|FORALL|ENDFORALL|ALLOCATE|DEALLOCATE|NULLIFY|INTENT|IN|OUT|INOUT|OPTIONAL|PARAMETER|SAVE|TARGET|POINTER|ALLOCATABLE|DIMENSION|EQUIVALENCE|COMMON|DATA|BLOCK|NAMELIST|ENTRY|INCLUDE|FORMAT|CALL|RESULT|RECURSIVE|PURE|ELEMENTAL|ABSTRACT|EXTENDS|FINAL|GENERIC|IMPORT|NON_OVERRIDABLE|NOPASS|PASS|PROTECTED|SEQUENCE|BIND|ASSOCIATE|CRITICAL|CONCURRENT|CONTIGUOUS|SUBMODULE|ERROR STOP|LOCK|UNLOCK|SYNC)\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: I/O statements
    {
      class: 'keyword',
      pattern: /\b(READ|WRITE|PRINT|OPEN|CLOSE|INQUIRE|BACKSPACE|ENDFILE|REWIND|FLUSH)\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Data types
    {
      class: 'builtin',
      pattern: /\b(INTEGER|REAL|DOUBLE PRECISION|COMPLEX|CHARACTER|LOGICAL|TYPE|CLASS)\b/gi,
      priority: 40
    },
    
    // PRIORITY 40: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(ABS|ACOS|AIMAG|AINT|ALOG|ALOG10|AMAX0|AMAX1|AMIN0|AMIN1|AMOD|ANINT|ASIN|ATAN|ATAN2|CABS|CCOS|CEXP|CHAR|CLOG|CMPLX|CONJG|COS|COSH|CSIN|CSQRT|DABS|DACOS|DASIN|DATAN|DATAN2|DBLE|DCOS|DCOSH|DDIM|DEXP|DIM|DINT|DLOG|DLOG10|DMAX1|DMIN1|DMOD|DNINT|DPROD|DSIGN|DSIN|DSINH|DSQRT|DTAN|DTANH|EXP|FLOAT|IABS|ICHAR|IDIM|IDINT|IDNINT|IFIX|INDEX|INT|ISIGN|LEN|LGE|LGT|LLE|LLT|LOG|LOG10|MAX|MAX0|MAX1|MAXVAL|MIN|MIN0|MIN1|MINVAL|MOD|NINT|REAL|SIGN|SIN|SINH|SNGL|SQRT|SUM|TAN|TANH|SIZE|SHAPE|RESHAPE|TRANSPOSE|MATMUL|DOT_PRODUCT|PRESENT|ASSOCIATED|CEILING|FLOOR|MERGE|PACK|UNPACK|SPREAD|TRIM|ADJUSTL|ADJUSTR|ALL|ANY|COUNT|PRODUCT|LBOUND|UBOUND|ALLOCATED|TINY|HUGE|EPSILON|NEAREST|SPACING)\b/gi,
      priority: 40
    },
    
    // PRIORITY 30: Function/subroutine calls (word before parenthesis)
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 20: Numbers (including scientific notation with E or D)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([EeDd][+-]?\d+)?/g,
      priority: 20
    }
  ]
});