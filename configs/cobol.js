// darkSyntax/configs/cobol.js - COBOL language configuration
// ============================================================
// COBOL - COmmon Business-Oriented Language (1959)
// The language that still runs the world's financial systems
//
// Configs
// =======================
// ALIASES: ['cobol', 'cbl', 'cob']
// File extensions: .cobol, .cbl, .cob
// Standardized: 1968 (ANSI), revised 1974, 1985, 2002, 2014, 2023
//
// COBOL SYNTAX NOTES
// ==================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by CODASYL committee (April 8, 1959 - January 8, 1960)
// - Based on Grace Hopper's FLOW-MATIC (1958)
// - First specification: COBOL-60 (January 1960)
// - U.S. Department of Defense mandated adoption (1960)
// - By 1970: Most widely used programming language in the world
// - First language standardized by ANSI (1968) and ISO (1972)
// - Designed to be readable by non-programmers (managers, auditors)
// - Explicitly intended as a "stopgap" - still here 65+ years later!
// - Grace Hopper called "the (grand)mother of COBOL"
// - Mary K. Hawes (Burroughs) initiated the project
// - Jean Sammet led specification design
//
// INFLUENCED
// ----------
// - PL/I (1964) - IBM's attempt to combine COBOL and FORTRAN
// - SQL (1974) - English-like database query language
// - ABAP (1983) - SAP's business programming language
// - RPG (IBM) - Report Program Generator for business
// - Natural language programming movement
// - Business process modeling languages
// - Verbose API design philosophy
//
// USED FOR
// --------
// - Banking and financial systems (43% of US banking systems)
// - Insurance claim processing
// - Government benefits systems (Social Security, unemployment)
// - Airline reservation systems
// - Healthcare administration
// - Payroll processing
// - Inventory management
// - ATM transactions (95% use COBOL)
// - Credit card processing (80% of in-person transactions)
//
// KEY FEATURES
// ------------
// - English-like syntax: MOVE X TO Y, ADD A TO B
// - Self-documenting code philosophy
// - Four-division structure: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
// - Fixed-point decimal arithmetic (no floating-point errors in money!)
// - Record-oriented file processing
// - Hierarchical data structures (01-49 levels)
// - PICTURE clause for data formatting
// - Verb-noun sentence structure
// - Over 300 reserved words
// - Case-insensitive
//
// CORE SYNTAX
// -----------
// Program structure:
//   IDENTIFICATION DIVISION.
//   PROGRAM-ID. MY-PROGRAM.
//   
//   ENVIRONMENT DIVISION.
//   
//   DATA DIVISION.
//   WORKING-STORAGE SECTION.
//   01 CUSTOMER-RECORD.
//      05 CUSTOMER-ID      PIC 9(6).
//      05 CUSTOMER-NAME    PIC X(30).
//      05 ACCOUNT-BALANCE  PIC 9(8)V99.
//   
//   PROCEDURE DIVISION.
//       DISPLAY "Hello, World!".
//       STOP RUN.
//
// PICTURE clauses:
//   PIC 9(5)      - 5 digits
//   PIC X(20)     - 20 characters
//   PIC 9(8)V99   - 8 digits, 2 decimal places
//   PIC S9(4)     - Signed 4-digit number
//   PIC $ZZZ,ZZ9.99 - Formatted currency
//
// Statements:
//   MOVE source TO destination
//   ADD A TO B GIVING C
//   COMPUTE TOTAL = PRICE * QUANTITY
//   IF condition THEN ... ELSE ... END-IF
//   PERFORM procedure-name
//   READ file-name INTO record
//   WRITE record FROM data-area
//
// The column rules were:
//    Columns 1-6: Sequence numbers (line numbers from punch cards)
//    Column 7: Indicator area (*, -, D for comment/continuation/debug)
//    Columns 8-11: Area A (divisions, sections, paragraphs, 01/77 levels)
//    Columns 12-72: Area B (regular statements)
//    Columns 73-80: Identification area (ignored)
//
// QUIRKS
// ------
// - **Verbosity is the point**: "MOVE X TO Y" vs "Y = X"
//   * Designed for business people, not mathematicians
//   * Sacrifices brevity for readability
//   * COBOL statement: 25 lines, Python equivalent: 10 lines
// - **Column-sensitive format** (historically):
//   * Columns 1-6: Sequence numbers (punch card era)
//   * Column 7: Comment indicator (*)
//   * Columns 8-11: Area A (divisions, sections, paragraphs)
//   * Columns 12-72: Area B (statements)
//   * Columns 73-80: Ignored (identification)
//   * Modern free-format available since COBOL-2002
// - **PICTURE clause madness**: Complex format specifications
//   * PIC $ZZZ,ZZ9.99 - currency with zero suppression
//   * PIC X(30) - 30-character string
//   * Learning PICTURE is learning a mini-language
// - **Hyphenated names everywhere**: CUSTOMER-ID, ACCOUNT-BALANCE
//   * Underscores not allowed historically
//   * Everything-is-hyphenated-like-this
// - **Periods are statement terminators**: Easy to forget
//   * Missing period = program won't compile
//   * Extra period = logic error
// - **Level numbers define hierarchy**: 01, 05, 10, 15...
//   * 01 is top level (record)
//   * 05 is child of 01
//   * 88 is condition name (boolean)
//   * 66 is RENAMES, 77 is standalone item
// - **No native arrays**: Use OCCURS clause
//   * 01 ITEM-TABLE.
//      05 ITEM OCCURS 100 TIMES PIC X(20).
// - **PERFORM instead of functions**: Procedural paragraphs
//   * PERFORM CALCULATE-TOTAL
//   * Goes to paragraph, executes, returns
// - **GOTO considered harmful**: Even in COBOL community
//   * ALTER statement (changes GOTO target) deprecated in 1985
//   * Jean Sammet: "ALTER was a terrible mistake"
// - **Implied subjects**: Can omit repeated subjects
//   * IF A = 1 OR 2 OR 3 means IF A = 1 OR A = 2 OR A = 3
// - **The skills crisis**: Average COBOL programmer age 55+
//   * Fewer than 24,000 COBOL programmers in US
//   * 220 billion lines of COBOL in production
//   * $3 trillion in daily commerce depends on it
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "COBOL was intended to be easy for programmers to learn and use, while still being readable to non-technical staff such as managers" - Original design goal
// - "COBOL: A weak, verbose, and flabby language used by code grinders" - The Jargon File
// - "If it ain't broke, don't fix it" - Every bank CTO about their COBOL systems
// - "You can write FORTRAN in any language" - Also true for COBOL
// - "The (grand)mother of COBOL" - Grace Hopper's nickname
// - "COBOL isn't dead - it's just resting on a mainframe" - Every COBOL programmer
//
// NOTES ON COBOL SYNTAX
// ---------------------
// - Comments use * in column 7 (or at start of line in free format)
// - Case-insensitive language (traditionally all uppercase)
// - Hyphenated variable names very common (FIRST-VAR, GROUP-VAR)
// - Level numbers (01-49, 66, 77, 88) define data structure hierarchy
// - PIC/PICTURE clauses define data types and formats
// - Period (.) is statement terminator - CRITICAL!
// - Four main divisions: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
// - English-like verb-noun syntax
// - Self-documenting philosophy


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('cobol', {
  rules: [
    // PRIORITY 100: Comments (asterisk in column 7 or entire line)
    {
      class: 'comment',
      pattern: /^\s*\*.*/gm,
      priority: 100
    },

    // PRIORITY 90: String literals
    {
      class: 'string',
      pattern: /"[^"]*"/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /'[^']*'/g,
      priority: 90
    },

    // PRIORITY 85: Division headers (highest level structure)
    {
      class: 'keyword',
      pattern: /\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b/gi,
      priority: 85
    },

    // PRIORITY 80: Section headers
    {
      class: 'keyword',
      pattern: /\b(CONFIGURATION|INPUT-OUTPUT|FILE|WORKING-STORAGE|LINKAGE|LOCAL-STORAGE)\s+SECTION\b/gi,
      priority: 80
    },

    // PRIORITY 75: Special Keywords like PROGRAM-ID, FUNCTION-ID
    {
      class: 'keyword',
      pattern: /\b(PROGRAM-ID|FUNCTION-ID)\b/gi,
      priority: 75
    },

    // PRIORITY 70: COBOL Paragraph/Section Names (Procedure Division labels)
    // These end with a period and are often on their own line.
    {
      class: 'function',
      pattern: /^\s*([A-Z0-9-]+)\s*\.\s*$/gmi,
      priority: 70
    },

    // PRIORITY 68: Level numbers (01-49, 66, 77, 88)
    {
      class: 'number',
      pattern: /^\s*(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33|34|35|36|37|38|39|40|41|42|43|44|45|46|47|48|49|66|77|88)\s+/gm,
      priority: 68
    },

    // PRIORITY 67: COBOL Data-names (variables) in declarations (after level numbers)
    {
      class: 'variable',
      pattern: /(?<=(?:0[1-9]|[1-4][0-9]|66|77|88)\s+)([A-Z0-9-]+)\b(?=\s*(PIC|PICTURE|VALUE|VALUES|OCCURS|REDEFINES|FILLER|\.|\n|$))/gi,
      priority: 67
    },

    // PRIORITY 66: Condition Names (88 level)
    {
      class: 'boolean',
      pattern: /(?<=\b88\s+)([A-Z0-9-]+)\b/gi,
      priority: 66
    },

    // PRIORITY 55: Variables used in PROCEDURE DIVISION (hyphenated names)
    // This matches hyphenated variable names like FIRST-VAR, GROUP-VAR
    // Must come BEFORE keywords to prevent partial matches
    {
      class: 'variable',
      pattern: /\b[A-Z0-9]+-[A-Z0-9-]+\b/gi,
      priority: 55
    },

    // PRIORITY 50: Major keywords
    {
      class: 'keyword',
      pattern: /\b(ACCEPT|ACCESS|ADD|ADDRESS|ADVANCING|AFTER|ALL|ALPHABET|ALPHABETIC|ALPHABETIC-LOWER|ALPHABETIC-UPPER|ALPHANUMERIC|ALPHANUMERIC-EDITED|ALSO|ALTER|ALTERNATE|AND|ANY|ARE|AREA|AREAS|ASCENDING|ASSIGN|AT|AUTHOR|BEFORE|BINARY|BLANK|BLOCK|BOTTOM|BY|CALL|CANCEL|CD|CF|CH|CHARACTER|CHARACTERS|CLASS|CLOCK-UNITS|CLOSE|COBOL|CODE|CODE-SET|COLLATING|COLUMN|COMMA|COMMON|COMMUNICATION|COMP|COMP-3|COMPUTATIONAL|COMPUTATIONAL-3|COMPUTE|CONTAINS|CONTENT|CONTINUE|CONTROL|CONTROLS|CONVERTING|COPY|CORR|CORRESPONDING|COUNT|CURRENCY|DATE|DATE-COMPILED|DATE-WRITTEN|DAY|DAY-OF-WEEK|DE|DEBUG-CONTENTS|DEBUG-ITEM|DEBUG-LINE|DEBUG-NAME|DEBUG-SUB-1|DEBUG-SUB-2|DEBUG-SUB-3|DEBUGGING|DECIMAL-POINT|DECLARATIVES|DELETE|DELIMITED|DELIMITER|DEPENDING|DESCENDING|DESTINATION|DETAIL|DISABLE|DISPLAY|DIVIDE|DOWN|DUPLICATES|DYNAMIC|EGI|ELSE|EMI|ENABLE|END|END-ADD|END-CALL|END-COMPUTE|END-DELETE|END-DIVIDE|END-EVALUATE|END-IF|END-MULTIPLY|END-OF-PAGE|END-PERFORM|END-READ|END-RECEIVE|END-RETURN|END-REWRITE|END-SEARCH|END-START|END-STRING|END-SUBTRACT|END-UNSTRING|END-WRITE|ENTER|ENTRY|EOP|EQUAL|ERROR|ESI|EVALUATE|EVERY|EXCEPTION|EXIT|EXTEND|EXTERNAL|FALSE|FD|FILE-CONTROL|FILLER|FINAL|FIRST|FOOTING|FOR|FROM|GENERATE|GIVING|GLOBAL|GO|GREATER|GROUP|HEADING|HIGH-VALUE|HIGH-VALUES|I-O|I-O-CONTROL|IF|IN|INDEX|INDEXED|INDICATE|INITIAL|INITIALIZE|INITIATE|INPUT|INSPECT|INSTALLATION|INTO|INVALID|IS|JUST|JUSTIFIED|KEY|LABEL|LAST|LEADING|LEFT|LENGTH|LESS|LIMIT|LIMITS|LINAGE|LINAGE-COUNTER|LINE|LINES|LOCK|LOW-VALUE|LOW-VALUES|MEMORY|MERGE|MESSAGE|MODE|MODULES|MOVE|MULTIPLE|MULTIPLY|NATIVE|NEGATIVE|NEXT|NO|NOT|NUMBER|NUMERIC|NUMERIC-EDITED|OBJECT-COMPUTER|OCCURS|OF|OFF|OMITTED|ON|OPEN|OPTIONAL|OR|ORDER|ORGANIZATION|OTHER|OUTPUT|OVERFLOW|PACKED-DECIMAL|PADDING|PAGE|PAGE-COUNTER|PERFORM|PF|PH|PIC|PICTURE|PLUS|POINTER|POSITION|POSITIVE|PRINTING|PROCEDURE|PROCEDURES|PROCEED|PROGRAM|PURGE|QUEUE|QUOTE|QUOTES|RANDOM|RD|READ|RECEIVE|RECORD|RECORDS|REDEFINES|REEL|REFERENCE|REFERENCES|RELATIVE|RELEASE|REMAINDER|REMOVAL|RENAMES|REPLACE|REPLACING|REPORT|REPORTING|REPORTS|RERUN|RESERVE|RESET|RETURN|REVERSED|REWIND|REWRITE|RF|RH|RIGHT|ROUNDED|RUN|SAME|SD|SEARCH|SECTION|SECURITY|SEGMENT|SEGMENT-LIMIT|SELECT|SEND|SENTENCE|SEPARATE|SEQUENCE|SEQUENTIAL|SET|SIGN|SIZE|SORT|SORT-MERGE|SOURCE|SOURCE-COMPUTER|SPACE|SPACES|SPECIAL-NAMES|STANDARD|STANDARD-1|STANDARD-2|START|STATUS|STOP|STRING|SUB-QUEUE-1|SUB-QUEUE-2|SUB-QUEUE-3|SUBTRACT|SUM|SUPPRESS|SYMBOLIC|SYNC|SYNCHRONIZED|TABLE|TALLYING|TAPE|TERMINAL|TERMINATE|TEST|TEXT|THAN|THEN|THROUGH|THRU|TIME|TIMES|TO|TOP|TRAILING|TRUE|TYPE|UNIT|UNSTRING|UNTIL|UP|UPON|USAGE|USE|USING|VALUE|VALUES|VARYING|WHEN|WITH|WORDS|WORKING-STORAGE|WRITE|ZERO|ZEROES|ZEROS)\b/gi,
      priority: 50
    },

    // PRIORITY 45: Picture clause
    {
      class: 'builtin',
      pattern: /\bPIC(?:TURE)?\s+[X9ASV\(\)\.\+\-\$\*Z,]+/gi,
      priority: 45
    },

    // PRIORITY 30: Special constants (ZERO, SPACE, etc.)
    {
      class: 'boolean',
      pattern: /\b(ZERO|ZEROS|ZEROES|SPACE|SPACES|HIGH-VALUE|HIGH-VALUES|LOW-VALUE|LOW-VALUES|QUOTE|QUOTES|NULL|NULLS)\b/gi,
      priority: 30
    },

    // PRIORITY 25: Numeric literals (including negative numbers and decimals)
    // Matches: 123, -123, 123.45, -123.45, +123.45
    {
      class: 'number',
      pattern: /\b[+\-]?\d+\.?\d*\b/g,
      priority: 25
    },

    // PRIORITY 10: Generic single-word identifiers (lowest priority)
    // This catches anything else that might be a variable but isn't hyphenated
    {
      class: 'variable',
      pattern: /\b([A-Z][A-Z0-9]*)\b/g,
      priority: 10
    }
  ]
});