// darkSyntax/configs/commodore64-basic.js - Commodore 64 BASIC V2 configuration
// ===============================================================================
// Commodore 64 BASIC V2 (1982)
// Hardware-specific BASIC for the legendary Commodore 64
//
// Configs
// =======================
// ALIASES: ['commodore64-basic', 'c64-basic', 'cbm-basic']
// File extensions: .bas, .prg
// Target: Commodore 64 (MOS 6510 CPU, VIC-II graphics, SID sound chip)
//
// COMMODORE 64 BASIC V2 SYNTAX NOTES
// ==================================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Launched with Commodore 64 in August 1982
// - Based on Microsoft BASIC from 1977 (licensed by Commodore)
// - Only 8KB ROM - extremely limited compared to competitors
// - No built-in graphics or sound commands (forced use of POKE/PEEK)
// - Sold on 17 million+ Commodore 64 computers (best-selling computer ever)
// - Remained unchanged throughout C64's entire production (1982-1994)
// - Created massive market for BASIC extension cartridges (Simons' BASIC, etc.)
// - Taught millions of kids programming through type-in magazines
//
// INFLUENCED
// ----------
// - BASIC 3.5 (1984) - Commodore Plus/4, added graphics/sound commands
// - BASIC 7.0 (1985) - Commodore 128, full structured programming
// - Super BASIC (1985) - Commercial C64 BASIC extension
// - Simons' BASIC (1983) - Popular C64 BASIC cartridge with 114 commands
// - The Freeze Machine (1984) - Machine code monitor + BASIC toolkit
// - Type-in magazine culture - Compute!, RUN, Ahoy!, etc.
//
// USED FOR
// --------
// - Home computing and education (1980s)
// - Game development (early C64 games, before ML dominated)
// - Learning programming (millions of first programmers)
// - Utilities and tools (file copiers, disk utilities)
// - Business applications (simple databases, inventory)
// - Sound and graphics demos (via POKE to SID and VIC-II)
// - Modern retro computing and demo scene
//
// KEY FEATURES
// ------------
// - Memory-mapped I/O: All hardware access via PEEK/POKE
// - POKE: Write byte to any memory address (0-65535)
// - PEEK: Read byte from any memory address
// - SYS: Call machine language routine at address
// - No graphics commands: Must POKE VIC-II chip (53248-53294)
// - No sound commands: Must POKE SID chip (54272-54296)
// - TI/TI$: Jiffy clock (1/60 second timer)
// - GET/GET#: Read single character (keyboard or file)
// - Line numbers mandatory: 0-63999
// - Immediate mode: Commands execute instantly without line number
//
// CORE SYNTAX
// -----------
// Memory Access:
//   POKE 53280,0        : Set border to black
//   POKE 53281,6        : Set background to blue  
//   X = PEEK(53280)     : Read border color
//   SYS 64738           : Soft reset (call ML routine)
//
// Graphics (via POKE to VIC-II):
//   POKE 53280,C        : Border color (C=0-15)
//   POKE 53281,C        : Background color
//   POKE 646,C          : Text color
//   POKE 53265,PEEK(53265) OR 32  : Enable bitmap mode
//
// Sound (via POKE to SID chip):
//   POKE 54296,15       : Volume max
//   POKE 54273,17       : Frequency low byte (voice 1)
//   POKE 54274,64       : Frequency high byte
//   POKE 54276,17       : Attack/Decay
//   POKE 54277,0        : Sustain/Release
//   POKE 54278,16       : Waveform (triangle)
//   POKE 54278,17       : Gate on (start sound)
//   POKE 54278,16       : Gate off (stop sound)
//
// Input:
//   GET K$              : Non-blocking keyboard read
//   INPUT "NAME";N$     : Blocking input
//   POKE 198,0          : Clear keyboard buffer
//
// Timing:
//   TI                  : Jiffies since power-on (1/60 sec)
//   TI$                 : Time as "HHMMSS" string
//
// QUIRKS
// ------
// - **POKE/PEEK hell**: No abstraction for hardware
//   * Want to change screen color? POKE 53281,6
//   * Want sound? POKE 20+ SID registers in correct order
//   * Wrong POKE = crash or corrupted memory!
// - **Memory map madness**: Must memorize critical addresses
//   * 53280: Border color
//   * 53281: Background color  
//   * 54272-54296: SID sound chip
//   * 53248-53294: VIC-II graphics chip
//   * 56576: Memory banking
// - **Only 38KB free RAM**: BASIC uses 1024-40959
//   * Screen memory: 1024-2023 (default)
//   * BASIC program: 2048-40959
//   * High memory reserved for BASIC interpreter and KERNAL
// - **No sprite commands**: Must POKE 8 bytes per sprite definition
//   * 63 bytes for sprite data, plus POKEs to enable/position
//   * Sprite collisions read via PEEK
// - **No line drawing**: Plot pixels one at a time via POKE
//   * Calculate bitmap address: 8192 + (Y AND 248)*40 + (X AND 504)
//   * Then POKE the bit pattern
// - **Slow interpreter**: 10x slower than compiled code
//   * FOR loops are glacial
//   * Games often mixed BASIC + machine language (SYS calls)
// - **TI$ quirk**: Reads as "HHMMSS" but sets differently
//   * TI$ = "123456" sets to 12:34:56
//   * But TI$ always reads with leading zeros: "012345"
// - **String memory leak**: No garbage collection until out of memory
//   * String operations fragment memory
//   * FRE(0) forces garbage collection
// - **Cassette tape support**: LOAD/SAVE work with datasette
//   * LOAD "PROGRAM",1 loads from tape
//   * LOAD "PROGRAM",8 loads from disk
// - **No ELSE**: IF...THEN only, no ELSE clause
//   * IF X=1 THEN PRINT "YES": REM no else!
// - **No WHILE/REPEAT**: Only FOR...NEXT and GOTO loops
// - **CHR$(147) magic**: CHR$(147) clears screen
//   * PRINT CHR$(147) same as typing SHIFT+CLR/HOME
// - **Power-on message**: Shows 38911 BASIC BYTES FREE
//   * Different on NTSC (38911) vs PAL (38907)
//
// FAMOUS QUOTES & SAYINGS  
// -----------------------
// - "POKE 53280,0: POKE 53281,0" - Every C64 kid's first program
// - "?SYNTAX ERROR IN 10" - The most-seen error message
// - "10 PRINT CHR$(205.5+RND(1)); : GOTO 10" - Famous one-line maze generator
// - "LOAD \"*\",8,1" - Loading the first program on your new C64
// - "Compute's Gazette taught me more than school ever did" - Every C64 programmer
//
// NOTES ON C64 BASIC SYNTAX
// --------------------------
// - Mandatory line numbers (0-63999)
// - Keywords are tokenized (1 byte per keyword)
// - Case-insensitive with PETSCII character set
// - Abbreviations: ? for PRINT, P. for POKE, etc.
// - Memory addresses always 0-65535
// - Hardware access only via POKE/PEEK/SYS
// - Graphics/sound require direct chip programming
// - No structured programming (only GOTO/GOSUB)
//
// SAMPLE CODE
// -----------
/*

*/
//
/*
10 REM SIMPLE GAME - AVOID THE WALLS
20 POKE 53280,6: POKE 53281,14
30 PRINT CHR$(147)
40 P=1024+20*40+10: REM PLAYER POSITION
50 POKE P,81: REM PLAYER CHAR
60 POKE 55296+(P-1024),1: REM WHITE COLOR
70 REM
80 REM GAME LOOP
90 GET K$
100 IF K$="W" THEN P=P-40
110 IF K$="S" THEN P=P+40
120 IF K$="A" THEN P=P-1
130 IF K$="D" THEN P=P+1
140 REM
150 REM CHECK COLLISION
160 IF PEEK(P)=160 THEN 90: REM SPACE OK
170 REM HIT WALL
180 PRINT "GAME OVER!": END
*/
//
/*
10 REM MUSIC PLAYER - SIMPLE MELODY
20 POKE 54296,15: REM VOLUME
30 FOR N=1 TO 10
40   READ FL,FH,D
50   POKE 54273,FL: POKE 54274,FH
60   POKE 54276,8: POKE 54277,0
70   POKE 54278,17: REM TRIANGLE ON
80   FOR W=1 TO D*10: NEXT W
90   POKE 54278,16: REM OFF
100 NEXT N
110 REM FREQ DATA: LOW,HIGH,DURATION
120 DATA 17,64,10
130 DATA 23,71,10
140 DATA 28,80,10
150 DATA 33,90,10
160 DATA 39,101,20
*/


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================

darkSyntax.registerLanguage('commodore64-basic', {
  showLineNumbers: false,
  rules: [
    // PRIORITY 100: Comments (REM only - no ' in C64 BASIC V2)
    {
      class: 'comment',
      pattern: /\bREM\b.*$/gim,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 80: Line numbers (mandatory in C64 BASIC)
    {
      class: 'decorator',
      pattern: /^\s*\d+\s+/gm,
      priority: 80
    },
    
    // PRIORITY 75: Hardware-specific commands (POKE, PEEK, SYS)
    {
      class: 'builtin',
      pattern: /\b(POKE|PEEK|SYS|WAIT|USR)\b/gi,
      priority: 75
    },
    
    // PRIORITY 70: I/O commands
    {
      class: 'keyword',
      pattern: /\b(PRINT|INPUT|GET|OPEN|CLOSE|CMD|LOAD|SAVE|VERIFY)\b/gi,
      priority: 70
    },
    
    // PRIORITY 65: Control flow
    {
      class: 'keyword',
      pattern: /\b(IF|THEN|FOR|TO|STEP|NEXT|GOTO|GOSUB|RETURN|ON|END|STOP|RUN|CONT)\b/gi,
      priority: 65
    },
    
    // PRIORITY 60: Data and variables
    {
      class: 'keyword',
      pattern: /\b(DIM|DATA|READ|RESTORE|LET|CLR|NEW|LIST)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: File operations  
    {
      class: 'keyword',
      pattern: /\b(GET#|INPUT#|PRINT#)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Functions
    {
      class: 'builtin',
      pattern: /\b(ABS|ASC|ATN|CHR\$|COS|EXP|FN|FRE|INT|LEFT\$|LEN|LOG|MID\$|POS|RIGHT\$|RND|SGN|SIN|SPC|SQR|STR\$|TAB|TAN|VAL)\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: Timing functions (C64-specific)
    {
      class: 'builtin',
      pattern: /\b(TI|TI\$|TIME\$|TIME)\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Status and system
    {
      class: 'builtin',
      pattern: /\b(ST|STATUS|DEF)\b/gi,
      priority: 40
    },
    
    // PRIORITY 35: Logical operators
    {
      class: 'keyword',
      pattern: /\b(AND|OR|NOT)\b/gi,
      priority: 35
    },
    
    // PRIORITY 30: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([E][+-]?\d+)?\b/gi,
      priority: 30
    },
    
    // PRIORITY 25: Memory addresses (common C64 addresses)
    // Highlight well-known hardware addresses
    {
      class: 'number',
      pattern: /\b(53280|53281|646|53248|53249|53269|54272|54273|54274|54276|54277|54278|54296|56576|55296|64738|198)\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Variables with type suffixes
    {
      class: 'variable',
      pattern: /\b([A-Z][A-Z0-9]?)[$%]/gi,
      priority: 20
    },
    
    // PRIORITY 15: Labels (for GOTO/GOSUB)
    {
      class: 'function',
      pattern: /\b([A-Z][A-Z0-9]*)\s*(?=\()/gi,
      priority: 15
    }
  ]
});