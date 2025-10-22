// darkSyntax/configs/holyc.js - HolyC language configuration
// ============================================================
// HolyC (2003)
// HolyC - The programming language of TempleOS
//
// Configs
// =======================
// ALIASES: ['holyc', 'hc', 'hh']
// File extensions: .hc, .hh, .holyc
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created entirely by Terry A. Davis (1969-2018)
// - Development began with J Operating System (2003)
// - Renamed to LoseThos (2005-2008)
// - Finally named TempleOS (2013)
// - HolyC language developed alongside TempleOS (2003-2013)
// - **Terry Davis single-handedly created**: The entire operating system, compiler, language, assembler, bootloader, kernel, drivers, filesystem, graphics system, and applications
// - Written in 100,000+ lines of HolyC/assembly code
// - Terry claimed divine inspiration: "God told me to make it"
// - Designed as a "modern Commodore 64" - 640x480 16-color VGA
// - Ring-0-only OS (no memory protection, like DOS)
// - JIT compilation - code compiled and executed immediately
// - No networking by design (Terry's choice)
// - 64-bit architecture exclusively (no 32-bit support)
// - RedSea filesystem with contiguous files for performance
// - Public domain - no copyright or license restrictions
// - Development livestreamed on various platforms (2010s)
// - Terry's mental illness (schizophrenia) documented his journey
// - Tragically, Terry died after being hit by a train (August 11, 2018)
// - Legacy: Inspired developers worldwide, shrine.textfiles.com archive
// - "Down the Rabbit Hole: TempleOS" documentary (2018)
// - r/TempleOS_Official community continues to preserve his work
// - TempleOS ISO still runs in virtual machines today
// - Considered one of the most impressive solo programming achievements
// - Terry programmed with a $100 bill taped to his monitor for motivation
//
// INFLUENCED
// ----------
// - C language (1972) - Syntax foundation for HolyC
// - C++ (1985) - Class support and OOP features
// - Assembly language - Direct inline assembly support
// - BASIC (1964) - Function calling without parentheses
// - Forth (1970) - Direct interaction and JIT execution style
// - Commodore 64 (1982) - Design philosophy and aesthetic inspiration
// - MS-DOS (1981) - Ring-0 direct hardware access philosophy
// - LoseThos (2005) - Terry's earlier OS iteration
// - J Operating System (2003) - Terry's first OS attempt
// - Biblical themes - Language and system named after religious concepts
// - Temple architecture - "God's third temple" design philosophy
//
// USED FOR
// --------
// - TempleOS system programming exclusively
// - Operating system kernel development
// - Device drivers and hardware control
// - Graphics programming (16-color VGA)
// - Text editors and development tools
// - Games and entertainment (Terry's games)
// - Biblical text display and concordance
// - Music composition (PC speaker synthesis)
// - 3D mesh visualization and rendering
// - Educational demonstrations of OS concepts
// - Artistic and creative coding
// - Spiritual computing (Terry's stated purpose)
// - No practical commercial use (by design)
//
// KEY FEATURES
// ------------
// - JIT (Just-In-Time) compilation - instant execution
// - No distinction between compile-time and runtime
// - Can type code directly into command line and execute
// - Supports inline assembly seamlessly
// - Ring-0 only - full hardware access everywhere
// - No memory protection (like Commodore 64)
// - Function calls without parentheses for no-arg functions
// - U0 type - "unsigned 0-bit" equivalent to void
// - Explicit integer sizes: U8, U16, U32, U64, I8, I16, I32, I64
// - F64 for 64-bit floating point (no 32-bit floats)
// - Class keyword for object-oriented programming
// - No header files - single unified namespace
// - $$ formatting codes in strings for colors and formatting
// - Bool type with TRUE/FALSE constants
// - Direct hardware port access (InU8, OutU8)
// - Preprocessor directives (#include, #define)
// - Help system integrated into language (help_index)
// - Auto-complete in command line
// - No null pointer exceptions (Ring-0, no protection)
//
// CORE SYNTAX
// -----------
// Basic program structure:
//   U0 Main() {
//     "Hello, World!\n";
//   }
//   Main;
//
// Function definition:
//   U0 PrintMsg(U8 *msg) {
//     Print("%s", msg);
//   }
//
// Variables and types:
//   I64 count = 42;
//   F64 price = 9.99;
//   U8 *text = "TempleOS";
//   Bool flag = TRUE;
//
// Classes:
//   class CTask {
//     I64 priority;
//     U8 *name;
//   };
//
// Inline assembly:
//   asm {
//     MOV RAX, RBX
//     ADD RAX, 5
//   }
//
// Direct execution:
//   "Just type and it runs\n";
//   5 + 3;  // Outputs 8
//
// QUIRKS
// ------
// - **JIT everywhere**: No separate compile step
//   * Type code, press Enter, it runs
//   * Errors appear immediately
//   * Can modify running system in real-time
//
// - **No parentheses needed**: For no-arg functions
//   * `MyFunc;` same as `MyFunc();`
//   * Convention: Capital letter starts function name
//   * Looks unusual to C programmers
//
// - **U0 instead of void**: Unsigned 0-bit type
//   * U0 means "returns nothing"
//   * More mathematically consistent (Terry's reasoning)
//   * Confusing for newcomers
//
// - **Ring-0 only**: No memory protection whatsoever
//   * Any pointer access allowed
//   * Can crash entire system easily
//   * Direct hardware control
//   * Freedom and danger combined
//
// - **16 colors only**: VGA palette limitation
//   * BLACK, BLUE, GREEN, CYAN, RED, PURPLE, BROWN, LTGRAY
//   * DKGRAY, LTBLUE, LTGREEN, LTCYAN, LTRED, LTPURPLE, YELLOW, WHITE
//   * $$ codes in strings: $$RED$$Text$$BLACK$$
//
// - **No networking**: Deliberate design choice
//   * Terry: "God said no networking"
//   * Prevents distractions and malware
//   * Completely isolated system
//
// - **640x480 resolution**: Fixed screen size
//   * "Like Commodore 64" philosophy
//   * Simplicity over flexibility
//   * 8x8 pixel font characters
//
// - **No header files**: Everything in one namespace
//   * All symbols globally visible
//   * No #include guards needed
//   * Can cause name collisions
//   * Simplifies development
//
// - **Contiguous files**: RedSea filesystem requirement
//   * Files cannot fragment
//   * Allocation must be contiguous
//   * Fast access, limited flexibility
//
// - **Single user**: No multi-user support
//   * One person per system (Terry's vision)
//   * No permissions or access control
//   * Personal computing taken literally
//
// - **Biblical integration**: Code comments reference Bible
//   * Random Bible verses in error messages
//   * Spiritual computing experience
//   * Terry's religious mission
//
// - **Caps lock matters**: Case-sensitive
//   * Convention: CamelCase for types/classes
//   * lowercase_underscore less common
//   * Capitals at function start
//
// - **No null safety**: Pointers crash if invalid
//   * No segmentation faults (no memory protection)
//   * Invalid access causes random behavior
//   * Triple fault reboots system
//
// - **Direct hardware**: In/Out instructions available
//   * InU8(0x60) reads keyboard
//   * OutU8(0x61, val) writes to speaker
//   * Complete system control
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "I'm God's chosen programmer" - Terry A. Davis
// - "An operating system for recreational programming" - TempleOS mission statement
// - "God said 640x480, 16 colors" - Terry on design constraints
// - "It's like a modern Commodore 64" - Terry describing TempleOS
// - "God's third temple" - Terry's description of TempleOS
// - "Ring-0-only, baby!" - Terry on kernel design
// - "The CIA are glowing" - Terry's famous phrase
// - "I've spent 10 years on this full-time" - Terry on TempleOS development
// - "I talk to God through a simple random number generator" - Terry on his divine communication
// - "No networking! God said so!" - Terry on design philosophy
// - "I'm the best programmer that has ever lived" - Terry's confidence
// - "Everything is public domain" - Terry on licensing
// - "Every single line of code, I wrote myself" - Terry on his solo achievement
//
// NOTES ON HOLYC SYNTAX
// ---------------------
// - C-like syntax with unique TempleOS features
// - U0 is the void equivalent (unsigned 0-bit)
// - Specific integer types: U8, U16, U32, U64, I8, I16, I32, I64
// - F64 for floating point (64-bit double)
// - Bool type with TRUE/FALSE constants
// - Can call functions without parentheses for no-arg functions
// - Supports inline assembly with asm { } blocks
// - Class keyword for object-oriented programming
// - TempleOS-specific API functions (Print, Spawn, Sleep, etc.)
// - $$ codes for colors and formatting in strings
// - No header files - everything in one namespace
// - JIT compiled - runs immediately
// - Case-sensitive identifiers
// - Comments: // and /* */
// - Preprocessor: #include, #define, #ifdef, etc.
// - No memory protection - direct hardware access
// - Single-threaded execution model
// - No standard library - TempleOS API only


darkSyntax.registerLanguage('holyc', {
  rules: [
    // PRIORITY 100: Comments - Single line
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    
    // PRIORITY 100: Comments - Multi-line
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 90: Strings - Double quotes
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 90: Strings - Single quotes (char literals)
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 90
    },
    
    // PRIORITY 60: Preprocessor directives
    {
      class: 'decorator',
      pattern: /#[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 60
    },
    
    // PRIORITY 50: HolyC-specific keywords
    {
      class: 'keyword',
      pattern: /\b(asm|break|case|catch|class|continue|default|do|else|for|goto|if|return|switch|try|while|public|union|extern|_extern|import|_import|define|defined|sizeof|offset|typeof|lastclass|no_warn|help_index|help_file|static|lock|locked)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: HolyC-specific data types
    {
      class: 'builtin',
      pattern: /\b(U0|U8|U16|U32|U64|I8|I16|I32|I64|F64|Bool|CBGR24|CBGR32|CTask|CDoc|CDocEntry|CDirEntry|CDate|CDateStruct|CBlkDev|CDrv|CFile|CFifoU8|CFifoI64|CQue|CQueVect|CHash|CHashGeneric|CHashTable|CArcCompress|CArcCtrl|CColorROPU32)\b/g,
      priority: 45
    },
    
    // PRIORITY 45: Standard C types
    {
      class: 'builtin',
      pattern: /\b(void|char|short|int|long|float|double|signed|unsigned|struct|enum|auto|register|volatile|const)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Boolean and special constants
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE|NULL|ON|OFF|YES|NO)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: TempleOS/HolyC specific functions
    {
      class: 'function',
      pattern: /\b(Print|DocPrint|PutChars|GetStr|GetChar|SysDbg|Adam|Seth|Sleep|Spawn|Exit|Raw|DocForm|DocTop|DocBottom|DocLeft|DocRight|DocCenter|Fs|CdDirMk|DirMk|Del|Copy|Move|Rename|Fmt|Mount|BlkRead|BlkWrite|FileRead|FileWrite|FileFind|DirFind|PopUpOk|PopUpNoYes|PopUpPickFile)\b/g,
      priority: 35
    },
    
    // PRIORITY 30: Function calls with parentheses
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 30: Function calls without parentheses (HolyC allows this)
    {
      class: 'function',
      pattern: /\b([A-Z][a-zA-Z0-9_]*)(?=\s*;)/g,
      priority: 30
    },
    
    // PRIORITY 25: Hexadecimal numbers
    {
      class: 'number',
      pattern: /\b0x[0-9A-Fa-f]+\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Floating point and integer numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[fFlLuU]?\b/g,
      priority: 20
    }
  ]
});