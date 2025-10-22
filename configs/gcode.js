// darkSyntax/configs/gcode.js - G-code language configuration
// =============================================================
// G-code (1958)
// Geometric Code - Numerical control programming language
//
// Configs
// =======================
// ALIASES: ['gcode', 'nc', 'cnc', 'ngc', 'g']
// File extensions: .gcode, .nc, .cnc, .ngc, .g
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created at MIT Servomechanisms Laboratory (1958)
// - Developed by William Pease and James McDonough
// - First implementation for numerical control (NC) machines
// - Originally used punched tape for data input (1940s-1950s)
// - John T. Parsons and Frank L. Stulen created first NC milling machine (1952)
// - Funded by U.S. Air Force for aviation industry precision machining
// - Replaced manual machining with automated motor control
// - Standardized as RS-274-D by Electronic Industries Alliance (1980)
// - ISO 6983-1 international standard for CNC programming
// - MIT's Whirlwind I computer used for early G-code development
// - APT (Automatically Programmed Tool) language developed by MIT (1956)
// - Patrick J. Hanratty created Pronto, early commercial NC language (1957)
// - First CNC machine press conference at MIT (1959) - aluminum ashtray demo
// - Originally just "connect the dots" - no loops or conditionals
// - Modern G-code includes macro capabilities and variables
// - Revolution in manufacturing automation and precision
// - RepRap project adapted G-code for 3D printing (2005)
// - Marlin firmware created for RepRap/3D printers (2011)
//
// INFLUENCED
// ----------
// - APT (Automatically Programmed Tool) (1956) - Higher-level NC language
// - CAM (Computer-Aided Manufacturing) software (1960s+)
// - CAD (Computer-Aided Design) systems (1960s+)
// - Marlin firmware (2011) - 3D printer G-code interpreter
// - RepRapFirmware (2014) - Advanced 3D printer firmware
// - Klipper (2016) - Modern 3D printer firmware with Python preprocessing
// - Grbl (2009) - Arduino-based CNC controller
// - Smoothieware (2011) - ARM-based CNC/3D printer firmware
// - LinuxCNC (2000) - Linux-based CNC control
// - Mach3/Mach4 (2001) - Windows CNC control software
// - Post-processors in CAM software (Fusion 360, Mastercam, etc.)
// - Slic3r, Cura, PrusaSlicer (2011+) - 3D printing slicers
//
// USED FOR
// --------
// - CNC milling and machining
// - CNC lathes and turning centers
// - 3D printing (FDM/FFF, SLA, SLS)
// - Laser cutting and engraving
// - Plasma cutting
// - Waterjet cutting
// - Wire EDM (Electrical Discharge Machining)
// - CNC routers for woodworking
// - PCB milling and drilling
// - Pick-and-place machines
// - Robotics and automation
// - Coordinate measuring machines (CMM)
//
// KEY FEATURES
// ------------
// - Simple line-by-line execution model
// - Commands specify motion, speed, and tool control
// - Modal commands (stay active until changed)
// - Cartesian (X, Y, Z) or polar coordinate systems
// - Support for multiple axes (up to 9)
// - Feed rates (F) control movement speed
// - Spindle/extruder speed control (S)
// - Tool selection and changing (T codes)
// - Coolant and auxiliary function control (M codes)
// - Arc interpolation (G2/G3) for curved paths
// - Coordinate system offsets (G54-G59)
// - Absolute (G90) vs relative (G91) positioning
// - Optional line numbers (N) and checksums (*)
//
// CORE SYNTAX
// -----------
// Basic movement:
//   G0 X10 Y20 Z5 F3000    ; Rapid move
//   G1 X50 Y30 Z2 F1500    ; Linear move
//
// Arc movement:
//   G2 X20 Y10 I5 J5       ; Clockwise arc
//   G3 X20 Y10 R10         ; Counterclockwise arc
//
// 3D printing specific:
//   M104 S200              ; Set extruder temp
//   M140 S60               ; Set bed temp
//   G28                    ; Home all axes
//   G29                    ; Auto bed leveling
//
// Tool control:
//   M3 S1000               ; Spindle on clockwise
//   M5                     ; Spindle off
//   T1                     ; Select tool 1
//
// QUIRKS
// ------
// - **Case insensitive**: G1 = g1, M104 = m104
//   * Convention: Uppercase in generated code
//   * Most controllers accept mixed case
//
// - **Modal commands**: Settings persist until changed
//   * G90 sets absolute mode for all future moves
//   * G91 sets relative mode until G90 called
//   * M82 sets absolute E (extruder) mode
//
// - **Multiple formats**: No single standard
//   * RS-274-D (EIA standard)
//   * ISO 6983 (international)
//   * Fanuc, Haas, Siemens, Heidenhain dialects
//   * RepRap/Marlin for 3D printing
//   * Each manufacturer adds extensions
//
// - **Parameter letters**: Single character before number
//   * X, Y, Z for position
//   * I, J, K for arc centers
//   * F for feed rate
//   * S for spindle/extruder speed
//   * E for extruder position (3D printing)
//
// - **No expressions**: Just numbers
//   * Cannot write X=10+5
//   * Must pre-calculate all values
//   * Modern variants add variables: #1=#2+#3
//
// - **Comment styles**: Three different types
//   * Semicolon: ; comment (most common)
//   * Parentheses: (comment)
//   * Hash/pound: # comment (slicer-generated)
//
// - **Line numbers**: Optional and inconsistent
//   * N10, N20, N30 for ordering
//   * Often omitted in modern G-code
//   * Used for program organization/debugging
//
// - **Checksums**: For error detection
//   * *56 at end of line
//   * Used in serial communication
//   * Prevents corrupted commands
//
// - **Whitespace flexible**: Spaces optional
//   * G1X10Y20 same as G1 X10 Y20
//   * Readability vs compactness tradeoff
//
// - **Decimal points**: Vary by controller
//   * Some require: X10.0
//   * Others allow: X10
//   * Leading zeros optional: .5 or 0.5
//
// - **Feed rates**: Units depend on mode
//   * G20: inches per minute
//   * G21: millimeters per minute (most common)
//   * Must match coordinate units
//
// - **3D printing extensions**: M codes for heating
//   * M104: Set extruder temperature
//   * M140: Set bed temperature
//   * M109: Wait for extruder temp
//   * M190: Wait for bed temp
//   * Not in original G-code standard
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "On March 11, 1958, a new era in manufacturing production was born" - MIT announcement
// - "The automation of machine tool control began in the 19th century with cams" - NC History
// - "Moving the process off of the highly unionized factory floor and into the non-unionized white collar design office" - David F. Noble, Forces of Production
// - "G-code began as a limited language that lacked constructs such as loops, conditional operators, and programmer-declared variables" - Wikipedia
// - "The first-ever G-code was developed at the MIT Servomechanisms Laboratory in 1958" - Manufacturing history
//
// NOTES ON G-CODE SYNTAX
// -----------------------
// - Case-insensitive language (G1 = g1)
// - Commands are letters followed by numbers: G1, M104, X10
// - Multiple commands can appear on one line
// - Comments: ; (semicolon), ( parentheses ), # (hash)
// - Line numbers: N10, N100 (optional)
// - Checksums: *56 at end of line (optional)
// - Parameters: Single letter + number (X10.5, F3000)
// - Modal commands stay active until changed
// - G-codes: Geometric motion and machine modes
// - M-codes: Miscellaneous machine functions
// - T-codes: Tool selection
// - Coordinates: X, Y, Z (primary axes)
// - Arc parameters: I, J, K (relative to start point)
// - Feed rate: F (speed of movement)
// - Spindle/extruder speed: S
// - Extruder position: E (3D printing)
// - Absolute mode: G90 (default)
// - Relative mode: G91
// - Units: G20 (inches), G21 (millimeters)


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('gcode', {
  rules: [
    // PRIORITY 100: Comments (hash/pound style - used by some slicers)
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 100: Comments (semicolon style)
    {
      class: 'comment',
      pattern: /;.*$/gm,
      priority: 100
    },
    
    // PRIORITY 100: Comments (parenthesis style)
    {
      class: 'comment',
      pattern: /\([^)]*\)/g,
      priority: 100
    },
    
    // PRIORITY 95: Template variables in square brackets
    {
      class: 'string',
      pattern: /\[[^\]]+\]/g,
      priority: 95
    },
    
    // PRIORITY 90: Strings (some G-code variants support quotes)
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
    
    // PRIORITY 70: Line numbers
    {
      class: 'decorator',
      pattern: /\bN\d+\b/gi,
      priority: 70
    },
    
    // PRIORITY 60: G-commands (motion and machine control)
    {
      class: 'keyword',
      pattern: /\bG\d+(\.\d+)?/gi,
      priority: 60
    },
    
    // PRIORITY 60: M-commands (miscellaneous commands)
    {
      class: 'keyword',
      pattern: /\bM\d+(\.\d+)?/gi,
      priority: 60
    },
    
    // PRIORITY 55: Custom macro/function calls (PRINT_START, PRINT_END, etc.)
    // Must be at least 3 chars and can't be a single letter followed by digits
    {
      class: 'function',
      pattern: /\b[A-Z_][A-Z_]{2,}(?=\s|$|=)/g,
      priority: 55
    },
    
    // PRIORITY 54: T-commands (tool selection) - just the T letter
    {
      class: 'variable',
      pattern: /\bT(?=\d)/gi,
      priority: 54
    },
    
    // PRIORITY 50: Coordinate parameter letters (X, Y, Z) - standalone or with values
    {
      class: 'variable',
      pattern: /\b[XYZ](?=\s|$|[-\d])/gi,
      priority: 50
    },
    
    // PRIORITY 45: Extruder parameter letter (E) - standalone or with value
    {
      class: 'variable',
      pattern: /\bE(?=\s|$|[-\d])/gi,
      priority: 45
    },
    
    // PRIORITY 40: Feed rate parameter letter (F)
    {
      class: 'variable',
      pattern: /\bF(?=\s|$|\d)/gi,
      priority: 40
    },
    
    // PRIORITY 38: Macro parameters (EXTRUDER=, BED=, etc.) - must be 2+ chars
    {
      class: 'function',
      pattern: /\b[A-Z_][A-Z_0-9]+(?==)/g,
      priority: 38
    },
    
    // PRIORITY 35: Speed parameter letter (S)
    {
      class: 'variable',
      pattern: /\bS(?=\s|$|\d)/gi,
      priority: 35
    },
    
    // PRIORITY 33: Other parameter letters (I, J, K for arcs, P, R, H, etc.)
    {
      class: 'variable',
      pattern: /\b[IJKPRDHABCUV](?=\s|$|[-\d])/gi,
      priority: 33
    },
    
    // PRIORITY 20: Standalone numbers (temperatures, percentages, etc.)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 20
    },
    
    // PRIORITY 10: Checksum indicator
    {
      class: 'decorator',
      pattern: /\*\d+/g,
      priority: 10
    }
  ]
});