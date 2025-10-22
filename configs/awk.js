// darkSyntax/configs/awk.js - AWK language configuration
// =======================================================
// AWK (1977-1979)
// AWK - Pattern scanning and text processing language
//
// Configs
// =======================
// ALIASES: ['awk', 'gawk', 'nawk', 'mawk']
// File extensions: .awk
//
// AWK SYNTAX NOTES
// ================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// AWK (1977-1979):
// - Created by Alfred Aho, Peter Weinberger, and Brian Kernighan at Bell Labs
// - Name is acronym of creators' last names: A-ho, W-einberger, K-ernighan
// - Published in "The AWK Programming Language" (1988)
// - One of the first successful domain-specific languages
// - Influenced by C, but designed for text processing
// - Original AWK (1977), nawk "new awk" (1985), GNU gawk (1986)
//
// INFLUENCED
// ----------
// - Perl (1987) - Larry Wall called it "awk + sed + sh + more"
// - Python - List comprehensions and text processing patterns
// - Tcl - String handling and pattern matching
// - Ruby - Text manipulation philosophy
// - Modern data pipelines and stream processing
//
// USED FOR
// --------
// - Text file processing and transformation
// - Log file analysis and reporting
// - Data extraction and reformatting
// - Quick one-liners for text manipulation
// - CSV/TSV processing
// - System administration scripts
// - Unix pipeline processing
//
// KEY FEATURES
// ------------
// - Pattern-action paradigm: pattern { action }
// - Automatic field splitting ($1, $2, $3...)
// - Built-in variables: NR, NF, FS, RS, OFS, ORS
// - Associative arrays (hash tables)
// - Regular expression matching: /pattern/
// - BEGIN/END blocks for initialization/cleanup
// - Minimal syntax - no type declarations
// - Automatic conversion between strings and numbers
//
// CORE SYNTAX
// -----------
// Pattern-action structure:
//   pattern { action }
//   /regex/ { print $1 }
//   NR > 1 { sum += $3 }
//
// Special patterns:
//   BEGIN { ... }  # Execute before any input
//   END { ... }    # Execute after all input
//   /pattern/      # Match lines with regex
//   $3 > 100       # Match lines where field 3 > 100
//   NR == 1        # Match first line
//
// Fields and variables:
//   $0      # Entire line
//   $1, $2  # First field, second field, etc.
//   NF      # Number of fields
//   NR      # Number of records (line number)
//   FS      # Field separator (default: whitespace)
//   RS      # Record separator (default: newline)
//   OFS     # Output field separator
//   ORS     # Output record separator
//
// Built-in functions:
//   length(s)      # String length
//   substr(s,i,n)  # Substring
//   split(s,a,fs)  # Split string into array
//   gsub(r,s,t)    # Global substitute
//   match(s,r)     # Match regex
//   printf()       # Formatted output
//   sin(), cos()   # Math functions
//
// Operators:
//   ~   # Match regex
//   !~  # Don't match regex
//   ++  # Increment
//   --  # Decrement
//   &&  # Logical AND
//   ||  # Logical OR
//
// Control structures:
//   if (condition) { ... } else { ... }
//   while (condition) { ... }
//   for (i in array) { ... }
//   for (i=0; i<10; i++) { ... }
//
// Arrays:
//   arr[key] = value    # Associative arrays
//   arr[1] = "first"    # Can use numeric keys
//   for (k in arr) { print k, arr[k] }
//
// QUIRKS
// ------
// - No explicit variable declarations
// - Variables are global by default
// - Uninitialized variables are "" or 0 depending on context
// - Strings and numbers auto-convert
// - Pattern without action prints the line: /pattern/
// - Action without pattern runs for every line: { print $1 }
// - Empty pattern matches all lines
//
// COMMON PATTERNS
// ---------------
// Sum column:        { sum += $3 } END { print sum }
// Print specific:    NR > 1 { print $2, $4 }
// Count matches:     /ERROR/ { count++ } END { print count }
// Field separator:   BEGIN { FS="," } { print $1 }
// Unique values:     !seen[$1]++ { print $1 }
//
// AWK SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================

darkSyntax.registerLanguage('awk', {
  rules: [
    // PRIORITY 100: Regular expression patterns (in slashes) - MUST come before comments!
    // Matches /pattern/ in various AWK contexts
    {
      class: 'string',
      pattern: /(^|[\s~!(,])(\/(?:[^\/\n\\]|\\.)+\/)/gm,
      captureGroup: 2,
      priority: 100
    },
    
    // PRIORITY 95: Comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 95
    },
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 85: Special patterns BEGIN/END
    {
      class: 'decorator',
      pattern: /\b(BEGIN|END)\b/g,
      priority: 85
    },
    
    // PRIORITY 80: Built-in variables
    {
      class: 'decorator',
      pattern: /\b(NF|NR|FNR|FS|RS|OFS|ORS|FILENAME|ARGC|ARGV|ENVIRON|RSTART|RLENGTH)\b/g,
      priority: 80
    },
    
    // PRIORITY 76: Field references ($1, $2, $NF, etc.) - Higher priority than numbers
    // Match $ followed by: numbers, NF, variables, or expressions in parens
    {
      class: 'variable',
      pattern: /\$[0-9]+|\$NF|\$\([^)]+\)|\$[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 76
    },
    
    // PRIORITY 70: Keywords and control structures
    {
      class: 'keyword',
      pattern: /\b(if|else|while|for|do|break|continue|return|delete|exit|next|nextfile|function|print|printf|getline)\b/g,
      priority: 70
    },
    
    // PRIORITY 65: Operators (including regex match operators)
    {
      class: 'keyword',
      pattern: /[~!]=?|&&|\|\||[<>]=?|[+\-*\/%^]=?|\+\+|--/g,
      priority: 65
    },
    
    // PRIORITY 60: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(length|substr|index|split|sub|gsub|match|sprintf|sin|cos|atan2|exp|log|sqrt|int|rand|srand|toupper|tolower|system|close|getline)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Function definitions and calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 55
    },
    
    // PRIORITY 50: Numbers (including decimals and scientific notation)
    // Use negative lookbehind to avoid matching digits after $
    {
      class: 'number',
      pattern: /(?<!\$)\b\d+\.?\d*([eE][+-]?\d+)?\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Variables (user-defined)
    {
      class: 'variable',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\[)/g,
      captureGroup: 1,
      priority: 45
    },
    
    {
      class: 'variable',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=[+\-*\/%<>=!]=)/g,
      captureGroup: 1,
      priority: 45
    },
    
    // PRIORITY 40: Boolean-like keywords
    {
      class: 'boolean',
      pattern: /\b(true|false)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: in operator (for arrays)
    {
      class: 'keyword',
      pattern: /\bin\b/g,
      priority: 35
    }
  ]
});