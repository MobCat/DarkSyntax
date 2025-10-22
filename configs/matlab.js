// darkSyntax/configs/matlab.js - MATLAB configuration
// =====================================================
// MATLAB (1984)
// Matrix Laboratory - numerical computing and engineering language
//
// Configs
// =======================
// ALIASES: ['matlab', 'm']
// File extensions: .m
//
// .m - MATLAB script and function files
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Cleve Moler (1970s) - Originally FORTRAN library wrapper
// - MathWorks founded (1984) - Commercial development began
// - MATLAB 4 (1992) - Sparse matrices, GUI capabilities
// - Simulink integration (1990s) - Visual modeling and simulation
// - MATLAB 5 (1996) - Cell arrays, structures, multidimensional arrays
// - MATLAB 6 (R12, 2000) - Object-oriented programming support
// - MATLAB 7 (R14, 2004) - Nested functions, anonymous functions
// - MATLAB R2008a+ - Major UI improvements, handle graphics
// - MATLAB R2016b+ - Live scripts, app designer
// - Dominated academic research and engineering (especially control systems, signal processing)
// - Name: MATrix LABoratory - everything is a matrix
//
// INFLUENCED
// ----------
// - NumPy (1995) - Python numerical computing, similar array operations
// - Julia (2012) - Technical computing with MATLAB-like syntax
// - GNU Octave (1988) - Open-source MATLAB alternative
// - R (1993) - Statistical computing, vectorized operations
// - Mathematica (1988) - Symbolic math competitor
// - SciPy (2001) - Scientific Python libraries
//
// USED FOR
// --------
// - Academic research (universities worldwide)
// - Signal processing and communications
// - Control systems design
// - Image and video processing
// - Machine learning and deep learning (Deep Learning Toolbox)
// - Financial modeling and analysis
// - Computational biology and bioinformatics
// - Aerospace and automotive engineering
// - Test and measurement (instrument control)
//
// KEY FEATURES
// ------------
// - Everything is a matrix (scalars are 1x1 matrices)
// - Vectorized operations (no loops needed for element-wise operations)
// - Rich built-in mathematical functions
// - Powerful visualization and plotting
// - Interactive development environment (IDE)
// - Toolboxes for specialized domains
// - Live scripts with embedded output
// - Simulink for visual modeling
//
// CORE SYNTAX
// -----------
// - Variable assignment: x = 5, no declaration needed
// - Matrix creation: A = [1, 2, 3; 4, 5, 6] (semicolon separates rows)
// - Element access: A(1, 2) (1-indexed!)
// - Range notation: 1:10 creates [1, 2, ..., 10]
// - Colon operator: A(:, 2) gets all rows of column 2
// - Element-wise operators: .*, ./, .^
// - Matrix operators: *, /, ^
// - Function definition: function [out1, out2] = myFunc(in1, in2)
// - Anonymous functions: @(x) x^2
// - String comparison: strcmp, strcmpi
// - Logical indexing: A(A > 5) gets all elements greater than 5
//
// QUIRKS
// ------
// - **1-indexed arrays (not 0-indexed)**:
//   * A(1) is first element, A(0) is error
//   * Coming from C/Python/Java, this is jarring
//   * Historical decision from FORTRAN heritage
//   * end keyword for last index: A(end) gets last element
// - **Parentheses for both function calls and array indexing**:
//   * myFunc(5) calls function, A(5) indexes array
//   * No way to distinguish from syntax alone
//   * Context-dependent parsing
// - **Everything is a matrix**:
//   * Scalar 5 is actually 1x1 matrix
//   * No true primitive types
//   * Memory overhead but consistent semantics
// - **Semicolon suppresses output**:
//   * x = 5 displays "x = 5"
//   * x = 5; displays nothing
//   * Critical for preventing output spam in scripts
// - **Single quotes for strings and transpose**:
//   * 'hello' is a string (char array)
//   * A' is transpose (complex conjugate transpose)
//   * A.' is plain transpose (no conjugation)
//   * Confusing overload of apostrophe
// - **end has multiple meanings**:
//   * Ends function/if/for/while blocks
//   * Last index in array: A(end), A(2:end)
//   * Context-dependent keyword
// - **Column-major order**:
//   * Matrices stored column-first in memory
//   * A(:) linearizes column-by-column
//   * Opposite of C/Python row-major
//   * Important for performance
// - **Function files vs script files**:
//   * File with "function" keyword is function file
//   * File without is script (runs in base workspace)
//   * Function filename must match function name
//   * Confusing for beginners
// - **Workspace pollution**:
//   * Scripts run in caller's workspace
//   * Easy to accidentally overwrite variables
//   * Functions have their own workspace
// - **No return statement (usually)**:
//   * Assign to output variable names
//   * return keyword exists but rarely used (early exit only)
// - **Element-wise vs matrix operations**:
//   * A * B is matrix multiplication
//   * A .* B is element-wise multiplication
//   * Easy to forget the dot
// - **true/false are functions, not keywords**:
//   * true() returns logical 1, false() returns logical 0
//   * Can be shadowed by variables (bad practice)
// - **Strings are character arrays**:
//   * 'hello' is 1x5 char array
//   * String type added in R2016b but char arrays still common
//   * Concatenation: ['hello', ' ', 'world']
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Why use a for loop when you can vectorize?" - MATLAB philosophy
// - "Think in matrices" - Core MATLAB mindset
// - "MATLAB is slow" - Common complaint (but vectorized code is fast)
// - "Just add a semicolon" - Fix output spam
//
// NOTES ON MATLAB SYNTAX
// -------------------------
// - Comments: % for single line, %{ %} for block comments
// - Case-sensitive language
// - No type declarations (dynamically typed)
// - Arrays are 1-indexed (first element is A(1))
// - Semicolon at end of line suppresses output
// - Colon operator creates ranges: 1:10, 0:0.1:1
// - Transpose: A' (conjugate) or A.' (non-conjugate)
// - Continuation: ... continues line to next line
// - Tilde (~) ignores output: [~, maxIdx] = max(A)


// MATLAB SYNTAX CONFIGURATION FOR DARKSYNTAX
// ===========================================
darkSyntax.registerLanguage('matlab', {
  rules: [
    // Block comments %{ %}
    {
      class: 'comment',
      pattern: /%\{[\s\S]*?%\}/g,
      priority: 100
    },

    // Single-line comments
    {
      class: 'comment',
      pattern: /%.*$/gm,
      priority: 100
    },

    // Transpose operator (complex conjugate) - after variable/closing bracket
    {
      class: 'operator',
      pattern: /(?<=[a-zA-Z0-9_)\]])\'/g,
      priority: 89
    },

    // Strings (single quotes) - must come after transpose to avoid conflict
    {
      class: 'string',
      pattern: /'(?:[^'\n]|'')*'/g,
      priority: 90
    },

    // Function definitions
    {
      class: 'keyword',
      pattern: /\bfunction\b/g,
      priority: 65
    },

    // Function names in definitions
    {
      class: 'function',
      pattern: /\bfunction\s+(?:\[[^\]]*\]\s*=\s*)?([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 64
    },

    // Keywords
    {
      class: 'keyword',
      pattern: /\b(break|case|catch|classdef|continue|else|elseif|end|for|function|global|if|otherwise|parfor|persistent|return|spmd|switch|try|while)\b/g,
      priority: 50
    },

    // Built-in constants
    {
      class: 'boolean',
      pattern: /\b(true|false|pi|inf|Inf|nan|NaN|eps|i|j)\b/g,
      priority: 47
    },

    // Built-in functions (common ones)
    {
      class: 'builtin',
      pattern: /\b(abs|acos|all|any|asin|atan|atan2|ceil|clc|clear|close|cos|delete|diag|diff|disp|double|end|error|eval|exist|exp|eye|fclose|feval|fft|figure|find|floor|fopen|fprintf|fread|fwrite|get|gradient|hist|hold|imread|imshow|input|int8|int16|int32|int64|interp1|inv|ischar|isempty|isequal|isfield|isinf|isnan|isnumeric|isreal|length|linspace|load|log|log10|logical|max|mean|median|meshgrid|min|mod|ndims|norm|numel|ones|open|plot|plot3|print|rand|randn|readtable|real|reshape|rgb2gray|round|save|set|sign|sin|single|size|sort|sqrt|std|strcmp|strcmpi|strfind|sum|tan|text|title|transpose|uint8|uint16|uint32|uint64|var|which|who|whos|xlabel|xlim|ylabel|ylim|zeros|zlabel)\b/g,
      priority: 45
    },

    // Function calls (before parens)
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },

    // Class definitions
    {
      class: 'class',
      pattern: /\b(classdef)\s+([A-Z][a-zA-Z0-9_]*)/g,
      captureGroup: 2,
      priority: 66
    },

    // Properties, methods, events keywords
    {
      class: 'keyword',
      pattern: /\b(properties|methods|events)\b/g,
      priority: 50
    },

    // Numbers (including scientific notation, hex)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?[ij]?\b/g,
      priority: 21
    },

    // Operators (including element-wise)
    {
      class: 'operator',
      pattern: /(?:\.\.\.|\.\*|\.\/|\.\\|\.\^|==|~=|<=|>=|&&|\|\||<<|>>|[+\-*\/\\^<>=~&|:@.])/g,
      priority: 58
    }
  ]
});