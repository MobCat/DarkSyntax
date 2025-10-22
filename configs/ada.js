// darkSyntax/configs/ada.js - Ada language configuration
// =======================================================
// Ada (1980-1983)
// Ada - High-integrity programming language
//
// Configs
// =======================
// ALIASES: ['ada', 'adb', 'ads']
// File extensions: .ada, .adb (body), .ads (spec)
//
// ADA SYNTAX NOTES
// ================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// Ada (1980-1983):
// - Created by Jean Ichbiah and team at CII Honeywell Bull (France)
// - Sponsored by U.S. Department of Defense (DoD)
// - Named after Ada Lovelace (1815-1852), first computer programmer
// - DoD wanted one language to replace 450+ languages used in defense
// - First standardized: Ada 83 (ANSI/MIL-STD-1815)
// - Major revisions: Ada 95, Ada 2005, Ada 2012, Ada 2022
// - First internationally standardized OOP language (Ada 95)
// - Reference number 1815 honors Ada Lovelace's birth year
//
// INFLUENCED
// ----------
// - Derived from Pascal, ALGOL 68, and Simula
// - Influenced: SPARK (formal verification subset)
// - Influenced: Java (packages, exceptions, generics concepts)
// - Influenced: C# (some type system concepts)
// - Influenced: Rust (strong type safety philosophy)
// - Influenced: High-integrity language design principles
//
// USED FOR
// --------
// - Aerospace and defense systems (F-22, F-35, Boeing 777, Airbus A340/380)
// - Air traffic control systems worldwide
// - Railway signaling and control (Paris Metro, London Underground)
// - Medical devices and nuclear power plants
// - Embedded real-time systems
// - Safety-critical applications where failure = death
// - Space programs (ESA, NASA Mars rovers, James Webb telescope)
//
// KEY FEATURES
// ------------
// - Strong static typing with compile-time checks
// - Explicit type conversions (no implicit coercion)
// - Packages for modularity (spec + body separation)
// - Generics (templates)
// - Exception handling
// - Tasking (built-in concurrency/multithreading)
// - Contract-based programming (preconditions, postconditions)
// - Range constraints and subtypes
// - Representation clauses (control memory layout)
// - No undefined behavior (unlike C/C++)
// - Designed for long-term maintainability
//
// CORE SYNTAX
// -----------
// Program structure:
//   procedure Main is
//      -- declarations
//   begin
//      -- statements
//   end Main;
//
// Types:
//   Integer, Float, Boolean, Character, String
//   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);  -- Enumeration
//   type Speed is range 0 .. 300;                     -- Constrained
//   type Matrix is array (1..10, 1..10) of Float;     -- Array
//   type Node;  -- Incomplete type (for pointers)
//   type Node_Ptr is access Node;                     -- Pointer
//
// Variables:
//   Count : Integer := 0;
//   Pi : constant Float := 3.14159;
//   Name : String := "Ada";
//
// Control structures:
//   if Condition then ... elsif ... else ... end if;
//   case Value is when 1 => ...; when 2..5 => ...; end case;
//   for I in 1 .. 10 loop ... end loop;
//   while Condition loop ... end loop;
//   loop ... exit when Condition; ... end loop;
//
// Procedures and Functions:
//   procedure Put_Line (Text : String);
//   function Square (X : Integer) return Integer is
//   begin
//      return X * X;
//   end Square;
//
// Packages:
//   package Math is
//      function Add (A, B : Integer) return Integer;
//   end Math;
//   
//   package body Math is
//      function Add (A, B : Integer) return Integer is
//      begin
//         return A + B;
//      end Add;
//   end Math;
//
// QUIRKS
// ------
// - Case-insensitive but convention is Mixed_Case_With_Underscores
// - Assignment is := (not =)
// - Equality is = (not ==)
// - Comments use -- (not //)
// - Strings use "double quotes" (chars use 'single')
// - Array indices start at any value (not just 0 or 1)
// - Strong typing: Integer and Float don't mix without conversion
// - No null-terminated strings (knows its length)
// - begin/end blocks everywhere (explicit scope)
// - Verbose but readable (designed for teams, not individuals)


// ADA SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================
darkSyntax.registerLanguage('ada', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /--.*$/gm,
      priority: 100
    },
    
    // PRIORITY 95: Strings (double quotes)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 95
    },
    
    // PRIORITY 90: Character literals (single quotes)
    {
      class: 'string',
      pattern: /'[^'\\]'|'\\[nrt0\\']'/g,
      priority: 90
    },
    
    // PRIORITY 85: Pragmas (compiler directives)
    {
      class: 'decorator',
      pattern: /\bpragma\s+[a-zA-Z_][a-zA-Z0-9_]*/gi,
      priority: 85
    },
    
    // PRIORITY 80: Keywords (control structures)
    {
      class: 'keyword',
      pattern: /\b(if|then|elsif|else|end|case|when|loop|for|while|exit|return|begin|declare|is|in|out|of|range|others|null|goto|raise|select|accept|entry|do|delay|until|reverse)\b/gi,
      priority: 80
    },
    
    // PRIORITY 75: Keywords (declarations and definitions)
    {
      class: 'keyword',
      pattern: /\b(procedure|function|package|body|type|subtype|task|protected|generic|new|use|with|renames|aliased|all|constant|access|record|array|at|mod|rem|abs|not|and|or|xor|private|limited)\b/gi,
      priority: 75
    },
    
    // PRIORITY 70: Built-in types
    {
      class: 'builtin',
      pattern: /\b(Integer|Natural|Positive|Float|Long_Float|Short_Float|Boolean|Character|String|Wide_Character|Wide_String|Duration|Address)\b/gi,
      priority: 70
    },
    
    // PRIORITY 65: Exception keywords
    {
      class: 'keyword',
      pattern: /\b(exception|raise|others)\b/gi,
      priority: 65
    },
    
    // PRIORITY 60: Standard exceptions
    {
      class: 'builtin',
      pattern: /\b(Constraint_Error|Program_Error|Storage_Error|Tasking_Error|Data_Error|Numeric_Error)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: Boolean literals
    {
      class: 'boolean',
      pattern: /\b(True|False)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Null keyword
    {
      class: 'boolean',
      pattern: /\bnull\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: Numbers (including based notation like 16#FF#)
    {
      class: 'number',
      pattern: /\b\d+#[0-9A-F]+#|\b\d+\.\d+([eE][+-]?\d+)?|\b\d+\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Attributes (Type'First, Array'Length, etc.)
    {
      class: 'decorator',
      pattern: /\b[a-zA-Z_][a-zA-Z0-9_]*'[a-zA-Z_][a-zA-Z0-9_]*/gi,
      priority: 40
    },
    
    // PRIORITY 35: Function/procedure definitions
    {
      class: 'function',
      pattern: /\b(procedure|function)\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi,
      captureGroup: 2,
      priority: 35
    },
    
    // PRIORITY 32: Function/procedure calls (but not attribute functions)
    {
      class: 'function',
      pattern: /(?<!')([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/gi,
      captureGroup: 1,
      priority: 32
    },
    
    // PRIORITY 30: Type definitions
    {
      class: 'class',
      pattern: /\btype\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi,
      captureGroup: 1,
      priority: 30
    },
    
    // PRIORITY 25: Package names
    {
      class: 'class',
      pattern: /\bpackage\s+(?:body\s+)?([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)/gi,
      captureGroup: 1,
      priority: 25
    },
    
    // PRIORITY 20: Assignment operator
    {
      class: 'keyword',
      pattern: /:=/g,
      priority: 20
    },
    
    // PRIORITY 15: Comparison operators
    {
      class: 'keyword',
      pattern: /\/=|<=|>=|=>|\.\.|\*\*/g,
      priority: 15
    }
  ]
});