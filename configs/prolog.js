// darkSyntax/configs/prolog.js - Prolog language configuration
// ==============================================================
// Prolog (1972)
//
// Configs
// =======================
// ALIASES: ['prolog', 'pl', 'pro']
// File extensions: .pl, .pro, .P
//
// PROLOG SYNTAX NOTES
// ===================
//
// Prolog (PROgrammation en LOGique) is a logic programming language created
// in 1972 by Alain Colmerauer and Philippe Roussel. Unlike imperative languages,
// Prolog programs consist of facts and rules, and execution is done through
// logical inference and pattern matching.
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Alain Colmerauer and Philippe Roussel (1972)
// - Developed at University of Aix-Marseille, France
// - Based on Robert Kowalski's procedural interpretation of Horn clauses
// - First declarative programming language to gain widespread use
// - Standard: ISO/IEC 13211 (1995)
// - Major implementations: SWI-Prolog, GNU Prolog, YAP, SICStus
//
// INFLUENCED
// ----------
// - Mercury (1995) - Purely declarative logic/functional
// - Erlang (1986) - Pattern matching and declarative style
// - Datalog - Subset used in databases
// - Answer Set Programming (ASP)
// - Constraint Logic Programming (CLP)
//
// USED FOR
// --------
// - Artificial Intelligence and Expert Systems
// - Natural Language Processing
// - Theorem proving and symbolic computation
// - Database query languages
// - IBM Watson (parts written in Prolog)
// - Scheduling and planning systems
//
// PARADIGM SHIFT
// --------------
// Prolog is DECLARATIVE, not imperative:
// - You describe WHAT you want, not HOW to compute it
// - Program = Facts + Rules
// - Execution = Query + Logical Inference
// - Built-in backtracking and unification
// - Variables are logical variables, not storage locations
//
// PROGRAM STRUCTURE
// -----------------
// Facts: parent(tom, bob).
// Rules: grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
// Queries: ?- grandparent(tom, Who).
//
// SYNTAX ELEMENTS
// ---------------
// - Facts end with period: fact(argument).
// - Rules use :- (if): head :- body.
// - Queries start with ?-: ?- query(X).
// - Variables start with uppercase: X, Y, Result
// - Atoms start with lowercase: atom, parent, likes
// - Anonymous variable: _ (matches anything, don't care)
// - Lists: [1, 2, 3] or [Head|Tail]
// - Comments: % single-line or /* multi-line */
//
// OPERATORS
// ---------
// :- (if/rule)
// , (and/conjunction)
// ; (or/disjunction)
// \+ (not/negation by failure)
// = (unification)
// == (equality test)
// \= (not unifiable)
// \== (not equal)
// is (arithmetic evaluation)
// =:= (arithmetic equality)
// =\= (arithmetic inequality)
// @< @> @=< @>= (term comparison)
//
// BUILT-IN PREDICATES
// -------------------
// true, fail - Always succeed/fail
// assert, retract - Dynamic database modification
// findall, bagof, setof - Collect solutions
// member, append, length - List operations
// write, read, nl - I/O
// atom, number, var, nonvar - Type checking
// functor, arg, =.. - Term manipulation
//
// LISTS
// -----
// [1, 2, 3] - List notation
// [H|T] - Head and Tail decomposition
// [] - Empty list
// [X] - Single element
// [X, Y | Rest] - Multiple heads

darkSyntax.registerLanguage('prolog', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /%.*$/gm,
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 90: Strings
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
    
    // PRIORITY 80: Operators (must come before atoms)
    {
      class: 'keyword',
      pattern: /:-|-->|\\=|\\==|=:=|=\\=|@<|@>|@=<|@>=|==|\\+|=\.\./g,
      priority: 80
    },
    
    // PRIORITY 70: Built-in predicates
    {
      class: 'builtin',
      pattern: /\b(true|fail|assert|asserta|assertz|retract|retractall|findall|bagof|setof|member|append|length|reverse|sort|msort|keysort|atom|number|integer|float|var|nonvar|compound|atomic|functor|arg|copy_term|write|writeln|read|get|put|nl|tab|format|open|close|see|seen|tell|told|is|mod|abs|sign|sqrt|sin|cos|tan|exp|log|ceiling|floor|round|truncate|min|max|random|between|succ|plus|atom_codes|atom_chars|atom_length|atom_concat|sub_atom|char_code|upcase_atom|downcase_atom)\b/g,
      priority: 70
    },
    
    // PRIORITY 60: Control predicates and keywords
    {
      class: 'keyword',
      pattern: /\b(if|then|else|elif|endif|not|cut|repeat|halt|abort|catch|throw|true|fail|call|once)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Dynamic declaration
    {
      class: 'decorator',
      pattern: /:-\s*dynamic\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Query operator
    {
      class: 'decorator',
      pattern: /\?-/g,
      priority: 50
    },
    
    // PRIORITY 45: Anonymous variable
    {
      class: 'keyword',
      pattern: /\b_\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Variables (start with uppercase or underscore)
    {
      class: 'function',
      pattern: /\b[A-Z_][a-zA-Z0-9_]*/g,
      priority: 40
    },
    
    // PRIORITY 30: Predicate definitions (atom before parenthesis or :-)
    {
      class: 'function',
      pattern: /\b([a-z][a-zA-Z0-9_]*)\s*(?=\(|:-)/g,
      priority: 30
    },
    
    // PRIORITY 25: List brackets
    {
      class: 'keyword',
      pattern: /[\[\]]/g,
      priority: 25
    },
    
    // PRIORITY 20: Numbers (including floats and scientific notation)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?/g,
      priority: 20
    },
    
    // PRIORITY 15: Boolean-like atoms
    {
      class: 'boolean',
      pattern: /\b(true|false|yes|no)\b/g,
      priority: 15
    }
  ]
});