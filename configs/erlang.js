// darkSyntax/configs/erlang.js - Erlang configuration
// ====================================================
// Erlang (1986)
// Concurrent, functional programming language built for fault-tolerant systems
//
// Configs
// =======================
// ALIASES: ['erlang', 'erl', 'hrl']
// File extensions: .erl, .hrl
//
// .erl - Erlang source files (modules)
// .hrl - Erlang header files (records, macros)
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created at Ericsson (1986) by Joe Armstrong, Robert Virding, Mike Williams
// - Open-sourced (1998) - Led to widespread adoption
// - OTP framework established (1996) - Standard library and design patterns
// - WhatsApp acquisition by Facebook (2014) - Proved scalability (50 engineers, 900M users)
// - Influenced modern concurrency models and actor-based systems
//
// INFLUENCED
// ----------
// - Elixir (2011) - Runs on BEAM VM, modernized syntax
// - Scala (2004) - Actor model via Akka library
// - Go (2009) - Goroutines inspired by Erlang processes
// - Rust (2010) - Error handling philosophy (let it crash)
// - Clojure (2007) - Immutability and functional approach
//
// USED FOR
// --------
// - Telecommunications systems (original purpose)
// - Messaging platforms (WhatsApp, Discord voice/video)
// - Distributed databases (CouchDB, Riak)
// - Financial trading systems
// - Real-time multiplayer games
// - IoT and embedded systems
//
// KEY FEATURES
// ------------
// - Lightweight processes with actor model
// - "Let it crash" philosophy with supervisor trees
// - Hot code swapping without stopping systems
// - Immutable data and pattern matching
// - Built-in distributed computing
// - Soft real-time garbage collection per process
//
// CORE SYNTAX
// -----------
// - Variables must start with uppercase: Name, Value
// - Atoms are lowercase constants: ok, error, true
// - Pattern matching in function heads and case expressions
// - Lists: [1, 2, 3], [H|T] for head/tail destructuring
// - Tuples: {ok, Result}, {error, Reason}
// - Anonymous functions: fun(X) -> X * 2 end
// - Message passing: Pid ! Message, receive ... end
//
// QUIRKS
// ------
// - **Variables vs Atoms - The uppercase/lowercase distinction**:
//   * Variables MUST start with uppercase or underscore: X, Name, _Count, _
//   * Atoms MUST start with lowercase: ok, error, my_atom
//   * This is enforced by the language, not just convention
//   * Variables are placeholders that get bound to values (once per scope)
//   * Atoms are literal constants, like symbols or enums in other languages
//   * Example: In {ok, Result}, 'ok' is an atom (constant), 'Result' is a variable
//   * Atoms are used for: function names, module names, return tags, pattern matching
//   * Atoms can be quoted to include spaces/caps: 'My Atom', 'HTTP_ERROR'
// - **Single assignment**: Variables can only be bound once in a scope
//   * X = 5, then X = 6 causes a match error
//   * Not traditional assignment but pattern matching
//   * The = operator means "match", not "assign"
//   * You can rebind in a new scope (like inside a different function clause)
// - **Punctuation matters**: Commas (,) separate clauses, semicolons (;) separate alternatives, 
//   periods (.) end function definitions
// - **Atoms are NOT garbage collected**: Creating dynamic atoms can cause memory leaks
//   * Atoms are stored in a global atom table that never shrinks
//   * Maximum ~1 million atoms per VM (can crash if exceeded)
//   * Never use list_to_atom/1 on user input!
// - **String confusion**: "hello" is a list of integers [104,101,108,108,111]
//   * <<"hello">> is a binary (actual string data)
//   * [72,101,108,108,111] = "Hello" evaluates to true
//   * Modern Erlang code uses binaries for strings
// - **Function references**: fun module:function/arity notation
//   * fun lists:map/2 refers to map function with 2 arguments
//   * Arity (number of parameters) is part of function identity
// - **No return statement**: Functions return the last expression evaluated
// - **Guards are limited**: Only certain BIFs allowed in when clauses
//   * Can't call custom functions in guards, only specific built-ins
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Make it work, then make it beautiful, then if you really, really have to, make it fast." 
//   - Joe Armstrong
// - "Let it crash" - Erlang philosophy on error handling
// - "Nine nines" - 99.9999999% uptime (31ms downtime per year) achieved by Ericsson AXD301
//
// NOTES ON ERLANG SYNTAX
// -------------------------
// - Comments use % character
// - Atoms: lowercase identifiers or 'quoted atoms'
// - Variables: uppercase identifiers
// - Strings are lists of integers (use binaries for actual strings)
// - Functions defined with name(Args) -> Body.
// - Pattern matching with = operator
// - Records use # notation: #record{field=value}
// - Macros use ? notation: ?MACRO_NAME
// - Module attributes: -module(name). -export([func/arity]).


// ERLANG SYNTAX CONFIGURATION FOR DARKSYNTAX
// ===========================================
darkSyntax.registerLanguage('erlang', {
  rules: [
    // Comments (% style)
    {
      class: 'comment',
      pattern: /%.*$/gm,
      priority: 100
    },

    // Strings (double quotes - actually lists of integers)
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"/g,
      priority: 90
    },

    // Atoms (quoted with single quotes)
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Module attributes and preprocessor directives
    {
      class: 'decorator',
      pattern: /-\s*(module|export|import|compile|vsn|author|include|include_lib|define|undef|ifdef|ifndef|else|endif|record|type|spec|callback|behaviour|behavior)\b/g,
      priority: 75
    },

    // Macros (? prefix)
    {
      class: 'decorator',
      pattern: /\?[A-Z_][A-Z0-9_]*/g,
      priority: 74
    },

    // Record syntax (#record{})
    {
      class: 'class',
      pattern: /#[a-z][a-zA-Z0-9_]*/g,
      priority: 65
    },

    // Keywords
    {
      class: 'keyword',
      pattern: /\b(after|begin|case|catch|cond|end|fun|if|let|of|query|receive|try|when|andalso|orelse|bnot|not|div|rem|band|bor|bxor|bsl|bsr)\b/g,
      priority: 50
    },

    // Built-in functions (common BIFs)
    {
      class: 'builtin',
      pattern: /\b(spawn|spawn_link|spawn_monitor|spawn_opt|link|unlink|monitor|demonitor|exit|throw|error|erlang|self|is_atom|is_binary|is_boolean|is_float|is_function|is_integer|is_list|is_number|is_pid|is_port|is_reference|is_tuple|abs|hd|tl|length|size|tuple_size|byte_size|bit_size|element|setelement|list_to_tuple|tuple_to_list|float|round|trunc|apply|make_ref|node|nodes|now|time|date|halt|get|put|erase|register|unregister|whereis|send|process_flag|group_leader)\b/g,
      priority: 45
    },

    // Function calls (module:function or just function)
    {
      class: 'function',
      pattern: /\b([a-z][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },

    // Module reference (module:)
    {
      class: 'class',
      pattern: /\b([a-z][a-zA-Z0-9_]*)\s*:/g,
      captureGroup: 1,
      priority: 40
    },

    // Function definition (name/arity)
    {
      class: 'function',
      pattern: /\b([a-z][a-zA-Z0-9_]*)\/\d+/g,
      captureGroup: 1,
      priority: 38
    },

    // Boolean and special atoms
    {
      class: 'boolean',
      pattern: /\b(true|false|undefined|ok|error|infinity)\b/g,
      priority: 25
    },

    // Numbers (integers, floats, hex, binary, octal)
    {
      class: 'number',
      pattern: /\b\d+#[0-9a-zA-Z]+\b/g, // Base notation: 16#FF
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?\b/g, // Floats and scientific
      priority: 21
    },
    {
      class: 'number',
      pattern: /\$./g, // Character literals: $a, $\n
      priority: 20
    },

    // Variables (must start with uppercase or _)
    {
      class: 'variable',
      pattern: /\b[A-Z_][a-zA-Z0-9_]*\b/g,
      priority: 15
    },

    // Operators
    {
      class: 'operator',
      pattern: /(?:->|<-|=>|==|\/=|=:=|=\/=|=<|>=|<|>|\+\+|--|\+|-|\*|\/|!|\|{1,2}|#{1,2}|<<|>>|::|=)/g,
      priority: 58
    }
  ]
});