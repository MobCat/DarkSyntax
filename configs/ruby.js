// darkSyntax/configs/ruby.js - Ruby configuration
// =================================================
// Ruby (1995)
// Dynamic, object-oriented programming language designed for programmer happiness
//
// Configs
// =======================
// ALIASES: ['ruby', 'rb', 'rbw']
// File extensions: .rb, .rbw, .rake, .gemspec
//
// .rb - Standard Ruby source files
// .rbw - Ruby scripts without console (Windows)
// .rake - Rake build files (Ruby Make)
// .gemspec - Gem specification files
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Yukihiro "Matz" Matsumoto (1995) - Designed for programmer happiness
// - Ruby on Rails released (2004) - Revolutionized web development
// - Ruby 1.9 (2007) - Major performance improvements, new hash syntax
// - Ruby 2.0 (2013) - Keyword arguments, refinements
// - Ruby 3.0 (2020) - 3x performance goal achieved, static analysis tools
// - Influenced modern language design with focus on developer experience
//
// INFLUENCED
// ----------
// - Crystal (2014) - Compiled Ruby-like syntax with static typing
// - Elixir (2011) - Borrowed Ruby's readable syntax for Erlang VM
// - Swift (2014) - Optional chaining and guard clauses inspired by Ruby
// - CoffeeScript (2009) - Ruby-inspired syntax for JavaScript
// - Rust (2010) - Expression-oriented design philosophy
//
// USED FOR
// --------
// - Web applications (Ruby on Rails, Sinatra)
// - DevOps automation (Chef, Puppet, Vagrant)
// - Static site generators (Jekyll, Middleman)
// - Testing frameworks (RSpec, Cucumber)
// - Scripting and automation
// - API development and microservices
//
// KEY FEATURES
// ------------
// - "Principle of Least Surprise" - intuitive, readable syntax
// - Everything is an object (even numbers and nil)
// - Blocks and closures with clean syntax
// - Mixins via modules (multiple inheritance alternative)
// - Duck typing - "if it quacks like a duck..."
// - Metaprogramming with method_missing, define_method
// - Convention over configuration philosophy
//
// CORE SYNTAX
// -----------
// - Methods can be called without parentheses: puts "hello"
// - Question mark methods for predicates: empty?, valid?
// - Bang methods for dangerous operations: save!, sort!
// - Symbols are immutable identifiers: :symbol
// - String interpolation: "Hello #{name}"
// - Multiple ways to define strings: '', "", %q{}, %Q{}, heredocs
// - Blocks: { |x| x * 2 } or do |x| x * 2 end
// - Everything returns a value (no void functions)
//
// QUIRKS
// ------
// - **Methods can end with ? or !**:
//   * Question mark (?) means predicate: empty?, valid?, nil?
//   * Bang (!) means dangerous/mutating: save!, sort!, gsub!
//   * These are just naming conventions, not enforced by language
// - **Parentheses are optional**: puts("hi") and puts "hi" are identical
//   * Can lead to ambiguity: foo bar, baz could be foo(bar, baz) or foo(bar), baz
//   * Community style: use parens for method definitions, skip for DSLs
// - **Everything is truthy except false and nil**:
//   * 0 is truthy (unlike C, JavaScript)
//   * Empty strings "" are truthy
//   * Empty arrays [] are truthy
// - **Global variables use $**: $global_var
//   * Considered bad practice, rarely used in modern Ruby
//   * Some built-ins use them: $stdout, $LOAD_PATH
// - **Instance variables use @**: @instance_var
//   * Class variables use @@: @@class_var (also discouraged)
// - **Symbols vs Strings**:
//   * :symbol is immutable, only one copy in memory
//   * "string" creates new object each time
//   * Symbols used for hash keys, method names, constants
// - **Implicit return**: Last expression is automatically returned
//   * return keyword optional (and often discouraged)
// - **Multiple assignment**: a, b = 1, 2 or a, b = [1, 2]
// - **Safe navigation operator**: obj&.method won't crash if obj is nil (Ruby 2.3+)
// - **Monkey patching**: Can reopen any class and add/modify methods
//   * Powerful but dangerous - can break standard library
//   * Led to creation of refinements for safer patching
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Ruby is designed to make programmers happy." - Yukihiro Matsumoto (Matz)
// - "Matz is nice so we are nice" (MINASWAN) - Ruby community motto
// - "There's more than one way to do it" - Perl influence, Ruby philosophy
// - "Convention over configuration" - Rails philosophy that spread to other frameworks
//
// NOTES ON RUBY SYNTAX
// -------------------------
// - Comments use # character
// - Multi-line comments use =begin/=end (rarely used)
// - Case-sensitive language
// - Snake_case for variables and methods
// - CamelCase for classes and modules
// - SCREAMING_SNAKE_CASE for constants
// - Strings support interpolation with #{}
// - Regular expressions are first-class: /pattern/
// - Ranges: 1..10 (inclusive), 1...10 (exclusive)


// RUBY SYNTAX CONFIGURATION FOR DARKSYNTAX
// =========================================
darkSyntax.registerLanguage('ruby', {
  rules: [
    // Multi-line comments (=begin/=end)
    {
      class: 'comment',
      pattern: /^=begin[\s\S]*?^=end/gm,
      priority: 101
    },

    // Single-line comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },

    // Regex literals
    {
      class: 'string',
      pattern: /\/(?:[^\/\\\n]|\\.)+\/[imxo]*/g,
      priority: 92
    },

    // Heredocs
    {
      class: 'string',
      pattern: /<<[-~]?(['"`]?)(\w+)\1[\s\S]*?\n\2/g,
      priority: 91
    },

    // String interpolation (double quotes)
    {
      class: 'string',
      pattern: /"(?:[^"\\#]|\\.|#(?!\{))*(?:#\{[^}]*\}(?:[^"\\#]|\\.|#(?!\{))*)*"/g,
      priority: 90
    },

    // Single-quoted strings (no interpolation)
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Percent string literals
    {
      class: 'string',
      pattern: /%[qQwWiI]?[({[<](?:[^)\]}>\\]|\\.)*[)}\]>]/g,
      priority: 90
    },

    // Symbols
    {
      class: 'variable',
      pattern: /:[a-zA-Z_][a-zA-Z0-9_]*[?!]?/g,
      priority: 75
    },

    // Symbol with quotes
    {
      class: 'variable',
      pattern: /:'(?:[^'\\]|\\.)*'/g,
      priority: 75
    },

    // Instance variables
    {
      class: 'variable',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 74
    },

    // Class variables
    {
      class: 'variable',
      pattern: /@@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 74
    },

    // Global variables
    {
      class: 'variable',
      pattern: /\$[a-zA-Z_][a-zA-Z0-9_]*|\$[0-9]+|\$[!@&`'+~=/\\,;.<>*$?:"]/g,
      priority: 74
    },

    // Class definitions
    {
      class: 'class',
      pattern: /\b(class|module)\s+([A-Z][a-zA-Z0-9_]*(?:::[A-Z][a-zA-Z0-9_]*)*)/g,
      captureGroup: 2,
      priority: 65
    },

    // Keywords
    {
      class: 'keyword',
      pattern: /\b(BEGIN|END|alias|and|begin|break|case|class|def|defined|do|else|elsif|end|ensure|for|if|in|module|next|not|or|redo|rescue|retry|return|then|undef|unless|until|when|while|yield|super|self)\b/g,
      priority: 50
    },

    // Built-in methods and constants
    {
      class: 'builtin',
      pattern: /\b(puts|print|p|gets|require|require_relative|include|extend|attr_reader|attr_writer|attr_accessor|raise|fail|catch|throw|lambda|proc|block_given|loop|Array|Hash|String|Integer|Float|Symbol|Regexp|File|Dir|Time|Date|DateTime|Proc|Method|Class|Module|Object|Kernel|Enumerable|Comparable|Math|GC|ObjectSpace|Marshal|TRUE|FALSE|NIL|STDIN|STDOUT|STDERR|ENV|ARGV|RUBY_VERSION|RUBY_PLATFORM)\b/g,
      priority: 45
    },

    // Method definitions (with ?, !, =)
    {
      class: 'function',
      pattern: /\bdef\s+([a-z_][a-zA-Z0-9_]*[?!=]?)/g,
      captureGroup: 1,
      priority: 40
    },

    // Method calls (including ?, !)
    {
      class: 'function',
      pattern: /\b([a-z_][a-zA-Z0-9_]*[?!]?)\s*(?=\()/g,
      priority: 35
    },

    // Method calls without parens (conservative)
    {
      class: 'function',
      pattern: /\.([a-z_][a-zA-Z0-9_]*[?!]?)/g,
      captureGroup: 1,
      priority: 34
    },

    // Constants (including class names in code)
    {
      class: 'class',
      pattern: /\b[A-Z][A-Z0-9_]*\b/g,
      priority: 30
    },

    // Boolean and nil
    {
      class: 'boolean',
      pattern: /\b(true|false|nil)\b/g,
      priority: 25
    },

    // Numbers (including hex, binary, octal)
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0[bB][01]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0[oO][0-7]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?\b/g,
      priority: 21
    },

    // Operators
    {
      class: 'operator',
      pattern: /(?:<<|>>|<=>|===|==|!=|<=|>=|=~|!~|&&|\|\||\.\.\.?|\*\*|&\.|::|=>|->|\+=|-=|\*=|\/=|%=|\|=|&=|\^=|<<=|>>=|\?\.|[+\-*\/%=<>!&|^~?])/g,
      priority: 58
    }
  ]
});