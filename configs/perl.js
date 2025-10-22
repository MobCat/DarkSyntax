// darkSyntax/configs/perl.js - Perl language configuration
// ==========================================================
// Perl (1987)
// Practical Extraction and Report Language - The duct tape of the Internet
//
// Configs
// =======================
// ALIASES: ['perl', 'pl', 'pm']
// File extensions: .pl, .pm, .t, .pod
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Larry Wall, first released December 18, 1987
// - Originally designed for text processing and system administration
// - Name debate: "Practical Extraction and Report Language" or "Pathologically Eclectic Rubbish Lister"
// - Perl 2 (1988) - added regular expression improvements
// - Perl 3 (1989) - added binary data handling
// - Perl 4 (1991) - "Programming Perl" (the Camel Book) published
// - Perl 5 (1994) - complete rewrite, added objects, modules, references
// - CPAN launched (1995) - Comprehensive Perl Archive Network, revolutionary package system
// - Perl 5.6 (2000) - Unicode support, threading
// - Peak popularity: late 1990s to mid-2000s (the "CGI golden age")
// - Powered early web: CGI scripts, form processing, dynamic websites
// - Bioinformatics standard (1990s-2010s) - genome analysis, BLAST parsing
// - System administration lingua franca (1990s-2000s)
// - Perl 6 announcement (2000) - "We're rewriting everything!"
// - Perl 5.10 (2007) - Smart matching, say(), given/when
// - Perl 6 renamed to Raku (2019) - separate language, not Perl successor
// - Perl 7 announcement (2020) - modernized Perl 5 with saner defaults
// - Still widely used despite "Perl is dead" claims since 2010
// - Powers: booking.com, DuckDuckGo, Amazon infrastructure, bioinformatics
//
// INFLUENCED
// ----------
// - PHP (1995) - Perl's syntax heavily influenced early PHP ($ sigils, regex)
// - Ruby (1995) - Matz explicitly designed Ruby as "more object-oriented Perl"
// - Python (1991) - Guido cited Perl as influence, then positioned Python as opposite
// - JavaScript (1995) - Regular expressions directly borrowed from Perl
// - AWK evolution - Perl was "AWK on steroids"
// - Grep tools - pcre (Perl Compatible Regular Expressions) became standard
// - Bash scripting - Many Bash features inspired by Perl
// - Java regex (2000) - java.util.regex based on Perl 5 patterns
// - .NET Regex (2002) - Microsoft's implementation Perl-compatible
// - Go regex (2009) - RE2 engine, subset of Perl syntax
// - Rust regex (2014) - regex crate supports Perl-style patterns
//
// USED FOR
// --------
// - System administration and automation scripts
// - Text processing and parsing (logs, data files, reports)
// - CGI web applications (1990s-2000s dominance)
// - Bioinformatics and genomics analysis
// - Network programming and socket handling
// - File manipulation and batch processing
// - Regular expression testing and text transformation
// - Build systems (Perl-based make alternatives)
// - Testing frameworks (prove, Test::More)
// - Database interfacing (DBI - Database Independent Interface)
// - Web scraping and data extraction
// - Legacy system glue code
// - Quick one-liners for shell scripting
//
// KEY FEATURES
// ------------
// - "There's more than one way to do it" (TMTOWTDI) philosophy
// - Powerful regular expressions built into the language
// - Sigils for variable types ($scalar, @array, %hash)
// - Context sensitivity (scalar vs list context)
// - Extensive CPAN module ecosystem (over 200,000 modules)
// - Built-in text processing operations
// - Flexible syntax (optional parentheses, multiple statement terminators)
// - Dynamic typing with type conversion
// - References and complex data structures
// - Object-oriented programming (blessing references)
// - Closures and anonymous subroutines
// - Tied variables and operator overloading
// - Format statements for report generation
// - Built-in database interface (DBI)
// - Extensive file and directory operations
// - Socket programming built-in
// - Thread support (though controversial)
//
// CORE SYNTAX
// -----------
// Variables with sigils:
//   $scalar = "value";        # Scalar (string, number, reference)
//   @array = (1, 2, 3);       # Array
//   %hash = (key => "val");   # Hash (associative array)
//
// Subroutines:
//   sub greet {
//       my ($name) = @_;
//       return "Hello, $name!";
//   }
//
// Control structures:
//   if ($condition) { }
//   elsif ($other) { }
//   else { }
//   
//   foreach my $item (@array) { }
//   while ($condition) { }
//   until ($condition) { }
//
// Regular expressions:
//   $string =~ /pattern/;      # Match
//   $string =~ s/old/new/g;    # Substitute
//   $string =~ tr/a-z/A-Z/;    # Transliterate
//
// File operations:
//   open(my $fh, '<', 'file.txt') or die $!;
//   while (<$fh>) { print; }
//   close($fh);
//
// QUIRKS
// ------
// - **Sigils everywhere**: $scalar, @array, %hash, &subroutine, *typeglob
//   * $array[0] to access array element (not @array[0])
//   * $hash{key} to access hash value (not %hash{key})
//   * Context determines behavior: @array in scalar context returns count
// - **Context sensitivity**: Everything has scalar and list context
//   * my @array = localtime(); # Returns 9 elements
//   * my $time = localtime();  # Returns formatted string
//   * Same function, different result based on how you use it!
// - **Magic variables**: $_, $!, $@, $?, $$, $0, $1-$9, @ARGV, %ENV
//   * $_ is implicit variable everywhere
//   * while (<>) { print; } # Prints $_ automatically
//   * Concise but cryptic for newcomers
// - **TMTOWTDI taken to extreme**: Postfix conditionals, statement modifiers
//   * print "yes" if $x > 5;  # Postfix if
//   * do_thing() unless $flag; # Postfix unless
//   * foreach, for, map, grep all do similar things
// - **Special literals**: __FILE__, __LINE__, __PACKAGE__, __DATA__, __END__
//   * Can read data after __DATA__ as file handle
//   * Meta-programming built into language
// - **Barewords**: Unquoted strings sometimes allowed
//   * %hash = (key => value); # 'key' and 'value' are barewords
//   * use strict; disables most barewords (good practice)
// - **Reference syntax**: \$scalar, \@array, \%hash, $arrayref->[0], $hashref->{key}
//   * -> operator for dereferencing
//   * Autovivification: $hash{a}{b}{c} = 1; creates nested structure
// - **Regular expression delimiters**: m//, s///, tr///, qr//, qw//
//   * Can use any delimiter: s{old}{new}, m!pattern!, tr[a-z][A-Z]
//   * Confusing but useful when pattern contains slashes
// - **Special variables for regex**: $1, $2, $3... $&, $`, $'
//   * Capture groups automatically populate $1, $2, etc.
//   * $& is entire match (but slow to use!)
// - **Optional semicolons**: Before closing brace
//   * if ($x) { print "yes" } # Legal, no semicolon needed
//   * But required most other places
// - **Symbolic references**: $$name accesses variable named in $name
//   * $var = "foo"; $$var = 5; # Sets $foo to 5
//   * Powerful but dangerous, disabled by use strict 'refs';
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "There's more than one way to do it" (TMTOWTDI) - Perl motto
// - "Perl: The only language that looks the same before and after RSA encryption" - Keith Bostic
// - "Perl is the duct tape of the Internet" - Larry Wall
// - "I want a language that doesn't care what you think" - Larry Wall
// - "We will encourage you to develop the three great virtues of a programmer: laziness, impatience, and hubris" - Larry Wall
// - "Easy things should be easy, and hard things should be possible" - Larry Wall
// - "Perl is executable line noise" - Perl critics (embraced by community)
// - "Real Perl programmers don't use comments" - Community joke about cryptic code
// - "Write-only language" - Critics' description (code hard to read later)
// - "Python is for people who want to be programmers. Perl is for people who want to get things done." - Community saying
//
// NOTES ON PERL SYNTAX
// --------------------
// - Comments start with # (no multi-line comments)
// - Statements end with semicolon (optional before closing brace)
// - Variables start with sigil: $ @ % & *
// - String interpolation in double quotes: "Hello $name"
// - Single quotes are literal: 'Hello $name' prints literally
// - Regex operators: =~, !~, m//, s///, tr///
// - Anonymous references: [], {}, sub {}
// - Arrow operator: -> for dereferencing and method calls
// - Fat comma: => (same as comma, but quotes left side)
// - Range operator: .. (1..10 generates list)
// - Smart match: ~~ (Perl 5.10+)
// - Postfix conditionals: statement if condition;
// - Special file handles: STDIN, STDOUT, STDERR, DATA
// - Package separator: :: (Package::Module::function)


// PERL SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('perl', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings (before regex to avoid conflicts)
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
    
    // PRIORITY 85: q// quote operators
    {
      class: 'string',
      pattern: /\bq[qwrx]?\s*([^\w\s])(?:[^\1\\]|\\.)*\1/g,
      priority: 85
    },
    
    // PRIORITY 80: Heredoc (<<END)
    {
      class: 'string',
      pattern: /<<['"]?(\w+)['"]?;[\s\S]*?\n\1/g,
      priority: 80
    },
    
    // PRIORITY 75: Regular expressions
    {
      class: 'string',
      pattern: /\/((?:[^\/\\]|\\.)+)\/[gimsx]*/g,
      priority: 75
    },
    
    {
      class: 'string',
      pattern: /\b[sm]\s*([^\w\s])(?:[^\1\\]|\\.)*\1(?:[^\1\\]|\\.)*\1[gimsx]*/g,
      priority: 75
    },
    
    // PRIORITY 70: POD documentation
    {
      class: 'comment',
      pattern: /^=\w+[\s\S]*?^=cut/gm,
      priority: 70
    },
    
    // PRIORITY 65: Special literals
    {
      class: 'decorator',
      pattern: /\b(__FILE__|__LINE__|__PACKAGE__|__DATA__|__END__)\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Keywords
    {
      class: 'keyword',
      pattern: /\b(if|elsif|else|unless|while|until|for|foreach|do|given|when|default|continue|last|next|redo|goto|return|sub|package|use|require|our|my|local|state|no)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(print|printf|say|open|close|read|write|chomp|chop|split|join|grep|map|sort|reverse|push|pop|shift|unshift|splice|keys|values|each|exists|delete|defined|undef|ref|bless|tie|untie|eval|die|warn|exit)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Operators and regex operators
    {
      class: 'operator',
      pattern: /\b(and|or|not|xor|eq|ne|lt|gt|le|ge|cmp)\b/g,
      priority: 50
    },
    
    {
      class: 'operator',
      pattern: /=~|!~|=>|->|\.\./g,
      priority: 50
    },
    
    // PRIORITY 45: Subroutine definitions
    {
      class: 'function',
      pattern: /\bsub\s+([a-zA-Z_]\w*)/g,
      captureGroup: 1,
      priority: 45
    },
    
    // PRIORITY 40: Subroutine calls (with &)
    {
      class: 'function',
      pattern: /&([a-zA-Z_]\w*)/g,
      captureGroup: 1,
      priority: 40
    },
    
    // PRIORITY 35: Method calls
    {
      class: 'function',
      pattern: /->([a-zA-Z_]\w*)/g,
      captureGroup: 1,
      priority: 35
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_]\w*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[eE]?[+-]?\d*\b/g,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+\b/g,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b0[bB][01]+\b/g,
      priority: 25
    },
    
    {
      class: 'number',
      pattern: /\b0[0-7]+\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Special variables (Perl magic)
    {
      class: 'variable',
      pattern: /\$[_0-9]|\$[!@&`'+.\/\\,;#%=\-~^:|?*$<>[\]()]|\$\^[A-Z]/g,
      priority: 20
    },
    
    // PRIORITY 15: Variables with sigils
    {
      class: 'variable',
      pattern: /[\$@%][\{\w][a-zA-Z0-9_]*\b/g,
      priority: 15
    },
    
    // PRIORITY 10: Package names
    {
      class: 'class',
      pattern: /\b([A-Z][a-zA-Z0-9_]*(?:::[A-Z][a-zA-Z0-9_]*)*)\b/g,
      priority: 10
    }
  ]
});