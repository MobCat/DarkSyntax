// darkSyntax/configs/php.js - PHP language configuration
// ========================================================
// PHP (1995)
// PHP: Personal Home Page
// PHP: Hypertext Preprocessor (recursive acronym)
//
// Configs
// =======================
// ALIASES: ['php', 'php3', 'php4', 'php5']
// File extensions: .php, .phtml, .php3, .php4, .php5
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Rasmus Lerdorf (1994)
// - Originally "Personal Home Page Tools" (PHP/FI)
// - First release: PHP/FI 1.0 (June 8, 1995)
// - Rewritten by Zeev Suraski and Andi Gutmans as PHP 3 (1998)
// - Renamed to "PHP: Hypertext Preprocessor" (recursive acronym)
// - PHP 4 with Zend Engine 1.0 (May 22, 2000)
// - PHP 5 with Zend Engine 2.0, OOP improvements (July 13, 2004)
// - PHP 5.3 added namespaces and closures (2009)
// - PHP 7 major performance boost, dropped PHP 6 (December 3, 2015)
// - PHP 7.4 added typed properties and arrow functions (2019)
// - PHP 8.0 JIT compiler, union types, attributes (November 26, 2020)
// - PHP 8.1 enums, readonly properties, fibers (November 25, 2021)
// - PHP 8.2 readonly classes, disjunctive normal form types (December 8, 2022)
// - PHP 8.3 typed class constants (November 23, 2023)
// - Powers approximately 77% of websites (W3Techs, 2024)
// - Facebook wrote HipHop VM (HHVM) for PHP performance (2011)
// - Hack language forked from PHP by Facebook (2014)
// - WordPress, Drupal, Joomla built on PHP
// - Laravel framework revolutionized PHP development (2011)
// - Symfony framework for enterprise PHP (2005)
// - Composer dependency manager (2012)
// - PHP-FIG and PSR standards for interoperability (2009)
// - Despite criticism, remains dominant server-side language
// - "PHP is dead" declared yearly since 2005, still thriving
//
// INFLUENCED
// ----------
// - Perl (1987) - Syntax inspiration, CGI scripting model
// - C (1972) - Core syntax and function naming
// - Java (1995) - OOP features in PHP 5+
// - C++ (1985) - Class and object model
// - Tcl (1988) - Variable syntax with $ sigil
// - JavaScript (1995) - Curly brace syntax
// - Python (1991) - Some function names and conventions
//
// USED FOR
// --------
// - Dynamic websites and web applications
// - Content Management Systems (WordPress, Drupal, Joomla)
// - E-commerce platforms (Magento, WooCommerce, PrestaShop)
// - Web frameworks (Laravel, Symfony, CodeIgniter, Yii)
// - RESTful APIs and web services
// - Server-side scripting and form processing
// - Database-driven applications
// - Session management and authentication
// - File upload and manipulation
// - Email systems and newsletters
// - Social media platforms (Facebook originally PHP)
// - Forums and community platforms (phpBB, vBulletin)
// - Command-line scripts and utilities
// - Web crawlers and scrapers
// - Admin panels and dashboards
// - Blogging platforms (WordPress powers 43% of all websites)
//
// KEY FEATURES
// ------------
// - Server-side scripting language
// - Embedded in HTML with <?php ?> tags
// - Dynamically typed (gradual typing in PHP 7+)
// - Interpreted language (no compilation needed)
// - Extensive built-in function library (1000+ functions)
// - Database integration (MySQL, PostgreSQL, SQLite, etc.)
// - Object-oriented programming (PHP 5+)
// - Namespaces for code organization (PHP 5.3+)
// - Exception handling with try/catch
// - Anonymous functions and closures (PHP 5.3+)
// - Traits for horizontal code reuse (PHP 5.4+)
// - Generators for memory-efficient iteration (PHP 5.5+)
// - Built-in web server for development (PHP 5.4+)
// - Composer for dependency management
// - JIT compilation (PHP 8+)
// - Union types, named arguments (PHP 8+)
// - Attributes/Annotations (PHP 8+)
// - Enumerations (PHP 8.1+)
// - First-class callable syntax (PHP 8.1+)
//
// CORE SYNTAX
// -----------
// Basic PHP script:
//   <?php
//   echo "Hello, World!";
//   ?>
//
// Variables:
//   $name = "Alice";
//   $age = 30;
//   $active = true;
//
// Functions:
//   function greet($name) {
//     return "Hello, " . $name;
//   }
//
// Classes (PHP 5+):
//   class Person {
//     private $name;
//     
//     public function __construct($name) {
//       $this->name = $name;
//     }
//     
//     public function getName() {
//       return $this->name;
//     }
//   }
//
// Arrays:
//   $arr = array(1, 2, 3);
//   $arr = [1, 2, 3];  // Short syntax PHP 5.4+
//   $assoc = ['key' => 'value'];
//
// QUIRKS
// ------
// - **Inconsistent function naming**: Historical baggage
//   * str_replace vs. strpos (underscore inconsistency)
//   * htmlspecialchars vs. strip_tags
//   * No consistent naming convention
//   * Haystack/needle parameter order varies
//
// - **Variables need $ sigil**: Always required
//   * $variable not variable
//   * Must be prefixed in all contexts
//   * $$variable for variable variables
//
// - **Array + operator**: Different from array_merge
//   * $a + $b keeps keys from $a, skips duplicates
//   * array_merge() renumbers numeric keys
//   * Confusing behavior for beginners
//
// - **== vs ===**: Type coercion strikes again
//   * "0" == 0 is true (type coercion)
//   * "0" === 0 is false (strict comparison)
//   * Always use === for safety
//   * "0e123" == "0e456" is true (scientific notation!)
//
// - **Truth-y and false-y values**: Many surprises
//   * "0" is falsy (string zero)
//   * 0, 0.0, "", "0", [], null, false all falsy
//   * Everything else truthy (including "00", "false")
//
// - **Global keyword required**: For function scope
//   * Function cannot access global $var directly
//   * Must declare: global $var; inside function
//   * Or use $GLOBALS['var']
//
// - **References with &**: Pass by reference
//   * function modify(&$value) { }
//   * Can modify original variable
//   * Easy to introduce bugs
//
// - **Magic quotes**: Historical nightmare
//   * Auto-escaped quotes in PHP < 5.4
//   * Removed in PHP 5.4 (2012)
//   * Broke so much legacy code
//
// - **Register globals**: Security disaster
//   * URL params became variables automatically
//   * ?admin=1 created $admin variable
//   * Disabled by default PHP 4.2 (2002)
//   * Removed in PHP 5.4 (2012)
//
// - **PHP tags**: Multiple opening styles
//   * <?php standard (always works)
//   * <? short tags (discouraged)
//   * <?= echo shorthand
//   * ASP-style <% %> (removed)
//
// - **String concatenation**: Dot operator
//   * "Hello" . " World" (not +)
//   * + is addition only
//   * Different from JavaScript, Python
//
// - **Array functions**: Inconsistent parameters
//   * array_filter($array, $callback)
//   * array_map($callback, $array)
//   * Callback first vs array first
//   * Endless confusion
//
// - **NULL coalescing**: Multiple operators
//   * isset() checks existence
//   * empty() checks falsy
//   * ?? null coalescing (PHP 7+)
//   * ??= null coalescing assignment (PHP 7.4+)
//
// - **Type juggling**: Automatic conversions
//   * "10" + 5 = 15 (string becomes int)
//   * "10 apples" + 5 = 15 (ignores text!)
//   * "abc" + 5 = 5 (non-numeric string = 0)
//   * Declare strict_types=1 to prevent
//
// - **Error handling**: Multiple paradigms
//   * Errors (E_ERROR, E_WARNING, E_NOTICE)
//   * Exceptions (throw/catch)
//   * @ error suppression operator (evil)
//   * set_error_handler() for custom handling
//
// - **Include vs require**: Four variations
//   * include, require, include_once, require_once
//   * include warns on failure, require fatal errors
//   * _once prevents multiple inclusions
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "PHP is a minor evil perpetrated and created by incompetent amateurs, whereas Perl is a great and insidious evil perpetrated by skilled but perverted professionals" - Jon Ribbens
// - "I'm not a real programmer. I throw together things until it works then I move on" - Rasmus Lerdorf (PHP creator)
// - "PHP is like a hand grenade - easy to use, effective, but easy to blow yourself up with" - Programming wisdom
// - "I don't know how to stop it, there was never any intent to write a programming language" - Rasmus Lerdorf
// - "PHP is the duct tape of the Internet" - Developer saying
// - "There are two ways to write error-free programs; only the third one works" - Alan Perlis (applies to PHP)
// - "PHP is not dead, it just smells funny" - Annual declaration since 2005
// - "WordPress powers 43% of the web, so PHP powers 43% of the web" - Modern PHP defense
// - "Modern PHP is actually good now" - Developers since PHP 7
// - "I hate PHP, but I love the money" - Pragmatic developer sentiment
//
// NOTES ON PHP SYNTAX
// -------------------
// - Case-insensitive keywords and function names
// - Case-sensitive variable names
// - Comments: //, #, /* */
// - PHP code wrapped in <?php ?> tags
// - Short echo tag: <?= expression ?>
// - Variables always start with $: $variable
// - String concatenation with . (dot)
// - String interpolation in double quotes: "Hello $name"
// - Arrays: array() or [] (short syntax PHP 5.4+)
// - Associative arrays: ['key' => 'value']
// - Classes and objects: class Name { }
// - Visibility: public, private, protected
// - Constructors: __construct()
// - Static members: static $var, self::$var
// - Namespaces: namespace App\Http\Controllers;
// - Use statements: use Some\Namespace\Class;
// - Type hints: function(string $name): int
// - Nullable types: ?string (PHP 7.1+)
// - Union types: string|int (PHP 8+)
// - Named arguments: func(name: "value") (PHP 8+)
// - Attributes: #[Route("/path")] (PHP 8+)
// - Arrow functions: fn($x) => $x * 2 (PHP 7.4+)
// - Match expressions: match($x) { } (PHP 8+)
// - Enums: enum Status { } (PHP 8.1+)
// - Readonly properties: readonly string $name (PHP 8.1+)

// PHP SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================
darkSyntax.registerLanguage('php', {
  rules: [
    // PRIORITY 100: Comments must come first
    // Single-line comments (// style)
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    
    // Single-line comments (# style)
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // Multi-line comments
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    // Double-quoted strings (supports variable interpolation)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // Single-quoted strings (non-greedy, no newlines)
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },
    
    // Heredoc/Nowdoc syntax
    {
      class: 'string',
      pattern: /<<<['"]?(\w+)['"]?[\s\S]*?\1;/g,
      priority: 90
    },
    
    // PRIORITY 60: PHP tags
    {
      class: 'decorator',
      pattern: /<\?php|<\?=|\?>/g,
      priority: 60
    },
    
    // PRIORITY 55: Attributes (PHP 8+)
    {
      class: 'decorator',
      pattern: /#\[\w+(?:\([^)]*\))?\]/g,
      priority: 55
    },
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(abstract|and|array|as|break|callable|case|catch|class|clone|const|continue|declare|default|die|do|echo|else|elseif|empty|enddeclare|endfor|endforeach|endif|endswitch|endwhile|enum|eval|exit|extends|final|finally|fn|for|foreach|function|global|goto|if|implements|include|include_once|instanceof|insteadof|interface|isset|list|match|namespace|new|or|print|private|protected|public|readonly|require|require_once|return|static|switch|throw|trait|try|unset|use|var|while|xor|yield|from)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Magic constants
    {
      class: 'builtin',
      pattern: /\b(__CLASS__|__DIR__|__FILE__|__FUNCTION__|__LINE__|__METHOD__|__NAMESPACE__|__TRAIT__)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Built-in types
    {
      class: 'builtin',
      pattern: /\b(string|int|float|bool|array|object|mixed|void|null|false|true|self|parent|static|iterable|never|resource)\b/g,
      priority: 40
    },
    
    // Common built-in functions
    {
      class: 'builtin',
      pattern: /\b(array_map|array_filter|array_reduce|array_merge|array_push|array_pop|count|sizeof|strlen|substr|str_replace|str_contains|preg_match|preg_replace|explode|implode|json_encode|json_decode|file_get_contents|file_put_contents|isset|empty|unset|die|exit|var_dump|print_r|header|session_start|password_hash|password_verify|htmlspecialchars|strip_tags|trim|ltrim|rtrim)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Variables (must come before function calls)
    {
      class: 'variable',
      pattern: /\$[a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*/g,
      priority: 35
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Class names (PascalCase)
    {
      class: 'class',
      pattern: /\b([A-Z][a-zA-Z0-9_]*)\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 20
    },
    
    // Hexadecimal numbers
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+\b/g,
      priority: 20
    },
    
    // Binary numbers
    {
      class: 'number',
      pattern: /\b0[bB][01]+\b/g,
      priority: 20
    },
    
    // Octal numbers
    {
      class: 'number',
      pattern: /\b0[oO][0-7]+\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Booleans and null
    {
      class: 'boolean',
      pattern: /\b(true|false|null|TRUE|FALSE|NULL)\b/g,
      priority: 15
    }
  ]
});