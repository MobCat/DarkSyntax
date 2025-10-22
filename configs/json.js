// darkSyntax/configs/json.js - JSON language configuration
// ==========================================================
// JSON (2001)
// JavaScript Object Notation - Lightweight data interchange format
//
// Configs
// =======================
// ALIASES: ['json']
// File extensions: .json
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Douglas Crockford (2001)
// - First specified on json.org (2001)
// - RFC 4627 published (July 2006)
// - Revised as RFC 7159 (March 2014)
// - Current standard: RFC 8259 / ECMA-404 (December 2017)
// - Derived from JavaScript object literal notation (ECMAScript 3rd edition, 1999)
// - Designed as lightweight alternative to XML
// - Originally conceived for AJAX web applications
// - "I discovered JSON. I do not claim to have invented it" - Douglas Crockford
// - Popularized by XMLHttpRequest and AJAX (2005)
// - Became de facto standard for web APIs
// - REST APIs adopted JSON over XML (late 2000s)
// - NoSQL databases like MongoDB use JSON-like formats (BSON)
// - Configuration files increasingly use JSON (package.json, tsconfig.json)
// - State Management 0 specification (2013) formalized minimal subset
// - JSON5 extension created for more lenient parsing (2012)
// - JSONC (JSON with Comments) variant by Microsoft
// - JSON Schema for validation (2007)
// - Replaced XML for most web APIs by 2010
// - GitHub API switched from XML to JSON (2012)
// - "JSON has become the lingua franca of web services"
//
// INFLUENCED
// ----------
// - JavaScript (1995) - Parent language, object literal syntax
// - XML (1998) - Designed as simpler alternative
// - YAML (2001) - Superset of JSON in YAML 1.2+
// - TOML (2013) - Configuration file format alternative
// - Protocol Buffers (2008) - Binary alternative from Google
// - MessagePack (2008) - Binary JSON-like format
// - BSON (2009) - Binary JSON used by MongoDB
// - CBOR (2013) - Concise Binary Object Representation
// - JSON5 (2012) - Relaxed JSON syntax extension
// - JSONC (Microsoft) - JSON with comments
// - HOCON (2012) - Human-Optimized Config Object Notation
// - JSON-LD (2014) - Linked Data format by W3C
// - GeoJSON (2008) - Geospatial data format
// - JSON API (2013) - API specification standard
// - JSON Schema (2007) - Validation and documentation
// - JSON Patch (2013) - Format for describing changes (RFC 6902)
// - JSON Pointer (2013) - String syntax for identifying values (RFC 6901)
//
// USED FOR
// --------
// - REST API request and response payloads
// - Configuration files (package.json, tsconfig.json, eslint.json)
// - Data storage and exchange between systems
// - NoSQL database documents (MongoDB, CouchDB)
// - Application state serialization
// - Log file structured data
// - Internationalization language files (i18n)
// - GraphQL query results
// - Browser localStorage and sessionStorage
// - Build tool configurations (Webpack, Rollup, Vite)
// - IDE and editor settings (VS Code, Sublime Text)
// - Cloud service configurations (AWS, Azure, GCP)
// - Package managers (npm, yarn, pnpm)
// - API documentation (OpenAPI/Swagger)
// - Data import/export between applications
// - Message queue payloads (RabbitMQ, Kafka)
//
// KEY FEATURES
// ------------
// - Human-readable text format
// - Language-independent (parsers in every language)
// - Simple, minimal syntax with 6 data types
// - Self-describing and easy to understand
// - Lightweight compared to XML
// - Direct mapping to common data structures
// - No schema required (optional JSON Schema)
// - UTF-8 encoding by default
// - Whitespace insignificant (except in strings)
// - Strict syntax rules (no ambiguity)
// - Easy to parse and generate
// - Supports nested structures
// - Arrays and objects as containers
// - No circular references allowed
// - No comments in standard (by design)
// - No date/time types (strings or numbers)
// - Limited number precision (IEEE 754 double)
//
// CORE SYNTAX
// -----------
// Six data types:
//   1. Object: {"key": "value"}
//   2. Array: [1, 2, 3]
//   3. String: "text"
//   4. Number: 42, 3.14, -10, 1.5e10
//   5. Boolean: true, false
//   6. Null: null
//
// Object example:
//   {
//     "name": "John Doe",
//     "age": 30,
//     "email": "john@example.com"
//   }
//
// Array example:
//   ["apple", "banana", "orange"]
//
// Nested structures:
//   {
//     "user": {
//       "name": "Alice",
//       "roles": ["admin", "user"]
//     }
//   }
//
// QUIRKS
// ------
// - **No comments**: By design for simplicity
//   * Cannot add // or /* */ comments
//   * Workaround: Use "_comment" key with string value
//   * JSONC (Microsoft variant) allows comments
//   * Controversial decision, but keeps parsing simple
//
// - **No trailing commas**: Strict syntax
//   * `[1, 2, 3,]` is invalid
//   * `{"a": 1, "b": 2,}` is invalid
//   * Common source of parse errors
//   * JSON5 allows trailing commas
//
// - **Double quotes only**: No single quotes
//   * `"valid"` ✓
//   * `'invalid'` ✗
//   * Keys must be quoted: `{"key": value}` not `{key: value}`
//   * Different from JavaScript objects
//
// - **No undefined**: Only null exists
//   * JavaScript has both null and undefined
//   * JSON only supports null
//   * Undefined values omitted when stringifying
//
// - **No functions**: Data only
//   * Cannot store functions or methods
//   * Pure data representation
//   * No executable code
//
// - **No Date type**: Use strings or numbers
//   * ISO 8601 strings: "2024-10-09T12:00:00Z"
//   * Unix timestamps: 1728475200000
//   * No standard format (convention-based)
//
// - **Number precision**: IEEE 754 doubles
//   * Integers safe up to 2^53 - 1
//   * Large numbers may lose precision
//   * Use strings for arbitrary precision
//   * No Infinity, -Infinity, or NaN
//
// - **Key uniqueness**: Duplicate keys allowed
//   * `{"a": 1, "a": 2}` is technically valid
//   * Last value wins in most parsers
//   * Should be avoided for clarity
//
// - **No circular references**: Cannot serialize
//   * Object referencing itself causes error
//   * JavaScript JSON.stringify() throws TypeError
//   * Must flatten or transform data
//
// - **Whitespace ignored**: Outside strings
//   * `{"a":1}` same as `{ "a" : 1 }`
//   * Minification removes all unnecessary whitespace
//   * Pretty-printing for human readability
//
// - **Escape sequences**: Required for special chars
//   * `\"` for quote
//   * `\\` for backslash
//   * `\n` for newline
//   * `\t` for tab
//   * `\uXXXX` for Unicode
//
// - **No NaN or Infinity**: Not valid JSON
//   * JavaScript NaN/Infinity become null
//   * Must represent as strings if needed
//   * Or use sentinel values
//
// - **BOM discouraged**: UTF-8 without BOM
//   * Byte Order Mark may cause issues
//   * RFC 8259 says implementations "SHOULD" reject BOM
//   * Some parsers accept it
//
// - **Root must be object or array**: In practice
//   * Technically `"string"` or `42` are valid JSON
//   * Most APIs expect object or array
//   * JSON.parse() accepts primitives
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "I discovered JSON. I do not claim to have invented it" - Douglas Crockford
// - "JSON is the lingua franca of web services" - Industry saying
// - "The fat-free alternative to XML" - Douglas Crockford
// - "JSON is not just a data interchange format, it's a way of thinking about data" - Developer wisdom
// - "JSON's greatest strength is also its weakness: simplicity" - Common observation
// - "No comments in JSON? That's not a bug, that's a feature" - Douglas Crockford's stance
// - "JSON is what XML should have been" - Developer sentiment
// - "YAML is a superset of JSON, and JSON is a subset of JavaScript" - Format relationship
// - "If your API uses XML in 2024, we need to talk" - Modern web developer
//
// NOTES ON JSON SYNTAX
// --------------------
// - Case-sensitive for keys and string values
// - No comments allowed (by design)
// - Keys must be double-quoted strings
// - String values must use double quotes
// - Numbers: integers, decimals, negative, scientific notation
// - No trailing commas allowed
// - Whitespace (spaces, tabs, newlines) ignored outside strings
// - Unicode support via \uXXXX escape sequences
// - Only 6 data types: object, array, string, number, boolean, null
// - Objects use curly braces: { }
// - Arrays use square brackets: [ ]
// - Colon separates key and value: "key": value
// - Comma separates elements (no trailing comma)
// - No undefined, NaN, Infinity, or function types
// - No date type (use ISO 8601 strings)
// - No circular references
// - Root element typically object or array
// - UTF-8 encoding standard
// - MIME type: application/json
// - File extension: .json


// JSON SYNTAX CONFIGURATION FOR DARKSYNTAX
// =========================================
darkSyntax.registerLanguage('json', {
  rules: [
    // PRIORITY 90: Property keys (strings before colon) - Keywords
    // Everything before : is a key/keyword
    {
      class: 'keyword',
      pattern: /"(?:[^"\\]|\\.)*"(?=\s*:)/g,
      priority: 90
    },
    
    // PRIORITY 80: String values (after colon or in arrays)
    // JSON only supports double quotes
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 80
    },
    
    // PRIORITY 30: Numbers (integers and floats, including negative and scientific notation)
    {
      class: 'number',
      pattern: /-?\d+\.?\d*(?:[eE][+-]?\d+)?/g,
      priority: 30
    },
    
    // PRIORITY 20: Boolean and null values
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 20
    }
  ]
});