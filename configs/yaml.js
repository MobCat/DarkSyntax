// darkSyntax/configs/yaml.js - YAML language configuration
// =========================================================
// YAML (2001)
// YAML Ain't Markup Language - Human-friendly data serialization
//
// Configs
// =======================
// ALIASES: ['yaml', 'yml']
// File extensions: .yaml, .yml
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Clark Evans, Ingy döt Net, and Oren Ben-Kiki (2001)
// - First version released (May 11, 2001)
// - Name changed from "Yet Another Markup Language" to recursive acronym (2002)
// - YAML 1.0 specification (January 29, 2004)
// - YAML 1.1 specification (2005)
// - YAML 1.2 specification (October 1, 2009) - aligned with JSON
// - Born from frustration with XML verbosity and complexity
// - Designed to be human-readable and writable
// - Influenced by email format, Python indentation, and Perl data structures
// - Became standard for configuration files in DevOps ecosystem
// - PyYAML library released (2006) - Python implementation
// - SnakeYAML released (2008) - Java implementation
// - js-yaml released (2011) - JavaScript implementation
// - libyaml released (2006) - C implementation by Kirill Simonov
// - YAML 1.2.2 specification (October 1, 2021) - latest revision
// - Adopted by major cloud platforms and orchestration tools
// - Became de facto standard for Kubernetes, Docker, CI/CD pipelines
//
// INFLUENCED
// ----------
// - Kubernetes (2014) - Popularized YAML for infrastructure as code
// - Docker Compose (2014) - Container orchestration configuration
// - Ansible (2012) - Configuration management playbooks
// - GitHub Actions (2019) - CI/CD workflow definitions
// - GitLab CI (2012) - Pipeline configuration
// - CircleCI (2011) - Build configuration
// - Travis CI (2011) - Build automation
// - OpenAPI/Swagger (2011) - API specification format
// - Helm (2015) - Kubernetes package manager charts
// - AWS CloudFormation (2011) - Infrastructure templates
// - Azure Resource Manager (2014) - ARM templates
// - Google Cloud Deployment Manager (2014)
// - Prometheus (2012) - Monitoring configuration
// - Grafana (2014) - Dashboard definitions
// - Jekyll (2008) - Static site generator front matter
// - Home Assistant (2013) - Smart home automation configuration
// - ESLint (2013) - Linting configuration
// - Prettier (2017) - Code formatting configuration
// - conda (2012) - Package management environment files
// - pre-commit (2014) - Git hook configuration
//
// USED FOR
// --------
// - Kubernetes manifests and configurations
// - Docker Compose multi-container definitions
// - CI/CD pipeline definitions (GitHub Actions, GitLab CI, CircleCI)
// - Configuration management (Ansible playbooks, SaltStack)
// - Infrastructure as Code (Terraform, CloudFormation, ARM templates)
// - API specifications (OpenAPI/Swagger)
// - Application configuration files
// - Data serialization and exchange
// - Static site generators (Jekyll, Hugo front matter)
// - Package manager manifests (conda, pubspec)
// - Monitoring and observability (Prometheus, Grafana)
// - Development tool configuration (ESLint, Prettier, pre-commit)
// - Smart home automation (Home Assistant)
// - Localization and translation files
//
// KEY FEATURES
// ------------
// - Human-readable syntax with minimal punctuation
// - Indentation-based structure (like Python)
// - Supports complex data structures (scalars, sequences, mappings)
// - Native data types (strings, integers, floats, booleans, null)
// - Multi-line strings (literal | and folded >)
// - Comments with # character
// - Anchors (&) and aliases (*) for reference reuse
// - Document separation with --- and ...
// - Type tags for explicit typing (!tag)
// - Superset of JSON (all JSON is valid YAML 1.2+)
// - Schema validation support
// - Multiple documents in single file
// - Unicode support
// - Date and timestamp types
// - No closing delimiters needed (unlike JSON/XML)
//
// CORE SYNTAX
// -----------
// Basic key-value pairs:
//   name: John Doe
//   age: 30
//   active: true
//
// Nested objects (indentation):
//   person:
//     name: John Doe
//     address:
//       street: 123 Main St
//       city: Boston
//
// Lists/Arrays:
//   fruits:
//     - apple
//     - banana
//     - orange
//
// Inline syntax:
//   person: {name: John, age: 30}
//   fruits: [apple, banana, orange]
//
// Multi-line strings:
//   description: |
//     This is a literal
//     block of text
//   folded: >
//     This text will be
//     folded into a single line
//
// Anchors and aliases:
//   defaults: &defaults
//     timeout: 30
//   service:
//     <<: *defaults
//     name: api
//
// QUIRKS
// ------
// - **Indentation is syntax**: Spaces only, no tabs allowed
//   * 2 spaces is standard convention
//   * Mixing spaces and tabs causes errors
//   * Indentation level determines nesting
//
// - **Multiple boolean representations**: true/false, yes/no, on/off
//   * All case-insensitive
//   * Can cause confusion: `no` becomes boolean false
//   * Norway country code `NO` parsed as false in YAML 1.1
//
// - **Implicit typing**: YAML guesses types
//   * `123` becomes integer
//   * `1.23` becomes float
//   * `true` becomes boolean
//   * Quote strings to prevent: `"123"` stays string
//
// - **The Norway problem**: Famous YAML gotcha
//   * Country code `NO:` parsed as `false:` in YAML 1.1
//   * Fixed in YAML 1.2 with stricter boolean parsing
//   * Always quote country codes: `"NO": Norway`
//
// - **Null representations**: null, ~, empty value
//   * `key:` with no value = null
//   * `key: ~` = null
//   * `key: null` = null
//
// - **Multi-line strings**: Two styles with different behavior
//   * Literal `|` preserves newlines and trailing spaces
//   * Folded `>` converts newlines to spaces
//   * Block chomping: `|-` `|+` `>-` `>+`
//
// - **Anchors and aliases**: Reuse definitions
//   * `&name` defines anchor
//   * `*name` references it
//   * `<<:` merges mappings (merge key)
//   * Cannot reference across documents
//
// - **Colon in strings**: Requires quotes or space
//   * `url: http://example.com` - WORKS (space after colon)
//   * `url:http://example.com` - ERROR (no space)
//   * `url: "http://example.com"` - WORKS (quoted)
//
// - **List vs mapping confusion**: Starting line matters
//   * `-` starts a list
//   * `key:` starts a mapping
//   * Cannot mix on same indentation level
//
// - **Document markers**: Optional but useful
//   * `---` starts document
//   * `...` ends document
//   * Multiple documents per file supported
//
// - **Comments**: Only # character, no multi-line
//   * Must start with `#` after whitespace
//   * No `/* */` or `<!-- -->` style comments
//   * No inline comments after values (space required)
//
// - **JSON compatibility**: YAML 1.2+ is JSON superset
//   * All valid JSON is valid YAML
//   * But not vice versa (indentation, comments, etc.)
//   * Can mix JSON and YAML syntax
//
// - **Special characters**: Some need quoting
//   * `:` `{` `}` `[` `]` `,` `&` `*` `#` `?` `|` `-` `<` `>` `=` `!` `%` `@` `` ` ``
//   * Context matters - not always required
//   * Safer to quote if unsure
//
// - **Trailing commas**: Not allowed
//   * `[1, 2, 3,]` is invalid
//   * Unlike JSON5 or JavaScript
//
// - **Version directives**: Specify YAML version
//   * `%YAML 1.2` at file start
//   * Rarely used in practice
//   * Parsers may ignore
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "YAML is a human friendly data serialization standard for all programming languages" - Official tagline
// - "YAML: probably not so great after all" - Hacker News thread title (2020)
// - "The Norway Problem" - Famous YAML gotcha where NO: was parsed as false:
// - "YAML is the new XML" - Common developer saying about config complexity
// - "Tabs vs spaces: YAML chose spaces and made it syntax" - Developer joke
// - "YAML: YAML Ain't Markup Language, but it's marked up anyway" - Recursive acronym joke
// - "If you think YAML is easier than JSON, try debugging an indentation error" - Anonymous
// - "YAML is great until you need to generate it programmatically" - DevOps wisdom
//
// NOTES ON YAML SYNTAX
// --------------------
// - Case-sensitive for keys and values
// - Comments use # character (no multi-line comments)
// - Indentation with spaces only (no tabs)
// - 2-space indentation is standard
// - Keys followed by colon and space: `key: value`
// - Lists use dash and space: `- item`
// - Inline collections: `[1, 2, 3]` and `{a: 1, b: 2}`
// - Multi-line strings: | (literal) and > (folded)
// - Anchors: &name to define, *name to reference
// - Document separator: ---
// - Document terminator: ...
// - Null values: null, ~, or empty
// - Boolean: true/false, yes/no, on/off (case-insensitive)
// - No closing brackets/braces needed for indented blocks
// - Strings can be unquoted if no special characters
// - Quote with " or ' when needed
// - Escape sequences in double quotes only
// - Type tags: !tag for explicit typing
// - Schema allows custom types
// - Multiple documents per file supported


// YAML Syntax Configuration for DarkSyntax
// =========================================

darkSyntax.registerLanguage('yaml', {
  rules: [
    // PRIORITY 100: Comments (must come first)
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings - Double quotes
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 90: Strings - Single quotes
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 90
    },
    
    // PRIORITY 85: Multi-line string indicators (just the | or > character and modifiers)
    {
      class: 'keyword',
      pattern: /[|>][-+]?(?=\s*$)/gm,
      priority: 85
    },
    
    // PRIORITY 60: Document separators (---)
    {
      class: 'keyword',
      pattern: /^---$/gm,
      priority: 60
    },
    
    // PRIORITY 60: Document end markers (...)
    {
      class: 'keyword',
      pattern: /^\.\.\.$/gm,
      priority: 60
    },
    
    // PRIORITY 55: Anchors and aliases (&anchor, *alias)
    {
      class: 'decorator',
      pattern: /[&*][a-zA-Z0-9_-]+/g,
      priority: 55
    },
    
    // PRIORITY 50: Special YAML values (must come after colon)
    {
      class: 'boolean',
      pattern: /:\s+(true|false|yes|no|on|off|null)(?=\s|$)/gim,
      captureGroup: 1,
      priority: 50
    },
    
    // PRIORITY 50: Null tilde (must come after colon)
    {
      class: 'boolean',
      pattern: /:\s+(~)(?=\s|$)/gm,
      captureGroup: 1,
      priority: 50
    },
    
    // PRIORITY 45: Tags (!tag)
    {
      class: 'decorator',
      pattern: /![a-zA-Z0-9_-]+/g,
      priority: 45
    },
    
    // PRIORITY 40: Keys (word followed by colon)
    {
      class: 'function',
      pattern: /^[ \t]*[a-zA-Z0-9_-]+(?=\s*:)/gm,
      priority: 40
    },
    
    // PRIORITY 40: Quoted keys
    {
      class: 'function',
      pattern: /^[ \t]*["'][^"']+["'](?=\s*:)/gm,
      priority: 40
    },
    
    // PRIORITY 35: Unquoted string values (after colon, excluding numbers/booleans/special chars)
    {
      class: 'string',
      pattern: /:\s+([A-Za-z_][^\n\r#]*?)(?=\s*(?:#|$))/gm,
      captureGroup: 1,
      priority: 35
    },
    
    // PRIORITY 30: List markers (-)
    {
      class: 'keyword',
      pattern: /^[ \t]*-(?=\s)/gm,
      priority: 30
    },
    
    // PRIORITY 26: Version numbers and dates (prevent number highlighting)
    {
      class: 'string',
      pattern: /:\s+(\d+\.\d+\.\d+)\b/gm,
      captureGroup: 1,
      priority: 26
    },
    
    // PRIORITY 26: ISO 8601 timestamps (after colon)
    {
      class: 'string',
      pattern: /:\s+(\d{4}-\d{2}-\d{2}(?:[T\s]\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})?)?)/gm,
      captureGroup: 1,
      priority: 26
    },
    
    // PRIORITY 26: ISO 8601 timestamps (after tags like !!timestamp)
    {
      class: 'string',
      pattern: /!\w+\s+(\d{4}-\d{2}-\d{2}(?:[T\s]\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})?)?)/gm,
      captureGroup: 1,
      priority: 26
    },
    
    // PRIORITY 25: Numbers (integers and floats) - after colon
    {
      class: 'number',
      pattern: /:\s+(-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)\s*(?:#|$)/gm,
      captureGroup: 1,
      priority: 25
    },
    
    // PRIORITY 25: Hexadecimal numbers
    {
      class: 'number',
      pattern: /:\s+(0x[0-9a-fA-F]+)\b/gm,
      captureGroup: 1,
      priority: 25
    },
    
    // PRIORITY 25: Octal numbers
    {
      class: 'number',
      pattern: /:\s+(0o[0-7]+)\b/gm,
      captureGroup: 1,
      priority: 25
    }
  ]
});