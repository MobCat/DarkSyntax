// darkSyntax/configs/sql.js - SQL language configuration
// ========================================================
// SQL (1974) / SEQUEL (1970s)
// SQL - Structured Query Language
//
// Configs
// =======================
// ALIASES: ['sql', 'mysql', 'postgresql', 'tsql', 'plsql']
// File extensions: .sql, .ddl, .dml
//
// SQL SYNTAX NOTES
// ================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// SEQUEL (1970s):
// - Created by Donald Chamberlin and Raymond Boyce at IBM
// - Developed for IBM's System R relational database
// - Originally "SEQUEL" (Structured English Query Language)
// - Renamed to "SQL" due to trademark issues
//
// SQL (1986 standardized):
// - First ANSI standard: SQL-86
// - SQL-89, SQL-92 (major), SQL:1999 (added recursion, triggers)
// - SQL:2003 (XML), SQL:2011 (temporal), SQL:2016 (JSON)
// - Most widely used database language
//
// INFLUENCED
// ----------
// - All modern databases (MySQL, PostgreSQL, Oracle, SQL Server)
// - NoSQL query languages (CQL for Cassandra)
// - LINQ (Language Integrated Query in C#)
// - Pandas (Python data analysis)
// - R's dplyr
//
// USED FOR
// --------
// - Relational database management (RDBMS)
// - Data warehousing and analytics
// - Business intelligence
// - Web applications and APIs
// - Financial systems
// - Every major enterprise system
//
// KEY FEATURES
// ------------
// - Declarative (say WHAT you want, not HOW)
// - Set-based operations (not procedural loops)
// - ACID transactions (Atomicity, Consistency, Isolation, Durability)
// - Joins (combining tables)
// - Aggregations (SUM, COUNT, AVG)
// - Subqueries and CTEs (WITH clause)
//
// COMMON SYNTAX
// -------------
// - Case-insensitive keywords: SELECT = select = SeLeCt
// - Comments: -- single line, /* multi-line */
// - Strings: 'single quotes' (standard)
// - Identifiers: "double quotes" or `backticks` (MySQL)
// - Statements end with semicolon: SELECT * FROM users;
// - JOIN syntax: FROM table1 JOIN table2 ON condition
// - WHERE filters rows, HAVING filters groups
// - NULL is special (not equal to anything, even itself)
//
// SQL DIALECTS
// ------------
// This config supports most SQL dialects:
// - MySQL/MariaDB (backticks, # comments)
// - PostgreSQL (advanced features, arrays)
// - SQL Server (T-SQL, @@ variables)
// - Oracle (PL/SQL)
// - SQLite (lightweight, no RIGHT JOIN)
//
// NOTES ON SQL HIGHLIGHTING
// --------------------------
// SQL has several challenges:
// 1. Case-insensitive keywords (SELECT = select = SeLeCt)
// 2. Multiple comment styles (-- and /* */ and #)
// 3. Different string delimiters (' for strings, " and ` for identifiers)
// 4. Many keywords and built-in functions
// 5. Multiple SQL dialects (MySQL, PostgreSQL, SQL Server, Oracle)
//
// FEATURES COVERED:
// - All major SQL commands (DDL, DML, DCL)
// - Data types (INT, VARCHAR, TIMESTAMP, etc.)
// - Built-in functions (COUNT, SUM, CONCAT, NOW, etc.)
// - Operators (AND, OR, NOT, IN, LIKE, BETWEEN)
// - Comments (single-line and multi-line)
// - Variables (@variable, @@system_variable)
// - Parameter placeholders (:param)
// - Table.column references
//
// SQL DIALECTS
// ------------
// This config supports most SQL dialects:
// - MySQL/MariaDB (backticks, # comments)
// - PostgreSQL (advanced features, arrays)
// - SQL Server (T-SQL, @@ variables)
// - Oracle (PL/SQL)
// - SQLite (lightweight, simplified)
//
// SQL SYNTAX CONFIGURATION FOR DARKSYNTAX
// ========================================

darkSyntax.registerLanguage('sql', {
  rules: [
    // PRIORITY 100: Comments must come first
    // Single-line comments (-- style)
    {
      class: 'comment',
      pattern: /--.*$/gm,
      priority: 100
    },
    
    // Multi-line comments (/* */ style)
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // Hash comments (MySQL style)
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    // Single-quoted strings (standard SQL)
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 90
    },
    
    // Double-quoted identifiers (some SQL dialects use these for table/column names)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // Backtick identifiers (MySQL)
    {
      class: 'string',
      pattern: /`(?:[^`\\]|\\.)*`/g,
      priority: 90
    },
    
    // PRIORITY 60: SQL Keywords (main commands)
    {
      class: 'keyword',
      pattern: /\b(SELECT|INSERT|UPDATE|DELETE|CREATE|ALTER|DROP|TRUNCATE|FROM|WHERE|JOIN|INNER|LEFT|RIGHT|FULL|OUTER|CROSS|ON|USING|GROUP BY|HAVING|ORDER BY|LIMIT|OFFSET|UNION|INTERSECT|EXCEPT|AS|INTO|VALUES|SET|TABLE|DATABASE|INDEX|VIEW|PROCEDURE|FUNCTION|TRIGGER|CONSTRAINT|PRIMARY KEY|FOREIGN KEY|UNIQUE|CHECK|DEFAULT|NOT NULL|AUTO_INCREMENT|IDENTITY)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: More SQL Keywords
    {
      class: 'keyword',
      pattern: /\b(AND|OR|NOT|IN|BETWEEN|LIKE|IS|NULL|EXISTS|ANY|ALL|CASE|WHEN|THEN|ELSE|END|IF|BEGIN|END|RETURN|DECLARE|WITH|RECURSIVE|DISTINCT|TOP|FETCH|ROWS|ONLY)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Conditional and logical operators
    {
      class: 'keyword',
      pattern: /\b(WHERE|HAVING|ON)\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: Data types
    {
      class: 'builtin',
      pattern: /\b(INT|INTEGER|BIGINT|SMALLINT|TINYINT|DECIMAL|NUMERIC|FLOAT|REAL|DOUBLE|VARCHAR|CHAR|TEXT|BLOB|DATE|TIME|DATETIME|TIMESTAMP|YEAR|BOOLEAN|BOOL|BIT|BINARY|VARBINARY|ENUM|SET|JSON|XML|UUID)\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(COUNT|SUM|AVG|MIN|MAX|ROUND|FLOOR|CEIL|ABS|SQRT|POWER|RAND|NOW|CURDATE|CURTIME|DATE_FORMAT|YEAR|MONTH|DAY|CONCAT|SUBSTRING|LENGTH|UPPER|LOWER|TRIM|REPLACE|COALESCE|NULLIF|CAST|CONVERT)\b/gi,
      priority: 40
    },
    
    // PRIORITY 35: Function calls (custom functions)
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },
    
    // PRIORITY 30: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 30
    },
    
    // PRIORITY 25: Table and column references with dots
    {
      class: 'function',
      pattern: /\b[a-zA-Z_][a-zA-Z0-9_]*\s*\.[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 25
    },
    
    // PRIORITY 20: Boolean and NULL
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE|NULL)\b/gi,
      priority: 20
    },
    
    // PRIORITY 15: Variables (@ prefix for user variables, @@ for system)
    {
      class: 'decorator',
      pattern: /@@?[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 15
    },
    
    // PRIORITY 10: Parameter placeholders
    {
      class: 'decorator',
      pattern: /:[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 10
    }
  ]
});