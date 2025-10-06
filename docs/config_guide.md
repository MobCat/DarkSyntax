# Creating Syntax Configs for DarkSyntax

Complete guide to building language syntax highlighting configurations.

## Table of Contents
- [Quick Start](#quick-start)
- [File Structure](#file-structure)
- [Rule System](#rule-system)
- [Available CSS Classes](#available-css-classes)
- [Priority System](#priority-system)
- [Common Patterns](#common-patterns)
- [Testing Your Config](#testing-your-config)
- [Common Mistakes](#common-mistakes)
- [Language Documentation Template](#language-documentation-template)

---

## Quick Start

1. Create `darkSyntax/configs/yourlang.js`
2. Add aliases to `darkSyntax/aliases.js`: `['yourlang', 'yl']`
3. Write your config following the template
4. Test with `<pre code="yourlang">code here</pre>`

---

## File Structure

Every config file has three parts:

### 1. Language Documentation (Comments)
```javascript
// darkSyntax/configs/python.js - Python language configuration
// ============================================================
// Python (1991)
// Python - High-level interpreted language
//
// Configs
// =======================
// ALIASES: ['python', 'py', 'pyw']
// File extensions: .py, .pyw
//
// [Historical info, features, quirks, etc.]
```

### 2. Demo Code (Commented Out)
```javascript
/* DEMO CODE - Sample code for testing
   ====================================

def hello():
    print("Hello, World!")

*/
```

### 3. Language Registration
```javascript
darkSyntax.registerLanguage('python', {
  rules: [
    // Your syntax rules here
  ]
});
```

---

## Rule System

Each rule is an object with these properties:

### Required Properties

**`class`** - CSS class to apply (string)
```javascript
class: 'keyword'  // Uses ds-keyword from theme
```

**`pattern`** - JavaScript RegExp to match (RegExp)
```javascript
pattern: /\b(if|else|for)\b/g  // MUST include 'g' flag
```

### Optional Properties

**`priority`** - Execution order (number, default: 0)
```javascript
priority: 100  // Higher runs first
```

**`captureGroup`** - Which regex group to highlight (number)
```javascript
pattern: /function\s+([a-zA-Z_]\w*)/g,
captureGroup: 1  // Only highlights the function name
```

### Complete Rule Example
```javascript
{
  class: 'function',
  pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
  captureGroup: 1,
  priority: 45
}
```

---

## Available CSS Classes

DarkSyntax supports 10 standard syntax classes:

| Class | Usage | Examples |
|-------|-------|----------|
| `keyword` | Language keywords | `if`, `for`, `class`, `def` |
| `string` | String literals | `"text"`, `'char'` |
| `comment` | Code comments | `// comment`, `/* block */` |
| `class` | Type/class names | `MyClass`, `String`, `List<T>` |
| `function` | Function names | `myFunc()`, `obj.method()` |
| `builtin` | Built-in types/functions | `print`, `int`, `std::vector` |
| `number` | Numeric literals | `42`, `3.14`, `0xFF` |
| `boolean` | Boolean/null values | `true`, `false`, `null` |
| `decorator` | Annotations/pragmas | `@decorator`, `#include` |
| `variable` | Variables/fields | `$var`, `@param`, `this` |

### Class Priority Guidelines

When multiple classes could apply to the same token:

1. **Classes** come before **functions** (declarations before usage)
2. **Built-ins** and **functions** are similar priority
3. **Variables** are usually lower priority
4. **Decorators** match the thing they decorate

---

## Priority System

**CRITICAL**: Priority determines which rule wins when patterns overlap.

### Standard Priority Ranges

```
100-110  Comments (MUST be first!)
90-99    Strings (before keywords!)
70-89    Decorators, preprocessors
60-69    Class/type definitions
50-59    Keywords
40-49    Built-in types/functions
30-45    Function definitions and calls
20-29    Numbers, booleans
10-19    Variables, identifiers
0-9      Everything else
```

### Why Priority Matters

```javascript
// WRONG - Keywords will match inside comments!
rules: [
  { class: 'keyword', pattern: /\bif\b/g, priority: 50 },
  { class: 'comment', pattern: /\/\/.*$/gm, priority: 100 }
]

// CORRECT - Comments processed first
rules: [
  { class: 'comment', pattern: /\/\/.*$/gm, priority: 100 },
  { class: 'keyword', pattern: /\bif\b/g, priority: 50 }
]
```

### Priority Examples

```javascript
// String inside comment: "// this is a string"
// Should be: [comment] not [comment][string][comment]
{ class: 'comment', pattern: /\/\/.*$/gm, priority: 100 },  // ← Higher
{ class: 'string', pattern: /".*"/g, priority: 90 }

// Keyword inside string: "if (true)"
// Should be: [string] not [string][keyword][string]
{ class: 'string', pattern: /".*"/g, priority: 90 },  // ← Higher
{ class: 'keyword', pattern: /\bif\b/g, priority: 50 }
```

---

## Common Patterns

### Comments

**Single-line** (// style):
```javascript
{
  class: 'comment',
  pattern: /\/\/.*$/gm,  // m flag for multiline
  priority: 100
}
```

**Multi-line** (/* */ style):
```javascript
{
  class: 'comment',
  pattern: /\/\*[\s\S]*?\*\//g,  // [\s\S] matches any character including newlines
  priority: 100
}
```

**Hash comments** (# style):
```javascript
{
  class: 'comment',
  pattern: /#.*$/gm,
  priority: 100
}
```

### Strings

**Double-quoted with escapes**:
```javascript
{
  class: 'string',
  pattern: /"(?:[^"\\]|\\.)*"/g,  // Handles \" inside strings
  priority: 90
}
```

**Single-quoted**:
```javascript
{
  class: 'string',
  pattern: /'(?:[^'\\]|\\.)*'/g,
  priority: 90
}
```

**Raw strings** (Python r"text"):
```javascript
{
  class: 'string',
  pattern: /r["'](?:[^"'\\]|\\.)*["']/g,
  priority: 90
}
```

### Keywords

**Basic keywords with word boundaries**:
```javascript
{
  class: 'keyword',
  pattern: /\b(if|else|for|while|return)\b/g,  // \b prevents matching "iffy"
  priority: 50
}
```

**Case-insensitive** (BASIC, SQL):
```javascript
{
  class: 'keyword',
  pattern: /\b(IF|THEN|ELSE|END)\b/gi,  // i flag for case-insensitive
  priority: 50
}
```

### Functions

**Function calls** (name followed by `(`):
```javascript
{
  class: 'function',
  pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,  // Lookahead doesn't consume (
  priority: 30
}
```

**Method calls** (object.method):
```javascript
{
  class: 'function',
  pattern: /\.([a-zA-Z_][a-zA-Z0-9_]*)/g,
  captureGroup: 1,  // Only highlight method name, not the dot
  priority: 30
}
```

**Function definitions**:
```javascript
{
  class: 'function',
  pattern: /\bfunction\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
  captureGroup: 1,
  priority: 35
}
```

### Numbers

**Integer and decimal**:
```javascript
{
  class: 'number',
  pattern: /\b\d+\.?\d*\b/g,  // Matches 42, 3.14
  priority: 20
}
```

**Hexadecimal**:
```javascript
{
  class: 'number',
  pattern: /\b0[xX][0-9A-Fa-f]+\b/g,  // 0xFF, 0x1A2B
  priority: 20
}
```

**Binary** (modern languages):
```javascript
{
  class: 'number',
  pattern: /\b0[bB][01]+\b/g,  // 0b1010
  priority: 20
}
```

**Scientific notation**:
```javascript
{
  class: 'number',
  pattern: /\b\d+\.?\d*([eE][+-]?\d+)?\b/g,  // 1.23e-4
  priority: 20
}
```

### Variables

**Field references** (AWK $1, $NF):
```javascript
{
  class: 'variable',
  pattern: /\$[0-9]+|\$NF|\$[a-zA-Z_][a-zA-Z0-9_]*/g,
  priority: 75
}
```

**Variable declarations**:
```javascript
{
  class: 'variable',
  pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*=/g,
  captureGroup: 1,
  priority: 15
}
```

### Classes/Types

**Class definitions**:
```javascript
{
  class: 'class',
  pattern: /\b(class|struct|interface)\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
  captureGroup: 2,
  priority: 65
}
```

**Type annotations**:
```javascript
{
  class: 'class',
  pattern: /:\s*([A-Z][a-zA-Z0-9_]*)/g,  // : TypeName
  captureGroup: 1,
  priority: 40
}
```

---

## Testing Your Config

### 1. Create Test HTML
```html
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="darkSyntax/themes/darkSyntax-sublime.css">
  <script src="darkSyntax/darkSyntax.js"></script>
</head>
<body>
  <pre code="yourlang">
    // Your test code here
    keyword test;
    "string test"
    function testFunc() {}
  </pre>
</body>
</html>
```

### 2. Test Edge Cases

Always test these scenarios:

```
✓ Keywords in strings: "if (true)" - should be ONE string color
✓ Keywords in comments: // if while - should be ONE comment color
✓ Numbers in strings: "42" - should be string, not number
✓ Strings with escapes: "say \"hello\"" - should handle \"
✓ Multi-line comments: /* line1\nline2 */ - both lines comment
✓ Function calls: myFunc() - "myFunc" should be function color
✓ Nested syntax: "string /*not comment*/" - string wins
```

### 3. Use Browser DevTools

Inspect the HTML output:
```html
<pre code="python" class="ds-highlighted">
  <span class="ds-keyword">if</span>
  <span class="ds-string">"test"</span>
</pre>
```

### 4. Check for Errors

Open browser console (F12) and look for:
- `DarkSyntax: Failed to load config for language...`
- `DarkSyntax: Missing CSS classes in theme: ds-...`
- JavaScript regex errors

---

## Common Mistakes

### 1. Missing 'g' Flag
```javascript
// WRONG - Only matches first occurrence
pattern: /keyword/

// CORRECT - Matches all occurrences
pattern: /keyword/g
```

### 2. Missing Word Boundaries
```javascript
// WRONG - Matches "return" in "returned"
pattern: /return/g

// CORRECT - Only matches whole word
pattern: /\breturn\b/g
```

### 3. Wrong Priority Order
```javascript
// WRONG - Keywords match before strings!
rules: [
  { class: 'keyword', pattern: /\bif\b/g, priority: 50 },
  { class: 'string', pattern: /".*"/g, priority: 90 }
]

// CORRECT - Always: Comments → Strings → Keywords
rules: [
  { class: 'comment', pattern: /\/\/.*$/gm, priority: 100 },
  { class: 'string', pattern: /".*"/g, priority: 90 },
  { class: 'keyword', pattern: /\bif\b/g, priority: 50 }
]
```

### 4. Greedy String Matching
```javascript
// WRONG - Matches from first " to last " on the line
pattern: /".*"/g  // "hello" and "world" becomes one match

// CORRECT - Non-greedy, stops at first closing "
pattern: /".*?"/g  // Matches "hello" and "world" separately
```

### 5. Unescaped Special Characters
```javascript
// WRONG - + is a regex operator
pattern: /a+b/g  // Matches "ab", "aaab", "aaaab"

// CORRECT - Escape special chars
pattern: /a\+b/g  // Matches literal "a+b"
```

Special characters that need escaping: `. * + ? ^ $ { } ( ) | [ ] \`

### 6. Not Handling Escape Sequences
```javascript
// WRONG - Breaks on strings like "say \"hello\""
pattern: /"[^"]*"/g  // Stops at \" thinking string ended

// CORRECT - Handles escaped quotes
pattern: /"(?:[^"\\]|\\.)*"/g  // Allows \" inside string
```

### 7. Wrong Capture Group
```javascript
// Pattern: /function\s+([a-zA-Z_]\w*)\s*\(/g
// Groups:   0=whole match, 1=function name

// WRONG - Highlights "function myFunc ("
captureGroup: 0

// CORRECT - Highlights only "myFunc"
captureGroup: 1
```

---

## Language Documentation Template

Use this template for the header comments in your config file:

```javascript
// darkSyntax/configs/yourlang.js - Language Name configuration
// ============================================================
// Language Name (Year)
// Brief Description
//
// Configs
// =======================
// ALIASES: ['yourlang', 'yl', 'ylang']
// File extensions: .yl, .ylang
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// [Who created it, when, where, why]
// [Major versions and milestones]
// [Impact on programming history]
//
// INFLUENCED
// ----------
// [Languages that borrowed from this one]
// [Concepts that spread to other languages]
//
// USED FOR
// --------
// [Main applications and domains]
// [Industries and use cases]
//
// KEY FEATURES
// ------------
// [Unique or notable features]
// [What makes this language special]
//
// CORE SYNTAX
// -----------
// [Basic syntax examples]
// [Common patterns and idioms]
//
// QUIRKS
// ------
// [Unusual behaviors]
// [Common gotchas]
// [Things that surprise new users]
//
// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================

/* DEMO CODE - Copy everything below into a test file
   ===================================================

[Your demo code here - should demonstrate ALL syntax classes]

*/

darkSyntax.registerLanguage('yourlang', {
  rules: [
    // Your rules here
  ]
});
```

---

## Tips & Best Practices

1. **Start simple**: Begin with just comments, strings, and keywords
2. **Test frequently**: Add one rule type at a time and test
3. **Copy patterns**: Most languages share similar syntax for comments/strings
4. **Use regex101.com**: Test your patterns before adding them
5. **Look at existing configs**: Find a similar language and adapt it
6. **Document thoroughly**: Future you will thank you
7. **Test edge cases**: Try to break your highlighting
8. **Check browser console**: Look for errors and warnings

---

## Examples by Complexity

### Simple Language (BASIC)
- No classes/objects
- Simple keywords and operators
- Line numbers as special case
- Case-insensitive

### Medium Language (Python)
- Indentation-sensitive (but not for highlighting)
- Decorators (@)
- Multiple string types (', ", ''', """)
- Built-in functions

### Complex Language (C++)
- Preprocessor directives (#include)
- Templates with < >
- Multiple inheritance
- Operator overloading
- Namespaces (std::)

---

## Getting Help

1. Check existing configs for similar languages
2. Test your regex patterns at regex101.com
3. Use browser DevTools to inspect HTML output
4. Check console for DarkSyntax warnings
5. Start with a working config and modify it

---

## Quick Reference Card

```javascript
// Priority order (higher first):
// 100+  Comments
// 90+   Strings
// 70-89 Decorators
// 60-69 Classes
// 50-59 Keywords
// 40-49 Builtins
// 30-45 Functions
// 20-29 Numbers/Booleans
// 10-19 Variables
// 0-9   Everything else

// Always include 'g' flag!
pattern: /regex/g

// Use \b for word boundaries
pattern: /\bword\b/g

// Handle escapes in strings
pattern: /"(?:[^"\\]|\\.)*"/g

// Use captureGroup to highlight part of match
pattern: /function\s+(\w+)/g,
captureGroup: 1

// Test: Comments → Strings → Keywords
```

---

*Last updated: 2025*
*DarkSyntax Version: 1.0*
