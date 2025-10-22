# Creating Themes for DarkSyntax

Complete guide to building custom syntax highlighting themes.

## Table of Contents
- [Quick Start](#quick-start)
- [Theme Structure](#theme-structure)
- [Required CSS Classes](#required-css-classes)
- [Syntax Token Classes](#syntax-token-classes)
- [Choosing Colors](#choosing-colors)
- [Testing Your Theme](#testing-your-theme)
- [Theme Examples](#theme-examples)

---

## Quick Start

1. Copy an existing theme from `darkSyntax/themes/`
2. Rename it to `darkSyntax-yourtheme.css`
3. Change the colors in the syntax token section
4. Test with `<link rel="stylesheet" href="darkSyntax/themes/darkSyntax-yourtheme.css">`

---

## Theme Structure

A DarkSyntax theme consists of two main parts:

### 1. UI Components (Wrapper, Titlebar, Buttons, Line Numbers)
These control the appearance of the code block wrapper, not the syntax highlighting.

### 2. Syntax Token Classes (The Important Part)
These are the `ds-*` classes that actually color your code.

---

## Required CSS Classes

### UI Components

You can customize these however you like:

```css
.ds-wrapper          /* Outer container */
.ds-titlebar         /* Top bar with filename */
.ds-title-left       /* Left side of titlebar */
.ds-title-right      /* Right side of titlebar */
.ds-btn              /* Copy/download buttons */
.ds-btn:hover        /* Button hover state */
.ds-btn.ds-success   /* Success state (after copy) */
.ds-content          /* Code content area */
.ds-line-numbers     /* Line numbers column */
pre[code]            /* The actual code block */
```

### Syntax Token Classes (REQUIRED)

These **must** be defined for syntax highlighting to work:

```css
.ds-keyword      /* Language keywords */
.ds-string       /* String literals */
.ds-comment      /* Code comments */
.ds-class        /* Class/type names */
.ds-function     /* Function names */
.ds-builtin      /* Built-in types/functions */
.ds-number       /* Numeric literals */
.ds-boolean      /* Boolean/null values */
.ds-decorator    /* Annotations/attributes/modifiers */
.ds-operator     /* Mathematical and logical operators */
.ds-variable     /* Variables/field references */
```

**Missing any of these will trigger a warning** in the browser console:
```
DarkSyntax: Missing CSS classes in theme: ds-operator
```

---

## Syntax Token Classes

Here's what each class is used for across different languages:

### ds-keyword
**Control flow, declarations, reserved words**

Examples:
- `if`, `else`, `for`, `while`, `return` (most languages)
- `class`, `function`, `var`, `let`, `const` (JavaScript)
- `def`, `import`, `from` (Python)
- `public`, `private`, `static` (Java, C++)
- `fn`, `struct`, `enum` (Rust)
- `BEGIN`, `END` (AWK)

**Color suggestions:**
- Bright/bold color to stand out
- Pink, red, blue, or purple are common
- Examples: `#f92672` (Monokai pink), `#569cd6` (VS Code blue)

### ds-string
**Text literals, characters**

Examples:
- `"hello world"` (most languages)
- `'character'` (C, C++)
- `` `template ${string}` `` (JavaScript)
- `r"raw string"` (Python)
- `/regex/` (AWK, JavaScript)

**Color suggestions:**
- Warm colors: yellow, orange, beige
- Easy on the eyes since strings are common
- Examples: `#e6db74` (Monokai yellow), `#ce9178` (VS Code orange)

### ds-comment
**Code comments**

Examples:
- `// single line` (C-style)
- `/* multi line */` (C-style)
- `# comment` (Python, Bash, AWK)
- `<!-- HTML comment -->`
- `' BASIC comment`

**Color suggestions:**
- Muted/dim color (comments are less important)
- Gray, dark green, or brown
- Often italic
- Examples: `#75715e` (Monokai gray), `#6a9955` (VS Code green)

### ds-class
**User-defined types, class names**

Examples:
- `MyClass` in `class MyClass` (OOP languages)
- `Person` in `Person user = new Person()` (Java, C#)
- `List<String>` (generics)
- Type annotations in TypeScript/Python
- `i32`, `u64`, `String` (Rust types)

**Color suggestions:**
- Bright, important color (types are significant)
- Green, cyan, or teal
- Often same family as functions
- Examples: `#a6e22e` (Monokai green), `#4ec9b0` (VS Code teal)

### ds-function
**Function and method names**

Examples:
- `myFunction()` (function calls)
- `object.method()` (method calls)
- `function myFunc()` (definitions)
- `def my_func():` (Python)
- `fn add(a: i32)` (Rust)

**Color suggestions:**
- Distinct but not overpowering
- Lighter green, yellow-green, cyan, or yellow
- Related to class color but distinguishable
- Examples: `#a6da95` (light green), `#dcdcaa` (VS Code yellow), `#4b96a7` (ST4 teal)

### ds-builtin
**Built-in types, standard library functions, macros**

Examples:
- `print`, `len`, `range` (Python)
- `console`, `document` (JavaScript)
- `std::vector`, `std::string` (C++)
- `Integer`, `String` (Java)
- `println!`, `vec!`, `format!` (Rust macros)
- `NR`, `NF`, `FS` (AWK built-in variables)

**Color suggestions:**
- Distinct from user functions
- Cyan, blue, or purple
- Often italic to emphasize they're built-in
- Examples: `#66d9ef` (Monokai cyan), `#4ec9b0` (VS Code cyan), `#628fb4` (ST4 blue)

### ds-number
**Numeric literals**

Examples:
- `42`, `3.14` (decimal)
- `0xFF`, `0x1A2B` (hexadecimal)
- `0b1010` (binary)
- `1.23e-4` (scientific notation)
- `100_000` (with underscores)

**Color suggestions:**
- Distinct color, often purple or orange
- Should be different from strings
- Examples: `#ae81ff` (Monokai purple), `#b5cea8` (VS Code light green)

### ds-boolean
**Boolean and null values**

Examples:
- `true`, `false` (most languages)
- `True`, `False` (Python)
- `null`, `nullptr` (C/C++)
- `NULL`, `nil`, `None`

**Color suggestions:**
- Often same as keywords or numbers
- Orange, blue, or purple
- Examples: `#fd971f` (Monokai orange), `#569cd6` (VS Code blue), `#ae81ff` (purple)

### ds-decorator
**Annotations, attributes, modifiers, preprocessor directives**

Examples:
- `@decorator` (Python)
- `@Override`, `@Entity` (Java)
- `#[derive(Debug)]` (Rust attributes)
- `#include`, `#define` (C/C++ preprocessor)
- `[Attribute]` (C#)
- `mut`, `pub`, `unsafe` (Rust modifiers)
- `pragma` (Ada)
- Shebangs: `#!/bin/bash`

**Color suggestions:**
- Often matches keywords or stands out
- Pink, magenta, orange, or purple
- Examples: `#f92672` (Monokai pink), `#c586c0` (VS Code purple), `#f27754` (ST4 orange)

### ds-operator
**Mathematical and logical operators**

Examples:
- Math: `+`, `-`, `*`, `/`, `%`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`, `&`, `|`, `^`
- Other: `->`, `::`, `.`, `?:`, `??`

**Color suggestions:**
- Distinct from keywords
- Orange, red, or bright color
- Should be readable but not overpowering
- Examples: `#f27754` (ST4 orange), `#d4d4d4` (VS Code default), `#f92672` (Monokai pink)

**Note:** Older languages (C, Pascal, BASIC) may not use this class extensively, as operators were often left unstyled. Modern languages (Rust, TypeScript, Swift) benefit from operator highlighting.

### ds-variable
**Variables, parameters, field references**

Examples:
- `$1`, `$2`, `$NF` (AWK field references)
- `$variable` (Bash, PHP)
- `@parameter` (Ruby)
- `this`, `self` (special variables)
- Variable names in declarations

**Color suggestions:**
- Distinct from functions and builtins
- Light cyan, light blue, or white/default text color
- Should be readable but not dominant
- Examples: `#a1efe4` (light cyan), `#9cdcfe` (VS Code light blue), `#fcfcfa` (default/white)

---

## Choosing Colors

### General Principles

1. **Contrast is king**: Ensure text is readable against background
2. **Hierarchy matters**: Important tokens (keywords, classes) should be brighter
3. **Related = Similar**: Functions and classes can be similar hues
4. **Test with real code**: Don't just look at swatches

### Color Relationships

**Group 1 - High Priority (Bright)**
- Keywords
- Classes
- Decorators
- Operators

**Group 2 - Medium Priority (Vivid)**
- Functions
- Builtins
- Strings

**Group 3 - Low Priority (Muted)**
- Comments (most muted)
- Numbers
- Booleans
- Variables

### Accessibility Considerations

**For color blindness:**
- Don't rely on red/green alone
- Use brightness and saturation differences
- Consider adding bold or italic styles
- Test with color blindness simulators

**For high contrast:**
- Use pure colors (#FF0000, #00FF00, #0000FF)
- Add font-weight: bold to important tokens
- Avoid mid-range grays
- Ensure background is pure black or white

### Common Color Palettes

**Monokai-style (dark, warm)**
```css
background: #272822;
text: #f8f8f2;
keyword: #f92672;    /* pink */
string: #e6db74;     /* yellow */
comment: #75715e;    /* gray */
function: #a6e22e;   /* green */
builtin: #66d9ef;    /* cyan */
operator: #f92672;   /* pink */
```

**VS Code-style (dark, cool)**
```css
background: #1e1e1e;
text: #d4d4d4;
keyword: #569cd6;    /* blue */
string: #ce9178;     /* orange */
comment: #6a9955;    /* green */
function: #dcdcaa;   /* yellow */
builtin: #4ec9b0;    /* teal */
operator: #d4d4d4;   /* default */
```

**GitHub-style (dark)**
```css
background: #0d1117;
text: #c9d1d9;
keyword: #ff7b72;    /* red */
string: #a5d6ff;     /* blue */
comment: #8b949e;    /* gray */
function: #d2a8ff;   /* purple */
builtin: #79c0ff;    /* cyan */
operator: #ff7b72;   /* red */
```

**Sublime Text 4 (dark)**
```css
background: #303841;
text: #fcfcfa;
keyword: #fb6a9f;    /* pink */
string: #e6db74;     /* yellow */
comment: #75715e;    /* gray */
function: #4b96a7;   /* teal */
builtin: #628fb4;    /* blue */
operator: #f27754;   /* orange */
```

---

## Testing Your Theme

### 1. Create Test HTML

```html
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="darkSyntax/themes/darkSyntax-yourtheme.css">
  <script src="darkSyntax/darkSyntax.js"></script>
</head>
<body>
  <h1>Testing My Theme</h1>
  
  <!-- Test all syntax classes -->
  <pre code="javascript">
// Comment test
const keyword = "string";
let x = 42;
x += 5;
function myFunction() {
  return true;
}
class MyClass {
  constructor() {
    console.log("builtin");
  }
}
  </pre>
</body>
</html>
```

### 2. Check All Classes

Make sure you can see distinct colors for:
- Comments
- Keywords (`const`, `function`, `class`)
- Strings (`"string"`)
- Functions (`myFunction`, `constructor`, `log`)
- Builtins (`console`)
- Numbers (`42`)
- Booleans (`true`)
- Classes (`MyClass`)
- Operators (`=`, `+=`)
- Variables (`x`)

### 3. Test in Different Languages

Try your theme with different language configs:
- Python (whitespace-sensitive)
- C++ (preprocessor directives)
- Rust (operators, lifetimes, macros)
- AWK (field references with $)
- Bash (dollar variables)

### 4. Check Browser Console

Look for warnings:
```
DarkSyntax: Missing CSS classes in theme: ds-operator
```

This means you forgot to define a required class.

---

## Theme Examples

### Minimal Dark Theme

```css
/* Wrapper and UI */
.ds-wrapper {
  background: #1e1e1e;
  border-radius: 4px;
}

.ds-titlebar {
  background: #252526;
  color: #858585;
  padding: 8px 12px;
}

pre[code] {
  background: #1e1e1e;
  color: #d4d4d4;
  padding: 16px;
  font-family: 'Consolas', monospace;
  font-size: 14px;
}

/* Syntax colors */
.ds-keyword { color: #569cd6; }
.ds-string { color: #ce9178; }
.ds-comment { color: #6a9955; font-style: italic; }
.ds-class { color: #4ec9b0; }
.ds-function { color: #dcdcaa; }
.ds-builtin { color: #4ec9b0; font-style: italic; }
.ds-number { color: #b5cea8; }
.ds-boolean { color: #569cd6; }
.ds-decorator { color: #c586c0; }
.ds-operator { color: #d4d4d4; }
.ds-variable { color: #9cdcfe; }
```

### Minimal Light Theme

```css
/* Wrapper and UI */
.ds-wrapper {
  background: #ffffff;
  border: 1px solid #e1e4e8;
  border-radius: 6px;
}

.ds-titlebar {
  background: #f6f8fa;
  color: #586069;
  padding: 8px 12px;
  border-bottom: 1px solid #e1e4e8;
}

pre[code] {
  background: #ffffff;
  color: #24292e;
  padding: 16px;
  font-family: 'Consolas', monospace;
  font-size: 14px;
}

/* Syntax colors */
.ds-keyword { color: #d73a49; }
.ds-string { color: #032f62; }
.ds-comment { color: #6a737d; font-style: italic; }
.ds-class { color: #6f42c1; }
.ds-function { color: #6f42c1; }
.ds-builtin { color: #005cc5; font-style: italic; }
.ds-number { color: #005cc5; }
.ds-boolean { color: #005cc5; }
.ds-decorator { color: #e36209; }
.ds-operator { color: #d73a49; }
.ds-variable { color: #24292e; }
```

---

## Advanced Techniques

### Using CSS Variables

```css
:root {
  --ds-bg: #1e1e1e;
  --ds-keyword: #569cd6;
  --ds-string: #ce9178;
  --ds-operator: #d4d4d4;
  /* ... more variables */
}

.ds-keyword { color: var(--ds-keyword); }
.ds-string { color: var(--ds-string); }
.ds-operator { color: var(--ds-operator); }
```

This makes it easier to create theme variants.

### Font Variations

```css
.ds-keyword {
  color: #f92672;
  font-weight: bold;  /* Make keywords bold */
}

.ds-builtin {
  color: #66d9ef;
  font-style: italic;  /* Make builtins italic */
}

.ds-comment {
  color: #75715e;
  font-style: italic;
  opacity: 0.7;  /* Make comments semi-transparent */
}
```

### Hover Effects

```css
pre[code] .ds-function:hover {
  text-decoration: underline;
  cursor: pointer;
}
```

---

## Tips & Best Practices

1. **Start from an existing theme**: Don't start from scratch
2. **Test with real code**: Syntax highlighting looks different with actual code
3. **Check contrast ratios**: Use tools like WebAIM's contrast checker
4. **Be consistent**: Similar tokens should use similar colors
5. **Don't use too many colors**: 7-9 distinct colors is plenty
6. **Test in different lighting**: What looks good at night might not work in daylight
7. **Consider your users**: Accessibility matters
8. **Operators matter for modern languages**: Rust, TypeScript, Swift benefit from operator highlighting

---

## Common Issues

### Colors Look Wrong
- Check contrast against background
- Ensure you're using hex colors correctly (#RRGGBB)
- Test in different browsers

### Missing Highlighting
- Check browser console for missing class warnings
- Ensure all 11 required classes are defined
- Verify CSS file is loading (Network tab)

### Classes Not Working
- Make sure selectors are correct: `.ds-keyword` not `.keyword`
- Check for typos in class names
- Ensure CSS file loads after darkSyntax.js

### Operators Not Showing
- Verify `.ds-operator` class is defined in your theme
- Check that the language config uses the operator class
- Modern languages (Rust, TypeScript) use operators more than older languages (C, Pascal)

---

*Last updated: 20251010*