// darkSyntax/configs/bash.js - Bash/Shell language configuration
// ===============================================================
// Bash (1989) / sh (1977)
// sh (Bourne Shell) - 1977 by Stephen Bourne at Bell Labs
// Bash (Bourne Again Shell) - 1989 by Brian Fox for GNU Project
//
// Configs
// =======================
// ALIASES: ['bash', 'sh', 'shell', 'zsh']
// File extensions: .sh, .bash, .zsh
//
// BASH/SHELL SYNTAX NOTES
// ========================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// sh (Bourne Shell) - 1977:
// - Created by Stephen Bourne at Bell Labs (AT&T)
// - Original Unix shell, replaced Thompson shell
// - Became POSIX standard shell
// - Influenced: ksh, bash, zsh, all modern shells
//
// Bash (Bourne Again Shell) - 1989:
// - Created by Brian Fox for GNU Project
// - Free/open source replacement for sh
// - Superset of sh with many enhancements
// - Default shell on Linux, macOS (until macOS Catalina)
// - Most widely used shell today
//
// INFLUENCED
// ----------
// - ksh (Korn Shell, 1983) - Combined sh and csh features
// - zsh (Z Shell, 1990) - Extended bash with more features
// - PowerShell (2006) - Windows shell, object-based
// - Fish (2005) - Modern shell with better defaults
//
// USED FOR
// --------
// - System administration and automation
// - DevOps and CI/CD pipelines
// - Build systems and scripts
// - Command-line utilities
// - Server configuration and deployment
// - Every Unix/Linux system
//
// SH VS BASH DIFFERENCES
// ----------------------
// sh (POSIX):
// - test with [ ]
// - Command substitution with `backticks`
// - Limited string manipulation
// - No arrays
//
// Bash enhancements:
// - Extended test with [[ ]]
// - $() command substitution (nested!)
// - Arrays: array=(one two three)
// - ${var//pattern/replacement}
// - Process substitution: <(command)
// - [[  ]] with && and ||
//
// NOTES ON BASH SYNTAX
// ---------------------
// - Comments use #
// - Variables: $var or ${var}
// - Special variables: $0, $1, $@, $?, $$, $!
// - Functions: name() { ... }
// - Control flow: if/then/fi, for/do/done, while/do/done
// - Test conditions: [ ] or [[ ]] with -eq, -ne, -gt, -lt, etc.
// - Command substitution: $(command) or `command`
// - Pipes: |, Redirection: >, >>, <, 2>, &>
// - Here-documents: << EOF ... EOF
// - String manipulation: ${var:start:length}, ${var/pattern/replacement}
// - Arithmetic: $((expression))
//
// BASH SYNTAX CONFIGURATION FOR DARKSYNTAX
// =========================================
// Supports Bash, sh, and compatible shells

darkSyntax.registerLanguage('bash', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 95: Shebang (special comment)
    {
      class: 'decorator',
      pattern: /^#!.*$/gm,
      priority: 95
    },
    
    // PRIORITY 92: Variables inside strings (MUST be higher than strings!)
    {
      class: 'variable',
      pattern: /(?<=")(?:[^"$\\]|\\.)*?(\$\{[^}]+\}|\$[a-zA-Z_][a-zA-Z0-9_]*|\$[@*#?!$0-9_-])(?:[^"$\\]|\\.)*?(?=")/g,
      captureGroup: 1,
      priority: 92
    },
    
    // PRIORITY 91: Command substitution inside strings
    {
      class: 'string',
      pattern: /(?<=")(?:[^"$\\]|\\.)*?(\$\([^)]*\)|`[^`]*`)(?:[^"$\\]|\\.)*?(?=")/g,
      captureGroup: 1,
      priority: 91
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
    
    // PRIORITY 85: Here-documents and here-strings
    {
      class: 'string',
      pattern: /<<-?\s*['"]?(\w+)['"]?[\s\S]*?\n\1/g,
      priority: 85
    },
    
    // PRIORITY 80: Variables
    {
      class: 'variable',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)(?==)/g,
      priority: 80
    },
    
    {
      class: 'variable',
      pattern: /\$\{[^}]+\}|\$[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 80
    },
    
    {
      class: 'variable',
      pattern: /\$[@*#?!$0-9_-]/g,
      priority: 80
    },
    
    // PRIORITY 70: Functions
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(\)/g,
      captureGroup: 1,
      priority: 70
    },
    
    // PRIORITY 65: Control flow keywords
    {
      class: 'keyword',
      pattern: /\b(if|then|else|elif|fi|case|esac|for|while|until|do|done|in|select|time|function)\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Command keywords and builtins
    {
      class: 'keyword',
      pattern: /\b(return|exit|break|continue|shift|export|declare|local|readonly|unset|eval|exec|source|alias|unalias|set|trap)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Test commands and operators
    {
      class: 'builtin',
      pattern: /\b(test|\[|\[\[)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Common bash built-in commands
    {
      class: 'builtin',
      pattern: /\b(echo|printf|read|cd|pwd|pushd|popd|dirs|let|expr|basename|dirname|type|which|command|builtin|enable|help|logout|times|ulimit|umask|wait|jobs|fg|bg|kill|disown)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Common Unix/Linux commands
    {
      class: 'builtin',
      pattern: /\b(ls|cat|grep|awk|sed|find|xargs|sort|uniq|wc|head|tail|cut|paste|tr|tee|diff|patch|tar|gzip|gunzip|zip|unzip|curl|wget|ssh|scp|rsync|chmod|chown|chgrp|ln|mkdir|rmdir|rm|cp|mv|touch|file|stat|date)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Redirection and pipes
    {
      class: 'keyword',
      pattern: /[|&><]+|>>&?|<<-?|&>|&>>|>\||<&|>&/g,
      priority: 40
    },
    
    // PRIORITY 35: Command substitution
    {
      class: 'string',
      pattern: /\$\([^)]*\)/g,
      priority: 35
    },
    
    {
      class: 'string',
      pattern: /`[^`]*`/g,
      priority: 35
    },
    
    // PRIORITY 30: Options/flags (start with - or --)
    {
      class: 'decorator',
      pattern: /\s(-{1,2}[a-zA-Z0-9_-]+)\b/g,
      captureGroup: 1,
      priority: 30
    },
    
    // PRIORITY 25: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Boolean-like values and null
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Logical operators in test conditions
    {
      class: 'keyword',
      pattern: /(?<=\s)(-eq|-ne|-lt|-le|-gt|-ge|-z|-n|-e|-f|-d|-r|-w|-x|-s|-h|-L|-o|-a|!)(?=\s)/g,
      priority: 15
    }
  ]
});

