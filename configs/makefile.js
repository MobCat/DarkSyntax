// darkSyntax/configs/makefile.js - Makefile configuration
// =========================================================
// Make / Makefile (1976)
// Build automation tool for compiling and managing project dependencies
//
// Configs
// =======================
// ALIASES: ['makefile', 'make', 'mk']
// File extensions: Makefile, makefile, .mk, GNUmakefile
//
// Makefile - Standard filename (capital M)
// makefile - Alternative lowercase
// GNUmakefile - GNU Make specific features
// .mk - Make include files
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Stuart Feldman at Bell Labs (1976) - Part of Unix development
// - Published in Unix v7 (1979) - Became standard build tool
// - GNU Make released (1988) - Richard Stallman, added many extensions
// - Influenced all modern build systems (Ant, Maven, Gradle, CMake, Ninja)
// - Still ubiquitous in C/C++ projects 50 years later
// - Turing Award mentioned (2003) - Feldman's contributions to software tools
// - POSIX standardized (1992) - Portable make specification
//
// INFLUENCED
// ----------
// - Ant (2000) - XML-based build for Java (declarative like Make)
// - Rake (2003) - Ruby build tool with Make-like syntax
// - SCons (2001) - Python-based build system
// - CMake (2000) - Cross-platform makefile generator
// - Gradle (2007) - Groovy-based build, task dependencies
// - Ninja (2010) - Faster Make alternative for large projects
// - npm scripts (2010) - JavaScript task runner, dependency-based
//
// USED FOR
// --------
// - C/C++ project compilation and linking
// - Linux kernel builds (largest Makefile in existence)
// - Embedded systems cross-compilation
// - Automatic code generation and preprocessing
// - Installing software (make install)
// - Running test suites
// - Documentation generation
// - General task automation
//
// KEY FEATURES
// ------------
// - Dependency-based execution (only rebuild what changed)
// - Pattern rules for generic transformations
// - Automatic variables ($@, $<, $^, etc.)
// - Variables and functions
// - Conditional compilation
// - Parallel execution (make -j)
// - Phony targets for non-file tasks
// - Implicit rules for common operations
//
// CORE SYNTAX
// -----------
// - Targets: target: dependencies
// - Recipes: TAB-indented commands (MUST be TAB, not spaces!)
// - Variables: VAR = value, $(VAR) or ${VAR} to use
// - Automatic variables: $@ (target), $< (first prereq), $^ (all prereqs)
// - Pattern rules: %.o: %.c
// - Comments: # character
// - Conditionals: ifdef, ifndef, ifeq, ifneq
// - Include: include other.mk
// - Functions: $(wildcard *.c), $(patsubst %.c,%.o,$(SOURCES))
//
// QUIRKS
// ------
// - **TABS are mandatory for recipes**:
//   * Recipes MUST be indented with TAB character, not spaces
//   * Using spaces causes cryptic error: "missing separator"
//   * One of the most hated features in programming
//   * Historical artifact from 1976 terminal conventions
// - **$ needs escaping for literal $**:
//   * $$ in Makefile becomes single $ in shell
//   * $(VAR) expands Make variable
//   * $$VAR passes $VAR to shell (shell variable)
// - **Recipes run in separate shells**:
//   * Each line runs in new shell invocation
//   * cd in one line doesn't affect next line
//   * Use ; and \ to chain commands: cd dir; ./build
//   * Or: .ONESHELL: directive (GNU Make 3.82+)
// - **Whitespace in variable assignments matters**:
//   * VAR = value (trailing space included!)
//   * VAR= value (no trailing space)
//   * VAR =value (leading space included!)
//   * VAR:=value (immediate expansion, no space)
// - **= vs := vs ?= vs +=**:
//   * = recursive expansion (evaluated when used)
//   * := simple expansion (evaluated immediately)
//   * ?= conditional assignment (only if not set)
//   * += append to variable
// - **@ suppresses command echo**:
//   * Normal: make prints command then runs it
//   * @command: runs without printing
//   * @ prefix commonly used for echo statements
// - **- ignores errors**:
//   * -command: continue even if command fails
//   * Useful for optional cleanup: -rm *.o
// - **Phony targets must be declared**:
//   * .PHONY: clean all install
//   * Tells Make these aren't actual files
//   * Without it, file named "clean" would break make clean
// - **Order-only prerequisites**:
//   * target: normal-prereqs | order-only-prereqs
//   * Order-only prereqs must exist but timestamp doesn't matter
//   * Used for directory creation
// - **Pattern rules use %**:
//   * %.o: %.c means any .o depends on corresponding .c
//   * $< and $@ used to reference matched parts
// - **Implicit rules exist for common tasks**:
//   * Make knows how to compile .c to .o automatically
//   * Can be confusing when they trigger unexpectedly
//   * make -r disables implicit rules
// - **Backslash continues lines**:
//   * Long lines split with \
//   * Must be at end of line, no trailing space!
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Why the #$@&% does Make use tabs?" - Every developer's first question
// - "Recursive Make Considered Harmful" - Famous 1997 paper by Peter Miller
// - "Make is to programming what assembly language is to high-level languages" - Build system evolution
// - "It's not a bug, it's a 50-year-old feature" - TAB requirement defense
//
// NOTES ON MAKEFILE SYNTAX
// -------------------------
// - Comments use # character
// - Targets followed by colon: target:
// - Dependencies after colon: target: dep1 dep2
// - Recipes must be TAB-indented (not spaces!)
// - Variables: $(VAR) or ${VAR}
// - Automatic variables: $@, $<, $^, $?, $*, $+, $|
// - Functions: $(function args)
// - Conditionals: ifdef, ifndef, ifeq, ifneq, else, endif
// - Special targets start with .: .PHONY, .SUFFIXES, .DEFAULT


// MAKEFILE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('makefile', {
  rules: [
    // Strings (double quotes) in recipes and assignments
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"/g,
      priority: 90
    },

    // Strings (single quotes) in recipes and assignments
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },

    // Comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },

    // Directives (include, ifdef, ifndef, ifeq, ifneq, else, endif, etc.)
    {
      class: 'decorator',
      pattern: /^[ \t]*(include|sinclude|-include|override|export|unexport|ifdef|ifndef|ifeq|ifneq|else|endif|define|endef)\b/gm,
      priority: 75
    },

    // Special targets (.PHONY, .SUFFIXES, .DEFAULT, etc.)
    {
      class: 'decorator',
      pattern: /^\.(PHONY|SUFFIXES|DEFAULT|PRECIOUS|INTERMEDIATE|SECONDARY|DELETE_ON_ERROR|IGNORE|LOW_RESOLUTION_TIME|SILENT|EXPORT_ALL_VARIABLES|NOTPARALLEL|ONESHELL|POSIX)\b/gm,
      priority: 74
    },

    // Target definitions (target: dependencies)
    {
      class: 'function',
      pattern: /^([a-zA-Z0-9_\-\.\/]+)(?=\s*:(?!=))/gm,
      priority: 60
    },

    // Pattern rules (%.o: %.c)
    {
      class: 'function',
      pattern: /^(%\.[a-zA-Z0-9_]+)(?=\s*:)/gm,
      priority: 61
    },

    // Variable assignments (VAR = value, VAR := value, VAR ?= value, VAR += value)
    {
      class: 'variable',
      pattern: /^([A-Z_][A-Z0-9_]*)\s*[:?+]?=/gm,
      captureGroup: 1,
      priority: 55
    },

    // Variable references $(VAR) or ${VAR}
    {
      class: 'variable',
      pattern: /\$[\(\{]([A-Za-z_][A-Za-z0-9_]*)[\)\}]/g,
      captureGroup: 1,
      priority: 50
    },

    // Automatic variables ($@, $<, $^, $?, $*, $+, $|, etc.)
    {
      class: 'variable',
      pattern: /\$[@<^?*+|%]/g,
      priority: 51
    },

    // Shell builtins and common commands in recipes
    {
      class: 'builtin',
      pattern: /\b(echo|cd|mkdir|rm|cp|mv|cat|grep|sed|awk|touch|chmod|chown|install|test|printf|export|source)\b/g,
      priority: 46
    },

    // Functions $(wildcard ...), $(patsubst ...), etc.
    {
      class: 'builtin',
      pattern: /\$[\(\{](subst|patsubst|strip|findstring|filter|filter-out|sort|word|wordlist|words|firstword|lastword|dir|notdir|suffix|basename|addsuffix|addprefix|join|wildcard|realpath|abspath|error|warning|info|shell|foreach|if|or|and|call|eval|file|value)[\s,]/g,
      captureGroup: 1,
      priority: 52
    },

    // Assignment operators
    {
      class: 'operator',
      pattern: /[:?+]?=/g,
      priority: 58
    },

    // Recipe prefix characters (@, -, +)
    {
      class: 'operator',
      pattern: /^[\t][@\-+]/gm,
      priority: 57
    }
  ]
});