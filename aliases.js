// darkSyntax/aliases.js - Language alias mappings
// ================================================
// 
// This file defines which language names map to which config files.
// 
// FORMAT:
// -------
// Each array represents one language family:
// [canonical_name, alias1, alias2, ...]
//
// - 0th index (canonical_name): The actual config filename (without .js)
//   Example: 'javascript' means the file is darkSyntax/configs/javascript.js
//
// - Remaining indices: Aliases that users can use in their HTML
//   Example: 'js', 'mjs' mean users can write code="js" or code="mjs"
//
// LOGIC:
// ------
// js → javascript → javascript.js
// mjs → javascript → javascript.js
// javascript → javascript → javascript.js
//
// Languages without aliases just have one item: ['go']
//
// ADDING NEW LANGUAGES:
// ---------------------
// 1. Create your config file: darkSyntax/configs/mylang.js
// 2. Add an entry here: ['mylang', 'ml', 'myml']
// 3. Users can now use: code="mylang" or code="ml" or code="myml"
//
// ================================================

darkSyntax.initializeAliases([
  // 0th index is the canonical name (config filename)
  // Rest are aliases users can type
  
  ['a0', 'univac1'],
  ['ada', 'adb', 'ads'],
  ['algol60', 'algol', 'alg'],
  ['assembly', 'asm', 's', 'nasm', 'masm'],
  ['autocode', 'manchester-autocode', 'auto'],
  ['awk', 'gawk', 'nawk', 'mawk'],
  ['bash', 'sh', 'shell', 'zsh'],
  ['basic', 'bas'],
  ['booth-arc', 'arc', 'contracted-notation'],
  ['brainfuck', 'bf', 'b'],
  ['c', 'h'],
  ['cobol', 'cbl', 'cob'],
  ['commodore64-basic', 'c64-basic', 'cbm-basic'],
  ['cpp', 'c++', 'cc', 'cxx', 'hpp', 'hxx'],
  ['csharp', 'cs', 'c#'],
  ['css'],
  ['erlang', 'erl', 'hrl'],
  ['flowmatic', 'flow-matic', 'b0'],
  ['forth', '4th', 'fth'],
  ['fortran', 'f90', 'f95', 'f03', 'f08', 'f', 'for'],
  ['fsharp', 'fs', 'f#', 'fsi', 'fsx'],
  ['gcode', 'nc', 'cnc', 'ngc', 'g'],
  ['golang', 'go'],
  ['haskell', 'hs'],
  ['holyc', 'hc', 'hh'],
  ['html', 'htm', 'xhtml'],
  ['intercal', 'i'],
  ['java'],
  ['javascript', 'js', 'mjs', 'cjs', 'jsx'],
  ['json', 'jsonc'],
  ['kotlin', 'kt', 'kts'],
  ['lisp', 'cl', 'el', 'l', 'lsp'],
  ['lua'],
  ['perl', 'pl', 'pm'],
  ['plm', 'plm80', 'plm86'],
  ['makefile', 'make', 'mk'],
  ['markdown', 'md', 'mdown', 'mkd'],
  ['matlab', 'm'],
  ['mobs-16', 'mobs', 'mobs16', 'm16'],
  ['objc', 'objectivec', 'obj-c', 'm', 'mm'],
  ['p-double-prime', 'p′′', 'p-prime-prime', 'pp', 'p2'],
  ['pascal', 'pas', 'pp'],
  ['php', 'php3', 'php4', 'php5'],
  ['powershell', 'ps1', 'psm1', 'psd1'],
  ['python', 'py', 'pyw', 'pyi'],
  ['quakec', 'qc'],
  ['r', 'rlang', 'rscript'],
  ['ruby', 'rb', 'rbw'],
  ['rust', 'rs'],
  ['scala', 'sc'],
  ['scheme', 'scm', 'ss', 'rkt', 'racket'],
  ['smalltalk', 'st', 'sm'],
  ['speedcoding', 'speedcode', 'speedco'],
  ['sql', 'mysql', 'postgresql', 'sqlite'],
  ['superplan', 'super'],
  ['swift'],
  ['typescript', 'ts', 'tsx'],
  ['unlambda', 'ul', 'unl'],
  ['visualbasic', 'vbnet', 'vbs', 'vb'],
  ['xml', 'xsd', 'xsl', 'xslt', 'svg', 'rss', 'atom', 'plist'],
  ['yaml', 'yml'],
]);