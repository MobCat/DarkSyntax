// darkSyntax/configs/powershell.js - PowerShell language configuration
// ======================================================================
// PowerShell (2006)
// PowerShell - Task automation and configuration management framework
//
// Configs
// =======================
// ALIASES: ['powershell', 'ps1', 'posh', 'pwsh']
// File extensions: .ps1, .psm1, .psd1
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Jeffrey Snover at Microsoft (2002-2006)
// - Originally codenamed "Monad" (2002-2003)
// - Renamed to Windows PowerShell (2006)
// - PowerShell 1.0 released (November 14, 2006)
// - Built on .NET Framework for Windows automation
// - PowerShell 2.0 with remoting and ISE (October 2009)
// - PowerShell 3.0 integrated into Windows 8 (September 2012)
// - PowerShell 4.0 with Desired State Configuration (October 2013)
// - PowerShell 5.0 with classes and package management (February 2016)
// - PowerShell 5.1 last Windows-only version (August 2016)
// - Open-sourced as PowerShell Core 6.0 (January 10, 2018)
// - Cross-platform support: Windows, Linux, macOS
// - PowerShell 7.0 on .NET Core 3.1 (March 4, 2020)
// - PowerShell 7.4 LTS latest stable (November 2023)
// - Jeffrey Snover's "Monad Manifesto" defined vision (2002)
// - Replaced cmd.exe and VBScript for Windows administration
// - Object-based pipeline vs text streams (revolutionary)
// - Verb-Noun cmdlet naming convention (design philosophy)
// - Azure Cloud Shell runs PowerShell by default
// - Windows Server management relies on PowerShell
// - Active Directory administration via PowerShell
// - DevOps and automation standard on Windows
// - GitHub hosts PowerShell as open source project
// - PowerShell Gallery for community modules
//
// INFLUENCED
// ----------
// - Unix shells (1970s+) - Shell concept, pipes, scripting
// - Bash (1989) - Command-line interface inspiration
// - Perl (1987) - Text processing and regex support
// - Python (1991) - Object-oriented scripting
// - C# (2000) - .NET integration, syntax elements
// - VBScript (1996) - Windows scripting predecessor
// - Windows Script Host (1998) - Automation framework predecessor
// - Ruby (1995) - Object pipeline concepts
// - Tcl (1988) - Embedded scripting language model
//
// USED FOR
// --------
// - Windows system administration and automation
// - Active Directory management and queries
// - Azure cloud resource management
// - Microsoft 365 / Office 365 administration
// - Exchange Server management
// - SQL Server administration and queries
// - DevOps automation and CI/CD pipelines
// - Configuration management (DSC)
// - Security and compliance automation
// - Network administration tasks
// - File system operations and bulk processing
// - Registry manipulation
// - Service and process management
// - Remote system management (PSRemoting)
// - Scheduled task automation
// - Log analysis and reporting
// - Web scraping and API interactions
// - IT infrastructure monitoring
//
// KEY FEATURES
// ------------
// - Object-based pipeline (passes .NET objects, not text)
// - Verb-Noun cmdlet naming convention (Get-Process, Set-Location)
// - Rich type system via .NET Framework/Core
// - Integrated help system (Get-Help cmdlet)
// - Tab completion and IntelliSense
// - Pipeline variable binding by name and type
// - Remote execution (Enter-PSSession, Invoke-Command)
// - Desired State Configuration (DSC) for infrastructure
// - Module system for code organization
// - Script blocks and closures
// - Error handling with Try/Catch/Finally
// - Built-in cmdlets for common tasks (700+ in core)
// - Aliases for Unix/CMD compatibility (ls, cd, dir)
// - Here-strings for multi-line text
// - Splatting for parameter organization
// - Advanced functions with parameter validation
// - PowerShell Remoting over WinRM/SSH
// - Background jobs and workflows
// - Provider system (filesystem, registry, certificate store)
// - Classes and enums (PowerShell 5.0+)
//
// CORE SYNTAX
// -----------
// Basic cmdlet usage:
//   Get-Process
//   Get-ChildItem -Path C:\
//   Set-Location -Path "C:\Users"
//
// Variables:
//   $name = "Alice"
//   $numbers = 1..10
//   $process = Get-Process -Name "notepad"
//
// Pipeline:
//   Get-Process | Where-Object {$_.CPU -gt 10} | Sort-Object CPU -Descending
//   Get-ChildItem *.txt | Select-Object Name, Length
//
// Functions:
//   function Get-Greeting {
//       param([string]$Name)
//       return "Hello, $Name!"
//   }
//
// Conditionals:
//   if ($x -gt 10) {
//       Write-Host "Greater than 10"
//   }
//
// QUIRKS
// ------
// - **Case-insensitive**: Get-Process = get-process = GET-PROCESS
//   * Cmdlets, variables, keywords all case-insensitive
//   * Convention: PascalCase for cmdlets
//   * String comparison case-insensitive by default
//
// - **Verb-Noun naming**: Strict cmdlet convention
//   * Approved verbs: Get, Set, New, Remove, Add, etc.
//   * New-Item not Create-Item
//   * Remove-Item not Delete-Item
//   * Get-ApprovedVerb shows all approved verbs
//
// - **Object pipeline**: Not text streams
//   * Pipes pass .NET objects with properties/methods
//   * Get-Process | Format-Table Name, CPU
//   * Can access object properties directly
//   * Revolutionary vs Unix text-based pipes
//
// - **$ prefix for variables**: Always required
//   * $variable not variable
//   * ${complex-name} for spaces/special chars
//   * $global:, $script:, $local: scopes
//
// - **Comparison operators**: Use - prefix
//   * -eq not == (equal)
//   * -ne not != (not equal)
//   * -gt not > (greater than)
//   * -lt not < (less than)
//   * -like, -match, -contains for patterns
//
// - **Parameters use dash**: -ParameterName
//   * Get-Process -Name "notepad"
//   * Can abbreviate: -Name or -N (if unique)
//   * Switch parameters: -Recurse, -Force
//
// - **Return is optional**: Last value returned
//   * function Add($a, $b) { $a + $b }
//   * No need for explicit return
//   * Return exits function early
//
// - **Arrays are flexible**: Dynamic sizing
//   * $arr = @()  # Empty array
//   * $arr = 1, 2, 3  # No brackets needed
//   * $arr += 4  # Append (creates new array)
//   * @(...) forces array context
//
// - **Hash tables**: @{} syntax
//   * $hash = @{Name="Alice"; Age=30}
//   * $hash.Name or $hash["Name"]
//   * [ordered] for ordered dictionaries
//
// - **Backtick escaping**: Not backslash
//   * `n for newline (not \n)
//   * `t for tab (not \t)
//   * `` for literal backtick
//   * Line continuation: `
//
// - **Here-strings**: Multi-line strings
//   * @"..."@ expandable (variables work)
//   * @'...'@ literal (no expansion)
//   * First delimiter must be last char on line
//
// - **Automatic variables**: $_ and more
//   * $_ current pipeline object
//   * $PSItem same as $_
//   * $args function arguments
//   * $true, $false, $null
//   * $? last command success
//
// - **Cmdlet vs Function**: Compiled vs script
//   * Cmdlets are compiled .NET classes
//   * Functions are PowerShell script
//   * Both use Verb-Noun naming
//   * Advanced functions act like cmdlets
//
// - **Splatting**: @ instead of $
//   * $params = @{Name="test"; Path="C:\"}
//   * Get-Item @params
//   * Passes hash as individual parameters
//
// - **String expansion**: Double quotes only
//   * "Hello $name" expands variables
//   * 'Hello $name' literal (no expansion)
//   * "Process: $($process.Name)" for expressions
//
// - **Execution policy**: Security feature
//   * Restricted: No scripts
//   * AllSigned: Only signed scripts
//   * RemoteSigned: Downloaded need signature
//   * Unrestricted: All scripts run
//   * Set-ExecutionPolicy to change
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Monad: An object-based shell for Windows" - Jeffrey Snover's manifesto (2002)
// - "PowerShell is the language of Windows administration" - Industry recognition
// - "The pipeline is the heart of PowerShell" - Core design philosophy
// - "PowerShell: Because clicking is for cavemen" - Admin humor
// - "I automate what I can, so I can focus on what I can't" - PowerShell user motto
// - "Get-Help is your friend" - First thing taught to new users
// - "Verb-Noun is not just convention, it's religion" - PowerShell community
// - "PowerShell makes the easy things easy and the hard things possible" - Design goal
// - "PowerShell: Where Unix meets .NET" - Cross-platform era
// - "Once you go PowerShell, you never go back to cmd" - Windows admin experience
//
// NOTES ON POWERSHELL SYNTAX
// ---------------------------
// - Case-insensitive language (Get-Process = get-process)
// - Comments: # (single-line), <# ... #> (multi-line)
// - Variables start with $: $variable
// - Cmdlets use Verb-Noun: Get-Process, Set-Location
// - Parameters prefixed with dash: -Name, -Path
// - Comparison operators: -eq, -ne, -gt, -lt, -ge, -le
// - Pattern matching: -like, -match, -contains
// - Logical operators: -and, -or, -not, -xor
// - Pipeline operator: | (passes objects)
// - String concatenation: + operator or "$var1 $var2"
// - Arrays: @(), 1,2,3, or 1..10 (range)
// - Hash tables: @{Key="Value"}
// - Here-strings: @"..."@ (expandable), @'...'@ (literal)
// - Backtick escape: `n, `t, `` (line continuation)
// - Automatic variables: $_, $PSItem, $true, $false, $null
// - Scope prefixes: $global:, $script:, $local:
// - .NET types: [string], [int], [System.IO.File]
// - Method calls: $object.Method()
// - Property access: $object.Property
// - Splatting: @params for parameter hash
// - Script blocks: { code }
// - Functions: function Name { }
// - Classes: class Name { } (PS 5.0+)
// - Try/Catch/Finally for error handling
// - Switch statement for multi-way branching
// - ForEach-Object for pipeline iteration
// - Where-Object for filtering
// - Select-Object for property selection


// PowerShell Syntax Highlighting Configuration for DarkSyntax
// ============================================================
darkSyntax.registerLanguage('powershell', {
  rules: [
    // PRIORITY 100: Comments (hash style)
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 100: Multi-line comments (block comments)
    {
      class: 'comment',
      pattern: /<#[\s\S]*?#>/g,
      priority: 100
    },
    
    // PRIORITY 95: Here-strings (multi-line strings)
    {
      class: 'string',
      pattern: /@["'][\s\S]*?["']@/g,
      priority: 95
    },
    
    // PRIORITY 90: Strings (double quotes - expandable, no newlines)
    {
      class: 'string',
      pattern: /"(?:[^"\n\\`]|`[^"]|\\.)*/g,
      priority: 90
    },
    
    // PRIORITY 90: Strings (single quotes - literal, no newlines)
    {
      class: 'string',
      pattern: /'(?:[^'\n\\]|\\.)*'/g,
      priority: 90
    },
    
    // PRIORITY 85: Boolean automatic variables
    {
      class: 'boolean',
      pattern: /\$(?:true|false|null)\b/gi,
      priority: 85
    },
    
    // PRIORITY 84: Special automatic variables (current object, etc.)
    {
      class: 'variable',
      pattern: /\$(?:_|PSItem|args|input|PSScriptRoot|PSCommandPath|HOME|PID|PWD|LASTEXITCODE|Host|Error|Matches)\b/g,
      priority: 84
    },
    
    // PRIORITY 80: Variables (general)
    {
      class: 'variable',
      pattern: /\$(?:[a-zA-Z_][a-zA-Z0-9_]*|{[^}]+})/g,
      priority: 80
    },
    
    // PRIORITY 70: Cmdlets and Functions (Verb-Noun pattern)
    {
      class: 'function',
      pattern: /\b([A-Z][a-z]+-[A-Z][a-zA-Z0-9]+)\b/g,
      priority: 70
    },
    
    // PRIORITY 65: Parameters (dash-prefixed)
    {
      class: 'decorator',
      pattern: /-[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 65
    },
    
    // PRIORITY 60: Comparison operators
    {
      class: 'keyword',
      pattern: /\B-(?:eq|ne|gt|ge|lt|le|like|notlike|match|notmatch|contains|notcontains|in|notin|replace|split|join|is|isnot|as|and|or|xor|not|band|bor|bxor|bnot|shl|shr)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: Keywords
    {
      class: 'keyword',
      pattern: /\b(begin|break|catch|class|continue|data|define|do|dynamicparam|else|elseif|end|enum|exit|filter|finally|for|foreach|from|function|if|in|param|process|return|switch|throw|trap|try|until|using|var|while|workflow)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Common cmdlet aliases
    {
      class: 'builtin',
      pattern: /\b(cd|ls|dir|pwd|cat|cp|mv|rm|rmdir|echo|cls|clear|type|more|sort|select|where|foreach|fl|ft|gci|gcm|gal|iex|sls|measure|group|tee)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: .NET types and type accelerators
    {
      class: 'class',
      pattern: /\[[a-zA-Z_][a-zA-Z0-9_.]*\]/g,
      priority: 45
    },
    
    // PRIORITY 40: Method calls and properties
    {
      class: 'function',
      pattern: /\.([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 40
    },
    
    // PRIORITY 30: Numbers (including hex)
    {
      class: 'number',
      pattern: /\b0x[0-9A-Fa-f]+\b/g,
      priority: 30
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*(?:[eE][+-]?\d+)?[dDlLfF]?\b/g,
      priority: 25
    }
  ]
});