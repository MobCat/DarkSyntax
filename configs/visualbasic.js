// darkSyntax/configs/visualbasic.js - Visual Basic .NET configuration
// =====================================================================
// Visual Basic .NET (2002)
// BASIC evolved - Object-oriented programming for the .NET Framework
//
// Configs
// =======================
// ALIASES: ['visualbasic', 'vbnet', 'vb']
// File extensions: .vb, .vbnet
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Announced by Microsoft on February 13, 2002
// - Released with Visual Studio .NET and .NET Framework 1.0
// - Complete rewrite of Visual Basic 6.0 (1998)
// - Controversial "not backward compatible" decision caused community split
// - Designed by Microsoft's Visual Basic .NET team led by Paul Vick
// - First BASIC language fully integrated with object-oriented framework
// - Introduced structured exception handling (Try/Catch) to BASIC family
// - Added full inheritance support (VB6 only had interface inheritance)
// - Version 7.0 (VS 2002), 7.1 (VS 2003), 8.0 (VS 2005) rapid evolution
// - VB 9.0 (2007) added LINQ - revolutionary query syntax integration
// - VB 10.0 (2010) added dynamic typing, auto-properties, collection initializers
// - VB 11.0 (2012) added async/await, iterators, caller info attributes
// - VB 14.0 (2015) reached feature parity with C# for most features
// - VB 15.0 (2017) - final major update, maintenance mode since 2020
// - Microsoft announced "VB.NET is stable" (no new features) in 2020
// - Remains supported in .NET 5, 6, 7, 8 but no longer evolving
//
// INFLUENCED
// ----------
// - QB64 (2007) - Modern QuickBASIC revival borrowed .NET-style syntax features
// - Small Basic (2008) - Microsoft's educational language simplified VB.NET concepts
// - PowerShell (2006) - Case-insensitive syntax and parameter binding influenced by VB
// - Xojo (2013) - Cross-platform BASIC adopted VB.NET's class structure
// - B4X suite (2010+) - Basic4Android/B4J borrowed VB.NET event handling model
// - IronPython (2006) - .NET integration patterns influenced by VB.NET/C# interop
//
// USED FOR
// --------
// - Windows desktop applications (WinForms, WPF)
// - ASP.NET web applications and services
// - Office automation and COM interop
// - Legacy Visual Basic 6 migration projects
// - Enterprise line-of-business applications
// - Database-driven applications (ADO.NET integration)
// - Excel/Access macro development (VBA compatibility)
// - Rapid application development (RAD) with drag-and-drop designers
// - Educational programming in schools (before Python took over)
// - Maintenance of existing VB.NET codebases
//
// KEY FEATURES
// ------------
// - Case-insensitive syntax (Developer-friendly)
// - Verbose, English-like keywords (readable by non-programmers)
// - Full .NET Framework integration
// - Object-oriented: classes, inheritance, interfaces, polymorphism
// - Structured exception handling (Try/Catch/Finally)
// - LINQ (Language Integrated Query) for data manipulation
// - XML literals - inline XML with IntelliSense
// - Late binding with Option Strict Off (dynamic typing)
// - Optional parameters and named arguments
// - Automatic memory management (garbage collection)
// - Properties with Get/Set blocks
// - Events with Handles and AddHandler
// - Operator overloading
// - Generics with constraints
// - Async/await for asynchronous programming
// - Extension methods
// - Anonymous types and lambda expressions
//
// CORE SYNTAX
// -----------
// Class definition:
//   Public Class Person
//       Public Property Name As String
//       Public Property Age As Integer
//   End Class
//
// Method definition:
//   Public Function Calculate(x As Integer, y As Integer) As Integer
//       Return x + y
//   End Function
//
// Control structures:
//   If condition Then
//       ' code
//   ElseIf otherCondition Then
//       ' code
//   Else
//       ' code
//   End If
//
// Loops:
//   For i As Integer = 1 To 10 Step 2
//       Console.WriteLine(i)
//   Next
//
//   For Each item In collection
//       ' process item
//   Next
//
// Exception handling:
//   Try
//       ' risky code
//   Catch ex As Exception
//       ' handle error
//   Finally
//       ' cleanup
//   End Try
//
// QUIRKS
// ------
// - **Case insensitivity**: DIM, Dim, dim, DiM all identical
//   * Controversial design - enables typos but aids accessibility
//   * IDE auto-corrects to PascalCase convention
// - **Explicit line continuation**: Underscore _ for multi-line statements
//   * For years required explicit continuation (removed in VB 10.0)
//   * Console.WriteLine("Hello " _ & "World")
// - **No braces**: Uses End statements instead of { }
//   * End If, End Sub, End Function, End Class, End While
//   * More verbose but arguably more readable for beginners
// - **Default properties without parameters prohibited**: Can't have parameterless default property
//   * Unlike VB6 which allowed it
//   * Default Properties must have at least one parameter
// - **Option Strict Off**: Allows implicit conversions and late binding
//   * "2" + 3 = 5 (string converts to number)
//   * obj.AnyMethodName() compiles without checking existence
//   * Horror for C# developers, convenience for VB6 migrants
// - **Logical operators don't short-circuit by default**: And/Or evaluate both sides
//   * Use AndAlso/OrElse for short-circuit evaluation
//   * If x <> Nothing And x.Name = "Bob" crashes if x is Nothing
//   * If x IsNot Nothing AndAlso x.Name = "Bob" safe
// - **Arrays are zero-based but declared with upper bound**: Dim arr(10) creates 11 elements
//   * Unlike C# where new int[10] creates 10 elements
//   * Confusing for developers from other languages
// - **Module vs Class confusion**: Modules are like static classes but different
//   * Members accessible without qualification
//   * Can't inherit from or instantiate modules
// - **Handles clause**: Methods can declare which events they handle
//   * Private Sub Button1_Click() Handles Button1.Click
//   * Compile-time event wiring vs runtime AddHandler
// - **XML literals**: Can embed XML directly in code
//   * Dim xml = <root><item>value</item></root>
//   * Powerful but bizarre feature rarely seen elsewhere
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Visual Basic .NET is not Visual Basic 7.0" - Microsoft announcement controversy (2002)
// - "Real programmers use C#; VB is for the suits" - Developer community joke
// - "I love Visual Basic, but Microsoft doesn't" - Community sentiment after 2020 announcement
// - "VB.NET: Where you can be lazy with Option Strict Off and nobody judges you" - Developer humor
// - "The death of VB.NET has been greatly exaggerated... for the last 15 years" - Ongoing maintenance mode
// - "If you can read it, it's probably VB. If you can't, it's probably C#." - On verbosity differences
//
// NOTES ON VISUAL BASIC .NET SYNTAX
// ---------------------------------
// - Case-insensitive keywords and identifiers
// - Comments start with single quote (')
// - REM comments for legacy compatibility
// - Strings use double quotes, "" for embedded quotes
// - Line continuation with underscore _ (VB 9 and earlier)
// - Implicit line continuation in VB 10+ (2010)
// - Type declaration with As keyword: Dim x As Integer
// - Assignment uses = operator (same as equality test in If)
// - Comparison operators: =, <>, <, >, <=, >=
// - Logical operators: And, Or, Not, Xor, AndAlso, OrElse
// - String concatenation with & operator (+ also works)
// - Block terminators: End If, End Sub, End Function, etc.
// - Attributes in angle brackets: <Serializable>
// - Preprocessor directives: #If, #Else, #End If, #Const
// - XML literals: Not highlighted (too complex for regex, rare usage)
// - Lambda syntax: Function(x) x * 2 or Sub(x) Console.WriteLine(x)


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('visualbasic', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /'.*$/gm,
      priority: 100
    },
    
    {
      class: 'comment',
      pattern: /\bREM\b.*$/gim,
      priority: 100
    },
    
    // PRIORITY 90: Strings (VB uses "" for escaping quotes)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|"")*"/g,
      priority: 90
    },
    
    // PRIORITY 85: XML-like content in expressions (treated as strings)
    // Matches angle bracket content that's NOT at line start (not an attribute)
    {
      class: 'string',
      pattern: /(?<!^|\s)<[A-Za-z_][A-Za-z0-9_:]*(?:\s+[A-Za-z_][A-Za-z0-9_]*\s*=\s*"[^"]*")*\s*\/?>/g,
      priority: 85
    },
    
    // PRIORITY 80: Attributes (angle brackets at line start only)
    {
      class: 'decorator',
      pattern: /^\s*<[A-Za-z_][A-Za-z0-9_]*(?:\([^)]*\))?>/gm,
      priority: 80
    },
    
    // PRIORITY 70: Preprocessor directives
    {
      class: 'decorator',
      pattern: /#[A-Za-z]+.*$/gm,
      priority: 70
    },
    
    // PRIORITY 65: Class/Module/Structure declarations
    {
      class: 'keyword',
      pattern: /\b(Class|Module|Namespace|Interface|Structure|Enum|End\s+Class|End\s+Module|End\s+Namespace|End\s+Interface|End\s+Structure|End\s+Enum)\b/gi,
      priority: 65
    },
    
    // PRIORITY 60: OOP keywords and modifiers
    {
      class: 'keyword',
      pattern: /\b(Public|Private|Protected|Friend|Shared|Static|ReadOnly|WriteOnly|Overridable|Overrides|MustOverride|NotOverridable|MustInherit|NotInheritable|Shadows|Inherits|Implements|New|Dim|Const|WithEvents|Handles|Of)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: Control flow keywords
    {
      class: 'keyword',
      pattern: /\b(If|Then|Else|ElseIf|End\s+If|Select|Case|End\s+Select|For|To|Step|Next|Each|In|While|End\s+While|Do|Loop|Until|Exit|Continue|Return|GoTo|On\s+Error|Resume|Throw)\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Procedure and function keywords
    {
      class: 'keyword',
      pattern: /\b(Sub|Function|Property|Get|Set|End\s+Sub|End\s+Function|End\s+Property|End\s+Get|End\s+Set|ByVal|ByRef|Optional|ParamArray|As|Async|Await|Iterator|Yield)\b/gi,
      priority: 50
    },
    
    // PRIORITY 45: Exception handling
    {
      class: 'keyword',
      pattern: /\b(Try|Catch|Finally|End\s+Try|When)\b/gi,
      priority: 45
    },
    
    // PRIORITY 40: Other keywords
    {
      class: 'keyword',
      pattern: /\b(Imports|Using|End\s+Using|With|End\s+With|AddHandler|RemoveHandler|RaiseEvent|Delegate|Event|Operator|DirectCast|TryCast|CType|GetType|TypeOf|Is|IsNot|Like|AddressOf|NameOf|Partial|Me|MyBase|MyClass)\b/gi,
      priority: 40
    },
    
    // PRIORITY 38: LINQ query keywords
    {
      class: 'keyword',
      pattern: /\b(From|Where|Select|Order\s+By|Group\s+By|Join|Let|Distinct|Skip|Take|Aggregate)\b/gi,
      priority: 38
    },
    
    // PRIORITY 35: Primitive data types
    {
      class: 'builtin',
      pattern: /\b(Boolean|Byte|SByte|Short|UShort|Integer|UInteger|Long|ULong|Single|Double|Decimal|Char|String|Date|Object|Variant)\b/gi,
      priority: 35
    },
    
    // PRIORITY 30: Common .NET types
    {
      class: 'builtin',
      pattern: /\b(Console|StringBuilder|List|Dictionary|ArrayList|Hashtable|Queue|Stack|DateTime|TimeSpan|Math|Exception|Task|File|Directory|Path|Regex|IEnumerable|ICollection)\b/gi,
      priority: 30
    },
    
    // PRIORITY 25: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(CStr|CInt|CLng|CDbl|CBool|CDate|Val|Len|Mid|Left|Right|Trim|LTrim|RTrim|UCase|LCase|InStr|Replace|Split|Join|Format|IsNumeric|IsDate|IsNothing|IsDBNull|Now|Today|Year|Month|Day|Hour|Minute|Second)\b/gi,
      priority: 25
    },
    
    // PRIORITY 20: Function and method calls
    {
      class: 'function',
      pattern: /\b([A-Za-z_][A-Za-z0-9_]*)\s*(?=\()/g,
      priority: 20
    },
    
    // PRIORITY 15: Numbers (all formats)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[E][+-]?\d+[FfDdLlSsUI]*/gi,
      priority: 15
    },
    
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[FfDdLlSsUI]*\b/g,
      priority: 15
    },
    
    {
      class: 'number',
      pattern: /&H[0-9A-F]+[LlSsUI]*/gi,
      priority: 15
    },
    
    {
      class: 'number',
      pattern: /&O[0-7]+[LlSsUI]*/gi,
      priority: 15
    },
    
    // PRIORITY 10: Boolean and null values
    {
      class: 'boolean',
      pattern: /\b(True|False|Nothing|Null|DBNull)\b/gi,
      priority: 10
    },
    
    // PRIORITY 5: Logical operators (word-based)
    {
      class: 'keyword',
      pattern: /\b(And|Or|Not|Xor|AndAlso|OrElse|Mod)\b/gi,
      priority: 5
    }
  ]
});