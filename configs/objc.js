// darkSyntax/configs/objc.js - Objective-C language configuration
// =================================================================
// Objective-C (1984)
// Objective-C - Object-oriented extension of C
//
// Configs
// =======================
// ALIASES: ['objc', 'objectivec', 'obj-c', 'm', 'mm']
// File extensions: .m (implementation), .mm (C++), .h (header)
//
// OBJECTIVE-C SYNTAX NOTES
// ========================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// Objective-C (1984):
// - Created by Brad Cox and Tom Love at their company Stepstone
// - Combined C with Smalltalk's object-oriented messaging
// - Licensed by NeXT Computer (Steve Jobs' company) in 1988
// - Became primary language for NeXTSTEP operating system
// - Apple acquired NeXT in 1997, made Objective-C the macOS/iOS language
// - Dominant Apple platform language until Swift (2014)
// - Still used in legacy macOS/iOS codebases
//
// INFLUENCED
// ----------
// - Swift (2014) - Apple's modern replacement, inspired by Objective-C lessons
// - Java (1995) - Borrowed some OOP concepts
// - Groovy (2003) - Dynamic features similar to Objective-C
// - C# properties inspired by Objective-C @property
//
// USED FOR
// --------
// - macOS applications (1997-2014 primary language)
// - iOS applications (2007-2014 primary language)
// - NeXTSTEP operating system
// - Legacy Apple codebases (still maintained)
// - GNUstep (open source NeXTSTEP implementation)
// - Foundation framework, UIKit, AppKit
//
// KEY FEATURES
// ------------
// - Smalltalk-style message passing: [object method:argument]
// - Dynamic runtime (objects resolved at runtime, not compile time)
// - Categories (add methods to existing classes)
// - Protocols (similar to interfaces)
// - Properties with automatic getters/setters
// - Automatic Reference Counting (ARC) - added in 2011
// - Blocks (closures) - added in 2009
// - @-prefixed keywords and literals
// - Retains all C features (it's a true superset of C)
//
// CORE SYNTAX
// -----------
// Interface (header):
//   @interface MyClass : NSObject
//   @property NSString *name;
//   - (void)instanceMethod:(int)param;
//   + (void)classMethod;
//   @end
//
// Implementation:
//   @implementation MyClass
//   - (void)instanceMethod:(int)param {
//       NSLog(@"Value: %d", param);
//   }
//   @end
//
// Message Passing:
//   [object method];
//   [object method:arg];
//   [object method:arg1 withParam:arg2];
//   [[MyClass alloc] init];  // Nested messages
//
// Properties:
//   @property (nonatomic, strong) NSString *name;
//   @synthesize name;  // Auto-generate getter/setter
//
// Protocols:
//   @protocol MyProtocol
//   - (void)requiredMethod;
//   @optional
//   - (void)optionalMethod;
//   @end
//
// Categories:
//   @interface NSString (MyAdditions)
//   - (BOOL)containsString:(NSString *)str;
//   @end
//
// Literals:
//   @"string"     // NSString
//   @42           // NSNumber from int
//   @3.14         // NSNumber from double
//   @YES, @NO     // NSNumber from BOOL
//   @[]           // NSArray
//   @{}           // NSDictionary
//
// QUIRKS
// ------
// - Method names can be very long: [self calculateTotal:items withTax:YES]
// - No method overloading (method name includes parameter names)
// - Requires manual retain/release (pre-ARC) or ARC (post-2011)
// - Brackets everywhere can be confusing
// - Hybrid of C's static typing and Smalltalk's dynamic typing
// - `id` type represents any object
// - `nil` is the null object (not NULL)
// - Header files (.h) separate from implementation (.m)
// - .mm files allow C++ code mixed with Objective-C


// OBJECTIVE-C SYNTAX CONFIGURATION FOR DARKSYNTAX
// ================================================
darkSyntax.registerLanguage('objc', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 95: Objective-C string literals (@"...")
    {
      class: 'string',
      pattern: /@"(?:[^"\\]|\\.)*"/g,
      priority: 95
    },
    
    // PRIORITY 90: Regular C strings
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
    
    // PRIORITY 85: @-keywords (interface, implementation, protocol, etc.)
    {
      class: 'keyword',
      pattern: /@(interface|implementation|protocol|end|property|synthesize|dynamic|class|selector|encode|synchronized|autoreleasepool|available|optional|required|public|private|protected|package|try|catch|finally|throw)\b/g,
      priority: 85
    },
    
    // PRIORITY 80: Preprocessor directives (#import, #include, #define)
    {
      class: 'decorator',
      pattern: /#\s*(import|include|define|ifdef|ifndef|endif|if|elif|else|pragma|undef|error|warning)\b/g,
      priority: 80
    },
    
    // PRIORITY 75: C/Objective-C keywords
    {
      class: 'keyword',
      pattern: /\b(auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|inline|int|long|register|restrict|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while|_Bool|_Complex|_Imaginary|id|instancetype|Class|SEL|IMP|BOOL|nil|Nil|YES|NO|self|super|in)\b/g,
      priority: 75
    },
    
    // PRIORITY 70: Objective-C literals (@YES, @NO, @42, etc.)
    {
      class: 'boolean',
      pattern: /@(YES|NO|true|false)\b/g,
      priority: 70
    },
    {
      class: 'number',
      pattern: /@\d+\.?\d*/g,
      priority: 70
    },
    
    // PRIORITY 65: Foundation classes and common types
    {
      class: 'class',
      pattern: /\b(NS[A-Z][a-zA-Z0-9]*|UI[A-Z][a-zA-Z0-9]*|CG[A-Z][a-zA-Z0-9]*)\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Built-in C library functions
    {
      class: 'builtin',
      pattern: /\b(printf|scanf|malloc|free|memcpy|strlen|strcpy|strcmp|NULL|EOF|size_t|dispatch_once|dispatch_async|dispatch_sync)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Method definitions (- or + followed by return type and method name)
    {
      class: 'function',
      pattern: /[-+]\s*\([^)]+\)\s*([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 55
    },
    
    // PRIORITY 50: Message sends ([object method] or [object method:arg])
    // Matches the method name inside square brackets
    {
      class: 'function',
      pattern: /\[\s*[a-zA-Z_][a-zA-Z0-9_]*\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 50
    },
    
    // PRIORITY 45: Function calls (C-style)
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 45
    },
    
    // PRIORITY 40: Property attributes
    {
      class: 'decorator',
      pattern: /\b(nonatomic|atomic|strong|weak|copy|assign|readonly|readwrite|getter|setter|unsafe_unretained|nonnull|nullable|null_resettable|null_unspecified)\b/g,
      priority: 40
    },
    
    // PRIORITY 30: Numbers (hex, decimal, float)
    {
      class: 'number',
      pattern: /\b0[xX][0-9A-Fa-f]+[ULul]?\b/g,
      priority: 30
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?[fFlLuU]?\b/g,
      priority: 25
    },
    
    // PRIORITY 20: NULL, nil, Nil
    {
      class: 'boolean',
      pattern: /\b(NULL|nil|Nil)\b/g,
      priority: 20
    }
  ]
});
