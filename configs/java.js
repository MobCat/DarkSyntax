// darkSyntax/configs/java.js - Java language configuration
// ==========================================================
// Java (1995)
// Java - Write once, run anywhere
//
// Configs
// =======================
// ALIASES: ['java']
// File extensions: .java
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by James Gosling at Sun Microsystems (1991-1995)
// - Originally named "Oak" after tree outside Gosling's office (1991)
// - Renamed to "Java" after coffee from Java, Indonesia (1995)
// - First public release: Java 1.0 (January 23, 1996)
// - Original goal: Interactive television software (too advanced for cable industry)
// - Pivot to World Wide Web applications (1994)
// - Java applets revolutionized web interactivity (mid-1990s)
// - "Write once, run anywhere" (WORA) philosophy via JVM
// - Java 2 (J2SE, J2EE, J2ME) platform editions (December 8, 1998)
// - Sun Microsystems acquired by Oracle Corporation (January 27, 2010)
// - Oracle vs Google lawsuit over Android's use of Java APIs (2010-2021)
// - Java became open source under GPL (November 13, 2006)
// - OpenJDK project launched as reference implementation (2007)
// - Major versions: Java 5 (generics, 2004), Java 8 (lambdas, 2014), Java 11 (LTS, 2018)
// - Six-month release cycle established (2017)
// - Java 17 LTS (September 14, 2021) - current major LTS
// - Java 21 LTS (September 19, 2023) - latest LTS
// - Over 3 billion devices run Java (famous marketing claim)
// - Dominated enterprise software for decades
// - Android built on Java (2008) - largest mobile OS by users
// - Minecraft written in Java - introduced millions to programming
// - Academic standard for computer science education (2000s-2010s)
// - Verbosity became both feature and criticism
// - "Enterprise" language stereotype developed
//
// INFLUENCED
// ----------
// - C++ (1985) - Syntax foundation, OOP concepts
// - Objective-C (1984) - Object-oriented design patterns
// - Smalltalk (1972) - Pure object-oriented philosophy, garbage collection
// - Mesa (1970s) - Exception handling model
// - Modula-3 (1988) - Module system concepts
// - UCSD Pascal (1978) - Bytecode compilation approach
// - Eiffel (1986) - Design by contract concepts
//
// USED FOR
// --------
// - Enterprise applications and services
// - Android mobile app development
// - Web applications (Spring, Jakarta EE)
// - Big data processing (Hadoop, Spark, Kafka)
// - Financial services and banking systems
// - E-commerce platforms
// - Scientific computing and simulations
// - Game development (Minecraft, RuneScape)
// - Desktop applications (Eclipse IDE, IntelliJ IDEA, NetBeans)
// - Embedded systems
// - IoT devices
// - Cloud services and microservices
// - Trading systems and stock exchanges
// - Government and defense systems
// - Academic teaching and research
// - Legacy system maintenance
//
// KEY FEATURES
// ------------
// - Platform independence via Java Virtual Machine (JVM)
// - Bytecode compilation (.class files)
// - Automatic garbage collection
// - Strong static typing with type inference (var keyword in Java 10+)
// - Object-oriented programming with single inheritance
// - Interface-based multiple inheritance
// - Exception handling (checked and unchecked)
// - Generics for type-safe collections (Java 5+)
// - Lambda expressions and functional interfaces (Java 8+)
// - Stream API for functional programming (Java 8+)
// - Module system (Java 9+)
// - Rich standard library (Java API)
// - Multithreading and concurrency support
// - Reflection and introspection
// - Annotations for metadata
// - JIT (Just-In-Time) compilation for performance
// - Security sandbox for applets
// - No pointers, no pointer arithmetic
// - No operator overloading (except + for strings)
// - No multiple inheritance of classes
//
// CORE SYNTAX
// -----------
// Basic program structure:
//   public class HelloWorld {
//       public static void main(String[] args) {
//           System.out.println("Hello, World!");
//       }
//   }
//
// Class definition:
//   public class Person {
//       private String name;
//       private int age;
//       
//       public Person(String name, int age) {
//           this.name = name;
//           this.age = age;
//       }
//       
//       public String getName() {
//           return name;
//       }
//   }
//
// Interfaces:
//   public interface Drawable {
//       void draw();
//   }
//
// Generics:
//   List<String> names = new ArrayList<>();
//   Map<String, Integer> ages = new HashMap<>();
//
// Lambda expressions (Java 8+):
//   list.forEach(item -> System.out.println(item));
//   list.stream()
//       .filter(x -> x > 10)
//       .map(x -> x * 2)
//       .collect(Collectors.toList());
//
// QUIRKS
// ------
// - **Not JavaScript**: Despite similar names
//   * Java: Compiled, statically-typed, OOP, runs on JVM
//   * JavaScript: Interpreted, dynamically-typed, prototype-based, runs in browsers
//   * Names similar due to marketing (JavaScript named to ride Java hype)
//
// - **Verbosity**: Notorious for boilerplate code
//   * Getter/setter methods for every field
//   * Factory factories and abstract factories
//   * "Enterprise" stereotype: AbstractSingletonProxyFactoryBean
//   * Modern Java reduced this (records, var, etc.)
//
// - **Checked exceptions**: Must declare or handle
//   * Method signatures list thrown exceptions
//   * `throws IOException` required in declaration
//   * try-catch blocks everywhere
//   * Controversial design decision
//
// - **Everything is a class**: No free functions
//   * `public static void main` required entry point
//   * `Math.sqrt()` instead of `sqrt()`
//   * Can't write standalone function
//
// - **Primitive vs Object types**: Two type systems
//   * Primitives: int, boolean, double (lowercase)
//   * Objects: Integer, Boolean, Double (capitalized)
//   * Autoboxing/unboxing converts between them
//   * Performance implications
//
// - **No operator overloading**: Except String +
//   * `+` works for string concatenation
//   * Cannot define `+` for custom classes
//   * Unlike C++, C#, Kotlin, Scala
//
// - **Single inheritance only**: For classes
//   * Class can extend only one class
//   * But implement multiple interfaces
//   * Composition over inheritance encouraged
//
// - **Final keyword**: Multiple meanings
//   * final variable = constant
//   * final method = cannot override
//   * final class = cannot extend
//   * Context determines meaning
//
// - **Static imports**: Can import static methods
//   * `import static java.lang.Math.*;`
//   * Then use `sqrt(4)` instead of `Math.sqrt(4)`
//   * Controversial for readability
//
// - **Type erasure**: Generics removed at runtime
//   * `List<String>` becomes `List` in bytecode
//   * Cannot check generic type at runtime
//   * Legacy compatibility decision
//   * Source of confusion
//
// - **No default arguments**: Must overload methods
//   * `void method(int x)` and `void method(int x, int y)`
//   * Cannot write `void method(int x, int y = 0)`
//   * Builder pattern emerged as workaround
//
// - **Null everywhere**: Billion dollar mistake
//   * Any reference can be null
//   * NullPointerException very common
//   * Optional<T> added in Java 8
//   * @Nullable/@NonNull annotations help
//
// - **Array covariance**: Surprising behavior
//   * `String[]` is subtype of `Object[]`
//   * Can cause runtime ArrayStoreException
//   * Generics fixed this (invariant)
//
// - **== vs equals()**: Reference vs value equality
//   * `==` compares references (memory address)
//   * `.equals()` compares values
//   * `"hello" == "hello"` might be true (string pool)
//   * Common beginner mistake
//
// - **Package naming**: Reverse domain convention
//   * `com.company.project.module`
//   * Creates deep directory structures
//   * Prevents naming conflicts
//
// - **Classpath hell**: Dependency management pain
//   * Multiple versions of same library conflicts
//   * Maven/Gradle emerged to solve this
//   * Module system in Java 9+ helps
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Write once, run anywhere" - Java marketing slogan (WORA)
// - "Write once, debug everywhere" - Programmer's joke about WORA
// - "Java is to JavaScript what car is to carpet" - Common comparison
// - "Java is like Alzheimer's, it starts off slow and eventually takes away your memory" - Programmer humor about garbage collection
// - "I invented Java. I fixed most of its flaws in Kotlin" - James Gosling (paraphrased)
// - "3 billion devices run Java" - Oracle marketing (from 2013)
// - "Java: write once, run away" - Critics' version of WORA
// - "If Java had true garbage collection, most programs would delete themselves upon execution" - Robert Sewell
// - "Java is, in many ways, C++" - Bjarne Stroustrup (C++ creator)
// - "The best thing about Java is the JVM" - Modern developers (using Kotlin, Scala, Clojure)
//
// NOTES ON JAVA SYNTAX
// --------------------
// - Strongly statically typed language
// - Case-sensitive identifiers
// - Comments: // (single), /* */ (multi), /** */ (Javadoc)
// - Every file is a class (file name must match public class name)
// - Package declaration at top: package com.example;
// - Import statements: import java.util.*;
// - Access modifiers: public, private, protected, package-private (default)
// - Primitive types: byte, short, int, long, float, double, boolean, char
// - Object types: String, Integer, List, Map, etc.
// - Arrays: int[] nums = new int[10];
// - Generics: List<T>, Map<K,V>
// - Annotations: @Override, @Deprecated, @SuppressWarnings
// - Exception handling: try-catch-finally, throws
// - String concatenation with +
// - Character literals: 'A', '\n', '\u0041'
// - Number suffixes: 123L (long), 3.14f (float), 3.14d (double)
// - Binary literals: 0b1010 (Java 7+)
// - Underscore in numbers: 1_000_000 (Java 7+)
// - Text blocks: """multi-line""" (Java 13+)
// - Switch expressions (Java 14+)
// - Records (Java 16+)
// - Pattern matching (Java 16+)
// - Sealed classes (Java 17+)


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('java', {
  rules: [
    // PRIORITY 100: Comments must come first
    // Single-line comments
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,
      priority: 100
    },
    
    // Multi-line comments
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // Javadoc comments
    {
      class: 'comment',
      pattern: /\/\*\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 90: Strings
    // Double-quoted strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // Single-quoted characters
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 90
    },
    
    // Text blocks (Java 13+)
    {
      class: 'string',
      pattern: /"""[\s\S]*?"""/g,
      priority: 90
    },
    
    // PRIORITY 70: Annotations
    {
      class: 'decorator',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 70
    },
    
    // PRIORITY 65: Package and import statements
    {
      class: 'keyword',
      pattern: /\b(package|import)\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Access modifiers and class-related keywords
    {
      class: 'keyword',
      pattern: /\b(public|private|protected|static|final|abstract|synchronized|volatile|transient|native|strictfp|sealed|non-sealed)\b/g,
      priority: 60
    },
    
    // PRIORITY 55: Control flow and structure keywords
    {
      class: 'keyword',
      pattern: /\b(class|interface|enum|record|extends|implements|permits|new|this|super|return|if|else|switch|case|default|break|continue|for|while|do|try|catch|finally|throw|throws|instanceof|assert|yield|var)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Primitive types
    {
      class: 'builtin',
      pattern: /\b(void|boolean|byte|char|short|int|long|float|double)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Common Java classes and types
    {
      class: 'builtin',
      pattern: /\b(String|Integer|Long|Double|Float|Boolean|Character|Byte|Short|Object|Class|System|Math|Thread|Runnable|Exception|Error|List|ArrayList|Map|HashMap|Set|HashSet|Collection|Arrays|Collections|Optional|Stream|Pattern|Matcher|File|Path|Scanner)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Class names (PascalCase before opening brace or extends/implements)
    {
      class: 'class',
      pattern: /\b([A-Z][a-zA-Z0-9]*)\s*(?=\{|extends|implements|permits|<)/g,
      priority: 40
    },
    
    // PRIORITY 35: Method and function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },
    
    // PRIORITY 30: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*[fFdDlL]?\b/g,
      priority: 30
    },
    
    // Hexadecimal
    {
      class: 'number',
      pattern: /\b0[xX][0-9a-fA-F]+[lL]?\b/g,
      priority: 30
    },
    
    // Binary (Java 7+)
    {
      class: 'number',
      pattern: /\b0[bB][01]+[lL]?\b/g,
      priority: 30
    },
    
    // Octal
    {
      class: 'number',
      pattern: /\b0[0-7]+[lL]?\b/g,
      priority: 30
    },
    
    // PRIORITY 20: Booleans and null
    {
      class: 'boolean',
      pattern: /\b(true|false|null)\b/g,
      priority: 20
    }
  ]
});