// darkSyntax/configs/python.js - Python language configuration
// ==============================================================
// Python (1991)
// Python - High-level language emphasizing readability
//
// Configs
// =======================
// ALIASES: ['python', 'py', 'python3']
// File extensions: .py, .pyw
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Guido van Rossum at CWI, Netherlands (1989-1991)
// - First release: Python 0.9.0 (February 1991)
// - Named after Monty Python's Flying Circus (not the snake)
// - Python 1.0 released (January 1994)
// - Python 2.0 with garbage collection and Unicode (October 16, 2000)
// - Python 3.0 "Py3K" broke backward compatibility (December 3, 2008)
// - Python 2.7 final Python 2 version (July 3, 2010)
// - Python 2 EOL (End of Life) on January 1, 2020
// - Python 3.6 added f-strings (December 23, 2016)
// - Python 3.8 added walrus operator := (October 14, 2019)
// - Python 3.9 dictionary merge operator | (October 5, 2020)
// - Python 3.10 added match/case pattern matching (October 4, 2021)
// - Python 3.11 performance improvements (October 24, 2022)
// - Python 3.12 improved error messages (October 2, 2023)
// - Guido van Rossum = "Benevolent Dictator For Life" until 2018
// - Transitioned to steering council governance (2019)
// - "The Zen of Python" (PEP 20) by Tim Peters defines philosophy
// - Most popular language for AI/ML and data science
// - GitHub's most popular language by pull requests
// - TIOBE Index #1 language (2021-present)
// - Stack Overflow's most wanted language (multiple years)
// - "Second best language for everything" saying
// - PyPI (Python Package Index) hosts 500,000+ packages
// - NumPy, Pandas, TensorFlow, PyTorch built on Python
// - Django and Flask dominate web frameworks
// - Jupyter Notebooks revolutionized data science
//
// INFLUENCED
// ----------
// - ABC language (1980s) - Guido worked on ABC, inspired Python's design
// - Modula-3 (1988) - Exception handling model
// - C (1972) - Implementation language, some syntax
// - Lisp (1958) - First-class functions, list comprehensions
// - Haskell (1990) - List comprehensions syntax
// - Icon (1977) - Generators and iteration
// - Perl (1987) - Text processing, regex
// - Java (1995) - Some OOP concepts
// - JavaScript (1995) - Dynamic typing influence
//
// USED FOR
// --------
// - Machine learning and artificial intelligence
// - Data science and analytics (Pandas, NumPy, Matplotlib)
// - Web development (Django, Flask, FastAPI)
// - Scientific computing and research
// - Automation and scripting
// - DevOps and system administration
// - Testing and quality assurance
// - Web scraping (BeautifulSoup, Scrapy)
// - Network programming and cybersecurity
// - Game development (Pygame, Panda3D)
// - Desktop GUI applications (Tkinter, PyQt, wxPython)
// - Image processing (Pillow, OpenCV)
// - Natural language processing (NLTK, spaCy)
// - Blockchain and cryptocurrency
// - Finance and algorithmic trading
// - Education and teaching programming
// - Rapid prototyping
// - Backend APIs and microservices
//
// KEY FEATURES
// ------------
// - Indentation-based block structure (no braces)
// - Dynamic typing with optional type hints (3.5+)
// - Interpreted language with bytecode compilation
// - Automatic memory management (garbage collection)
// - Everything is an object (first-class functions, classes)
// - Multiple programming paradigms (OOP, functional, imperative)
// - Extensive standard library ("batteries included")
// - List, dict, and set comprehensions
// - Generators and iterators
// - Context managers (with statement)
// - Decorators for metaprogramming
// - Multiple inheritance with MRO
// - Exception handling (try/except/finally)
// - F-strings for string formatting (3.6+)
// - Walrus operator := for assignment expressions (3.8+)
// - Pattern matching with match/case (3.10+)
// - Async/await for asynchronous programming (3.5+)
// - Duck typing ("if it walks like a duck...")
// - REPL for interactive development
// - Cross-platform compatibility
//
// CORE SYNTAX
// -----------
// Basic program:
//   print("Hello, World!")
//
// Variables and types:
//   name = "Alice"
//   age = 30
//   numbers = [1, 2, 3, 4, 5]
//   person = {"name": "Bob", "age": 25}
//
// Functions:
//   def greet(name):
//       return f"Hello, {name}!"
//
// Classes:
//   class Person:
//       def __init__(self, name, age):
//           self.name = name
//           self.age = age
//       
//       def greet(self):
//           return f"Hi, I'm {self.name}"
//
// List comprehension:
//   squares = [x**2 for x in range(10)]
//   evens = [x for x in range(10) if x % 2 == 0]
//
// QUIRKS
// ------
// - **Indentation is syntax**: Not just style
//   * 4 spaces standard (PEP 8)
//   * Mixing tabs/spaces causes errors
//   * No braces or end keywords
//   * "Significant whitespace" debate
//
// - **Python 2 vs Python 3**: Breaking changes
//   * print statement → print() function
//   * / operator: integer division → float division
//   * range() returns list → iterator
//   * Unicode strings by default in Python 3
//   * Python 2 EOL in 2020, but legacy code exists
//
// - **GIL (Global Interpreter Lock)**: Concurrency limit
//   * Only one thread executes Python at a time
//   * Limits multi-threaded performance
//   * Use multiprocessing or async for parallelism
//   * CPython implementation detail
//
// - **self must be explicit**: First parameter
//   * def method(self, arg): not method(arg):
//   * Must explicitly pass self
//   * Unlike implicit this in Java/JavaScript
//
// - **Mutable default arguments**: Gotcha
//   * def func(lst=[]): creates ONE list
//   * Default evaluated at definition time
//   * Shared across all calls
//   * Use None and create inside function
//
// - **Everything is an object**: Even functions
//   * Functions are first-class objects
//   * Can assign to variables, pass as arguments
//   * Classes are objects (metaclasses exist)
//   * Numbers, strings, everything is object
//
// - **Duck typing**: Type checking by behavior
//   * "If it walks like a duck and quacks like a duck..."
//   * No interfaces (until Protocol in 3.8)
//   * Runtime type checking
//   * EAFP: "Easier to Ask Forgiveness than Permission"
//
// - **Truthiness**: Many values are falsy
//   * False, None, 0, 0.0, '', [], {}, set() all falsy
//   * Everything else truthy
//   * if my_list: checks for empty list
//
// - **is vs ==**: Identity vs equality
//   * is checks object identity (same object)
//   * == checks value equality
//   * a = [1]; b = [1]; a == b (True), a is b (False)
//   * Small integers cached: 5 is 5 (True)
//
// - **Slicing syntax**: [start:stop:step]
//   * list[1:5] elements 1-4 (stop exclusive)
//   * list[:3] first 3 elements
//   * list[-1] last element
//   * list[::2] every other element
//   * list[::-1] reverses list
//
// - **Multiple assignment**: Tuple unpacking
//   * a, b = 1, 2
//   * x, y = y, x (swap without temp)
//   * Works with any iterable
//
// - **Underscores in names**: Special meanings
//   * _var private by convention
//   * __var name mangling in classes
//   * __init__ dunder (double underscore) methods
//   * _ stores last REPL result
//
// - **List vs tuple**: Mutable vs immutable
//   * Lists [1, 2, 3] mutable
//   * Tuples (1, 2, 3) immutable
//   * Single element tuple: (1,) not (1)
//
// - **/ vs //**: Division operators
//   * / always float division: 7/2 = 3.5
//   * // floor division: 7//2 = 3
//   * Changed from Python 2 to 3
//
// - **import ***: Namespace pollution
//   * from module import * discouraged
//   * Imports everything into namespace
//   * Hard to track where names come from
//   * Explicit imports preferred
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "There should be one-- and preferably only one --obvious way to do it" - Zen of Python
// - "Beautiful is better than ugly. Explicit is better than implicit." - Zen of Python
// - "Simple is better than complex. Complex is better than complicated." - Zen of Python
// - "Readability counts." - Zen of Python
// - "We are all consenting adults here" - Python philosophy on access control
// - "Python is the second best language for everything" - Common saying
// - "Life is better without braces" - Python vs C-like languages
// - "Whitespace is not the enemy" - Python defense
// - "It's easier to ask forgiveness than permission" - EAFP principle
// - "There are two types of programming languages: those people complain about and those nobody uses" - Bjarne Stroustrup (applies to Python)
//
// NOTES ON PYTHON SYNTAX
// -----------------------
// - Case-sensitive language (name != Name)
// - Comments use # (no multi-line comment syntax)
// - Indentation defines blocks (4 spaces standard)
// - No semicolons required (can use but discouraged)
// - No braces for blocks (indentation only)
// - Variables don't need declaration
// - Dynamic typing (types determined at runtime)
// - Type hints optional: def func(x: int) -> str:
// - Strings: 'single', "double", '''triple''', """triple"""
// - F-strings: f"Hello {name}"
// - Raw strings: r"C:\path\to\file"
// - Function definition: def name(params):
// - Class definition: class Name:
// - Decorators: @decorator
// - List comprehensions: [x for x in iterable if condition]
// - Dict comprehensions: {k: v for k, v in items}
// - Lambda functions: lambda x: x * 2
// - Generators: yield keyword
// - Context managers: with statement
// - Exception handling: try/except/finally/else
// - Walrus operator: := (assignment expression, 3.8+)
// - Pattern matching: match/case (3.10+)
// - Async/await: async def, await (3.5+)
// - None represents null/nil
// - True, False are capitalized
// - pass keyword for empty blocks
// - Multiple assignment: a, b = 1, 2
// - Slice notation: list[start:stop:step]


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('python', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings (multi-line first, then single-line)
    {
      class: 'string',
      pattern: /"""[\s\S]*?"""/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /'''[\s\S]*?'''/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"/g,
      priority: 90
    },
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,
      priority: 90
    },
    
    // PRIORITY 60: Decorators
    {
      class: 'decorator',
      pattern: /@[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 60
    },
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(def|class|if|elif|else|for|while|return|import|from|as|try|except|finally|raise|with|lambda|pass|break|continue|global|nonlocal|assert|del|yield|async|await|in|is|not|and|or|match|case)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Function/class definitions
    {
      class: 'function',
      pattern: /(?:def|class)\s+([a-zA-Z_][a-zA-Z0-9_]*)/g,
      captureGroup: 1,
      priority: 45
    },
    
    // PRIORITY 40: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(print|len|range|str|int|float|list|dict|set|tuple|type|isinstance|issubclass|open|input|enumerate|zip|map|filter|sum|max|min|sorted|abs|round|all|any|iter|next|dir|help|id|hash|repr|format|chr|ord|hex|oct|bin|pow|divmod|reversed|slice|staticmethod|classmethod|property|super|hasattr|getattr|setattr|delattr|vars|locals|globals)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Magic methods (dunder methods)
    {
      class: 'builtin',
      pattern: /\b__[a-zA-Z_][a-zA-Z0-9_]*__\b/g,
      priority: 35
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: self and cls
    {
      class: 'variable',
      pattern: /\b(self|cls)\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Numbers
    {
      class: 'number',
      pattern: /\b0x[0-9A-Fa-f]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0o[0-7]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b0b[01]+\b/g,
      priority: 22
    },
    {
      class: 'number',
      pattern: /\b\d+\.?\d*([eE][+-]?\d+)?[jJ]?\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Boolean and None
    {
      class: 'boolean',
      pattern: /\b(True|False|None)\b/g,
      priority: 15
    }
  ]
});