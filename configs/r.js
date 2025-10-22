// darkSyntax/configs/r.js - R language configuration
// ===================================================
// R (1993)
// R - Language for statistical computing and graphics
//
// Configs
// =======================
// ALIASES: ['r', 'rlang', 'rscript']
// File extensions: .r, .R
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Ross Ihaka and Robert Gentleman at University of Auckland (1991-1993)
// - Named "R" after creators' first names (Ross and Robert)
// - Also plays on S language name (R comes before S)
// - First release: R 0.49 (1993)
// - Open source under GPL (1995)
// - R 1.0.0 released (February 29, 2000)
// - R Foundation for Statistical Computing established (2003)
// - Comprehensive R Archive Network (CRAN) launched (1997)
// - Based on S language from Bell Labs (1976)
// - S-PLUS commercial implementation (1988)
// - R became free alternative to S-PLUS
// - RStudio IDE launched (February 28, 2011)
// - Tidyverse collection of packages (2016)
// - Pipe operator %>% revolutionized workflow (2014)
// - Native pipe |> added to base R (2021, R 4.1.0)
// - R Markdown for reproducible research (2012)
// - Shiny for web applications (2012)
// - ggplot2 revolutionized data visualization (2005)
// - Academic standard for statistics
// - Bioconductor for bioinformatics (2001)
// - Over 19,000 packages on CRAN (2024)
// - R Consortium formed by tech companies (2015)
// - Used in pharmaceutical industry for FDA submissions
// - New Zealand origins gave it global reach
//
// INFLUENCED
// ----------
// - S language (1976) - Direct predecessor from Bell Labs
// - Scheme (1975) - Lexical scoping and functional programming
// - Common Lisp (1984) - First-class functions
// - APL (1966) - Vectorized operations
// - Fortran (1957) - Numerical computing influence
// - C (1972) - Implementation language for R core
//
// USED FOR
// --------
// - Statistical analysis and hypothesis testing
// - Data science and machine learning
// - Data visualization and graphics
// - Bioinformatics and genomics
// - Epidemiology and public health
// - Clinical trials and pharmaceutical research
// - Econometrics and financial modeling
// - Social science research
// - Market research and survey analysis
// - Academic research and publications
// - Reproducible research with R Markdown
// - Interactive reports and dashboards (Shiny)
// - Time series analysis and forecasting
// - Spatial data analysis and GIS
// - Text mining and natural language processing
// - Web scraping and data collection
// - Database connectivity and big data
// - Teaching statistics and data science
//
// KEY FEATURES
// ------------
// - Vector-based operations (no explicit loops needed)
// - First-class functions and functional programming
// - Comprehensive statistical functions built-in
// - Extensive graphics capabilities (base, ggplot2, lattice)
// - Data frames as primary data structure
// - Missing value handling (NA)
// - Lazy evaluation of function arguments
// - Lexical scoping with closures
// - Interactive REPL environment
// - Package system via CRAN
// - Integration with C, C++, Fortran
// - Formula notation for modeling (y ~ x)
// - Vectorized arithmetic and logical operations
// - Built-in matrix operations
// - Support for complex numbers
// - Regular expression support
// - Environment and namespace system
// - S3 and S4 object systems
// - Reference classes (R5)
// - Metaprogramming capabilities
//
// CORE SYNTAX
// -----------
// Basic operations:
//   x <- 5
//   y <- c(1, 2, 3, 4, 5)
//   mean(y)
//
// Vectors and data frames:
//   vec <- c(1, 2, 3)
//   df <- data.frame(
//     name = c("Alice", "Bob", "Charlie"),
//     age = c(25, 30, 35)
//   )
//
// Functions:
//   add <- function(a, b) {
//     return(a + b)
//   }
//
// Control flow:
//   if (x > 0) {
//     print("Positive")
//   } else {
//     print("Non-positive")
//   }
//
// Loops:
//   for (i in 1:10) {
//     print(i)
//   }
//
// QUIRKS
// ------
// - **1-based indexing**: Arrays start at 1, not 0
//   * vec[1] is first element
//   * vec[0] returns empty vector
//   * Confusing for programmers from other languages
//
// - **Assignment operators**: Multiple choices
//   * <- preferred assignment operator
//   * = also works (but discouraged in some contexts)
//   * -> right assignment: 5 -> x
//   * <<- super assignment for parent scope
//   * ->> right super assignment
//
// - **Periods in names**: Just a character
//   * my.variable is valid identifier
//   * No special meaning like OOP in other languages
//   * data.frame, read.csv use periods
//   * Different from Python/JavaScript
//
// - **TRUE/FALSE vs T/F**: Both work but different
//   * TRUE and FALSE are keywords
//   * T and F are variables (can be reassigned!)
//   * T <- FALSE; if (T) {} # dangerous
//   * Always use TRUE/FALSE explicitly
//
// - **Vectorization**: Operations on entire vectors
//   * c(1,2,3) + c(4,5,6) = c(5,7,9)
//   * No need for loops
//   * Recycling: shorter vector repeated
//   * c(1,2,3) + c(1) = c(2,3,4)
//
// - **NA vs NULL vs NaN**: Different meanings
//   * NA = missing value (Not Available)
//   * NULL = null object (doesn't exist)
//   * NaN = Not a Number (0/0)
//   * Inf = infinity (1/0)
//   * Each behaves differently in operations
//
// - **Everything is a vector**: Even scalars
//   * 5 is actually c(5)
//   * length(5) = 1
//   * No true scalar type
//
// - **Negative indexing**: Excludes elements
//   * vec[-1] returns all except first
//   * vec[c(-1,-3)] excludes first and third
//   * Cannot mix positive and negative indices
//
// - **Logical indexing**: Filter with boolean vectors
//   * vec[vec > 5] returns elements > 5
//   * df[df$age > 30, ] filters data frame
//   * Powerful but can be cryptic
//
// - **$ for column access**: Data frame syntax
//   * df$column accesses column
//   * df[["column"]] also works
//   * df$new_col <- values adds column
//
// - **Functions as objects**: First-class
//   * functions can be assigned to variables
//   * passed as arguments
//   * returned from functions
//   * Anonymous functions: function(x) x^2
//
// - **Lazy evaluation**: Arguments evaluated when used
//   * function(x, y = expensive()) only evaluates y if used
//   * Enables neat tricks
//   * Can cause confusion with side effects
//
// - **Copy-on-modify**: Memory semantics
//   * R copies when modifying
//   * x <- y; y[1] <- 5; x unchanged
//   * Good for safety, bad for performance
//
// - **Factor type**: Categorical data
//   * Stores categories efficiently
//   * Underlying integer with labels
//   * Can cause unexpected behavior
//   * stringsAsFactors=FALSE often needed
//
// - **Recycling rule**: Vectors repeated
//   * c(1,2,3,4) + c(1,2) = c(2,4,4,6)
//   * Shorter vector recycled to match longer
//   * Warning if lengths not multiples
//
// - **Subsetting syntax**: Many ways
//   * vec[1] first element
//   * vec[c(1,3)] elements 1 and 3
//   * vec[vec > 5] logical subsetting
//   * vec[-2] all except second
//   * df[1, 2] row 1, column 2
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "To understand computations in R, two slogans are helpful: Everything that exists is an object. Everything that happens is a function call." - John Chambers (S creator)
// - "R is quirky, flawed, and an enormous success" - Hadley Wickham
// - "The best thing about R is that it was written by statisticians. The worst thing about R is that it was written by statisticians." - Bo Cowgill
// - "R is not a programming language, it's a data analysis environment" - Common saying
// - "If you can dream it, there's probably an R package for it" - R community
// - "Vectorize or die" - R performance wisdom
// - "In R, everything is a vector" - Fundamental concept
// - "R makes easy things hard and hard things easy" - Learning curve observation
// - "Use <- for assignment, = for function arguments" - Style convention
// - "There are three types of lies: lies, damned lies, and statistics run in Excel" - Modified saying (R advocacy)
//
// NOTES ON R SYNTAX
// -----------------
// - Case-sensitive language (Name != name)
// - Comments use # (no multi-line comments)
// - Assignment: <- (preferred), =, ->, <<-, ->>
// - Vectors created with c(): c(1, 2, 3)
// - Indexing starts at 1, not 0
// - Negative indices exclude elements
// - Logical indexing: vec[vec > 5]
// - Data frames: data.frame(col1=..., col2=...)
// - Column access: df$column or df[["column"]]
// - Function definition: function(args) { body }
// - Control flow: if, else, for, while, repeat
// - Formula notation: response ~ predictors
// - Pipe operators: |> (base R 4.1+), %>% (magrittr)
// - NA for missing values
// - NULL for null objects
// - TRUE/FALSE for booleans (T/F work but discouraged)
// - Periods in names: my.variable (valid identifier)
// - Package loading: library(pkg) or require(pkg)
// - Namespace access: pkg::function()
// - Help system: ?function or help(function)
// - Vectorized operations: c(1,2,3) * 2 = c(2,4,6)
// - Apply family: apply, lapply, sapply, tapply, mapply
// - String concatenation: paste() or paste0()
// - Matrix operations: %*%, t(), solve(), eigen()
// - List creation: list(a=1, b=2, c=3)
// - Factor for categorical: factor(c("a","b","a"))


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('r', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /#.*$/gm,
      priority: 100
    },
    
    // PRIORITY 90: Strings
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
    
    // PRIORITY 50: Keywords
    {
      class: 'keyword',
      pattern: /\b(if|else|repeat|while|function|for|in|next|break|return|stop|warning|message)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Control flow
    {
      class: 'keyword',
      pattern: /\b(tryCatch|withCallingHandlers|stopifnot)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Built-in functions
    {
      class: 'builtin',
      pattern: /\b(c|list|matrix|array|data\.frame|factor|vector|mean|median|sd|var|sum|min|max|length|nrow|ncol|dim|names|colnames|rownames|head|tail|str|summary|print|cat|paste|paste0|sprintf|apply|lapply|sapply|tapply|mapply|aggregate|merge|subset|transform|with|within|attach|detach|library|require|source|load|save|read\.csv|write\.csv|read\.table|write\.table|plot|hist|boxplot|barplot|lines|points|abline|legend|par|png|pdf|dev\.off|lm|glm|anova|t\.test|cor|cov)\b/g,
      priority: 40
    },
    
    // PRIORITY 30: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_\.]*)\s*(?=\()/g,
      priority: 30
    },
    
    // PRIORITY 25: Assignment operators
    {
      class: 'keyword',
      pattern: /<-|<<-|->|->>|=/g,
      priority: 25
    },
    
    // PRIORITY 22: Pipe operators
    {
      class: 'keyword',
      pattern: /\|>|%>%/g,
      priority: 22
    },
    
    // PRIORITY 20: Numbers (including scientific notation and special suffixes)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*(?:[eE][+-]?\d+)?[Li]?\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Boolean and special values
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE|T|F|NULL|NA|NA_integer_|NA_real_|NA_complex_|NA_character_|NaN|Inf)\b/g,
      priority: 15
    }
  ]
});