// darkSyntax/configs/markdown.js - Markdown language configuration
// ===================================================================
// Markdown (2004)
// Markdown - Lightweight markup language for formatted text
//
// Configs
// =======================
// ALIASES: ['markdown', 'md', 'mdown', 'mkd']
// File extensions: .md, .markdown, .mdown, .mkd, .mkdn
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by John Gruber with Aaron Swartz (2004)
// - First announcement on Daring Fireball blog (March 19, 2004)
// - Version 1.0.1 released (December 17, 2004)
// - Designed as "easy-to-read, easy-to-write plain text format"
// - Goal: Readable as-is without rendering
// - Name is a play on "markup" languages like HTML
// - Original Perl implementation by John Gruber
// - Influenced by email conventions and plain text formatting
// - No formal specification initially (led to fragmentation)
// - GitHub Flavored Markdown (GFM) introduced (2009)
// - MultiMarkdown extended original syntax (2005)
// - Pandoc Markdown for academic writing (2006)
// - CommonMark specification effort began (2012)
// - CommonMark 1.0 released (August 28, 2014)
// - Became ubiquitous for README files, documentation, forums
// - Reddit, Stack Overflow, GitHub adopted Markdown variants
// - Static site generators popularized Markdown (Jekyll 2008, Hugo 2013)
// - Academic writing with Markdown and Pandoc gained traction
// - Markdown editors became a software category
// - "Markdown everywhere" philosophy in developer tools
//
// INFLUENCED
// ----------
// - GitHub (2008) - GitHub Flavored Markdown (GFM) for issues, PRs, wikis
// - Stack Overflow (2008) - Q&A formatting with Markdown
// - Reddit (2005) - Comment and post formatting
// - Discord (2015) - Chat message formatting
// - Slack (2013) - Message formatting (modified Markdown)
// - Jekyll (2008) - Static site generator with Markdown
// - Hugo (2013) - Fast static site generator
// - Gatsby (2015) - React-based static site generator
// - Obsidian (2020) - Note-taking with Markdown
// - Notion (2016) - Markdown-style shortcuts
// - Typora (2015) - WYSIWYG Markdown editor
// - Bear (2016) - Note-taking app with Markdown
// - iA Writer (2010) - Distraction-free Markdown editor
// - Ulysses (2003, Markdown support 2016) - Writing app
// - Pandoc (2006) - Universal document converter
// - Jupyter Notebooks (2014) - Markdown cells for documentation
// - R Markdown (2012) - Literate programming for R
// - GitBook (2014) - Documentation platform
// - Read the Docs (2010) - Documentation hosting
// - Docusaurus (2017) - Documentation website generator by Facebook
// - MkDocs (2014) - Documentation generator
// - VuePress (2018) - Vue-powered static site generator
// - Sphinx (2008) - Python documentation (supports Markdown)
// - Discourse (2013) - Forum software with Markdown
// - Joplin (2017) - Open source note-taking
// - Standard Notes (2016) - Encrypted notes with Markdown
// - DEV.to (2016) - Developer blogging platform
// - Hashnode (2019) - Developer blogging
// - Medium (2012) - Story editor inspired by Markdown simplicity
//
// USED FOR
// --------
// - README files and project documentation
// - Technical documentation and wikis
// - Blog posts and articles
// - Note-taking and personal knowledge management
// - Academic writing and papers
// - Comments and forum posts
// - Chat messages and Discord formatting
// - Static website content
// - Email composition (some clients)
// - Book writing and publishing
// - Technical specifications
// - API documentation
// - Presentation slides (with tools like reveal.js)
// - Jupyter notebook documentation cells
// - Git commit messages (formatted)
// - Issue tracking and bug reports
//
// KEY FEATURES
// ------------
// - Plain text format readable without rendering
// - Simple, intuitive syntax
// - Converts to HTML (and other formats via Pandoc)
// - Headers with # symbols (1-6 levels)
// - Emphasis: *italic* or _italic_, **bold** or __bold__
// - Lists: ordered (1. 2. 3.) and unordered (- * +)
// - Links: [text](url) and images: ![alt](url)
// - Code: inline `code` and fenced ```code blocks```
// - Blockquotes with > prefix
// - Horizontal rules with --- or ***
// - Line breaks with two spaces or backslash
// - Automatic URL detection (in many flavors)
// - HTML fallback (can mix HTML in Markdown)
// - Tables (in extended flavors like GFM)
// - Task lists (in GFM): - [ ] and - [x]
// - Strikethrough in GFM: ~~text~~
// - Footnotes (in some flavors)
// - Definition lists (in some flavors)
//
// CORE SYNTAX
// -----------
// Headers:
//   # H1
//   ## H2
//   ### H3
//
// Emphasis:
//   *italic* or _italic_
//   **bold** or __bold__
//   ***bold italic***
//
// Lists:
//   - Item 1
//   - Item 2
//     - Nested item
//   
//   1. First
//   2. Second
//
// Links and images:
//   [Link text](https://url.com)
//   ![Image alt](image.jpg)
//
// Code:
//   Inline `code` here
//   
//   ```javascript
//   // Code block
//   const x = 42;
//   ```
//
// Blockquotes:
//   > This is a quote
//   > Continued quote
//
// QUIRKS
// ------
// - **No standard specification initially**: Led to many flavors
//   * CommonMark attempted to standardize (2014)
//   * GitHub Flavored Markdown (GFM) most popular
//   * Implementations differ in edge cases
//
// - **Whitespace matters**: Line breaks and indentation
//   * Two spaces at end of line = line break
//   * Four spaces or one tab = code block (original)
//   * Blank line needed between elements
//
// - **Emphasis parsing ambiguity**: `*` and `_` interpretation
//   * `this_is_not_italic` vs `this *is* italic`
//   * Underscores inside words often not parsed
//   * Asterisks more reliable for emphasis
//
// - **HTML passthrough**: Can mix HTML and Markdown
//   * <div>HTML content</div> works in Markdown
//   * Some parsers restrict this for security
//   * Markdown inside HTML blocks often not parsed
//
// - **Escaping special characters**: Backslash escapes
//   * \* not parsed as list or emphasis
//   * \[ not parsed as link
//   * \` not parsed as code
//
// - **Link reference definitions**: Can define links separately
//   * [link text][ref]
//   * [ref]: https://url.com
//   * Cleaner for repeated links
//
// - **Indented code blocks**: Four spaces or one tab
//   * Original Markdown feature
//   * Fenced code blocks (```) more popular now
//   * Indented blocks can be triggered accidentally
//
// - **Hard line breaks**: Multiple ways to create
//   * Two spaces at end of line (invisible!)
//   * Backslash at end of line
//   * HTML <br> tag
//   * Blank line creates paragraph break
//
// - **Horizontal rules**: Multiple syntaxes
//   * --- or *** or ___
//   * Must be on own line
//   * At least 3 characters needed
//
// - **Nested lists**: Indentation is critical
//   * 2 or 4 spaces for sub-items (depends on implementation)
//   * Tab characters handled inconsistently
//   * Mixing ordered and unordered can be tricky
//
// - **Automatic link detection**: Varies by flavor
//   * Plain URLs may or may not become links
//   * <https://url.com> always becomes link
//   * Email addresses: <email@example.com>
//
// - **Code block language hints**: Fenced blocks support syntax highlighting
//   * ```javascript
//   * ```python
//   * Not standardized initially, now common
//
// - **Tables**: Extended Markdown (GFM) feature
//   * | Column 1 | Column 2 |
//   * | -------- | -------- |
//   * | Cell 1   | Cell 2   |
//   * Alignment with :---: :--- ---:
//
// - **Task lists**: GFM extension
//   * - [ ] Unchecked
//   * - [x] Checked
//   * Interactive in GitHub, static elsewhere
//
// - **Strikethrough**: GFM extension
//   * ~~crossed out~~
//   * Not in original Markdown
//
// - **Footnotes**: Some flavors support
//   * Text with footnote[^1]
//   * [^1]: Footnote content
//   * Not in CommonMark
//
// - **Front matter**: YAML/TOML metadata
//   * ---
//   * title: Page Title
//   * ---
//   * Used by static site generators
//   * Not part of Markdown spec
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Markdown is intended to be as easy-to-read and easy-to-write as is feasible" - John Gruber
// - "The overriding design goal for Markdown's formatting syntax is to make it as readable as possible" - John Gruber
// - "Markdown is a text-to-HTML conversion tool for web writers" - John Gruber
// - "Write in Markdown, publish anywhere" - Modern content creation motto
// - "Markdown: because Word is too complicated and HTML is too ugly" - Developer joke
// - "README.md: The face of your project" - Open source wisdom
// - "If you can write an email, you can write Markdown" - Common intro phrase
// - "Markdown everywhere" - Developer tools philosophy
// - "The best markup language is the one that gets out of your way" - Content creator wisdom
//
// NOTES ON MARKDOWN SYNTAX
// -------------------------
// - Plain text format with minimal markup
// - Readable in raw form without rendering
// - Headers: # for H1, ## for H2, up to ###### for H6
// - Emphasis: *italic* _italic_ **bold** __bold__ ***bold-italic***
// - Lists: unordered with - or * or +, ordered with 1. 2. 3.
// - Links: [text](url "optional title")
// - Images: ![alt text](url "optional title")
// - Inline code: `code` with backticks
// - Code blocks: indent 4 spaces or use ``` fences
// - Blockquotes: > prefix for quote lines
// - Horizontal rule: --- or *** or ___ (3+ characters)
// - Line breaks: two spaces at end of line or \
// - Escape special chars: \* \` \[ \] etc
// - HTML can be mixed in (most parsers)
// - Case-sensitive for language tags in code blocks
// - Blank lines separate paragraphs
// - Whitespace at start of line matters (indentation)


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('markdown', {
  rules: [
    // PRIORITY 100: Code blocks (fenced with backticks) - must come first
    {
      class: 'comment',
      pattern: /```[\s\S]*?```/g,
      priority: 100
    },
    
    // PRIORITY 95: Inline code
    {
      class: 'string',
      pattern: /`[^`\n]+`/g,
      priority: 95
    },
    
    // PRIORITY 90: Headers (# ## ### etc)
    {
      class: 'keyword',
      pattern: /^#{1,6}\s+.+$/gm,
      priority: 90
    },
    
    // PRIORITY 85: Blockquotes
    {
      class: 'comment',
      pattern: /^>\s+.+$/gm,
      priority: 85
    },
    
    // PRIORITY 80: Horizontal rules
    {
      class: 'keyword',
      pattern: /^(?:---|\*\*\*|___)$/gm,
      priority: 80
    },
    
    // PRIORITY 75: Bold text
    {
      class: 'keyword',
      pattern: /\*\*[^*\n]+\*\*|__[^_\n]+__/g,
      priority: 75
    },
    
    // PRIORITY 70: Italic text
    {
      class: 'variable',
      pattern: /\*[^*\n]+\*|_[^_\n]+_/g,
      priority: 70
    },
    
    // PRIORITY 65: Strikethrough (GFM)
    {
      class: 'decorator',
      pattern: /~~[^~\n]+~~/g,
      priority: 65
    },
    
    // PRIORITY 60: Links [text](url)
    {
      class: 'function',
      pattern: /\[([^\]]+)\]\(([^)]+)\)/g,
      priority: 60
    },
    
    // PRIORITY 55: Images ![alt](url)
    {
      class: 'decorator',
      pattern: /!\[([^\]]*)\]\(([^)]+)\)/g,
      priority: 55
    },
    
    // PRIORITY 50: Link references [text][ref]
    {
      class: 'function',
      pattern: /\[([^\]]+)\]\[([^\]]+)\]/g,
      priority: 50
    },
    
    // PRIORITY 45: Link definitions [ref]: url
    {
      class: 'decorator',
      pattern: /^\[([^\]]+)\]:\s+.+$/gm,
      priority: 45
    },
    
    // PRIORITY 40: Unordered list markers
    {
      class: 'keyword',
      pattern: /^[\s]*[-*+]\s+/gm,
      priority: 40
    },
    
    // PRIORITY 35: Ordered list markers
    {
      class: 'number',
      pattern: /^[\s]*\d+\.\s+/gm,
      priority: 35
    },
    
    // PRIORITY 30: Task list checkboxes (GFM)
    {
      class: 'boolean',
      pattern: /^[\s]*[-*+]\s+\[[x\s]\]/gim,
      priority: 30
    },
    
    // PRIORITY 25: Footnote references [^1]
    {
      class: 'decorator',
      pattern: /\[\^[^\]]+\]/g,
      priority: 25
    },
    
    // PRIORITY 20: HTML tags (common in Markdown)
    {
      class: 'builtin',
      pattern: /<\/?[a-zA-Z][^>]*>/g,
      priority: 20
    }
  ]
});