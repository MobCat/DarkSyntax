// darkSyntax/configs/html.js - HTML language configuration
// ==========================================================
// HTML (1993)
// HyperText Markup Language - Standard markup for web documents
//
// Configs
// =======================
// ALIASES: ['html', 'htm', 'xhtml']
// File extensions: .html, .htm, .xhtml
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by Tim Berners-Lee at CERN (1991)
// - First public description "HTML Tags" document (1991)
// - HTML 1.0 informal specification (1993)
// - HTML 2.0 - First standard specification, RFC 1866 (November 1995)
// - HTML 3.2 - W3C Recommendation (January 14, 1997)
// - HTML 4.0 - CSS separation, internationalization (December 18, 1997)
// - HTML 4.01 - Bug fixes and clarifications (December 24, 1999)
// - XHTML 1.0 - XML-based reformulation (January 26, 2000)
// - XHTML 2.0 - Abandoned due to lack of backward compatibility (2009)
// - HTML5 - Living standard, modern web APIs (October 28, 2014)
// - HTML 5.1 - Minor updates (November 1, 2016)
// - HTML 5.2 - Payment Request API, dialog element (December 14, 2017)
// - HTML Living Standard - Continuous updates by WHATWG (2019+)
// - Original HTML had only 18 elements (1991)
// - First browser: WorldWideWeb (later Nexus) by Tim Berners-Lee (1990)
// - Mosaic browser popularized HTML and the web (1993)
// - Netscape Navigator vs Internet Explorer "Browser Wars" (1995-2001)
// - Web Standards Project founded to promote standards compliance (1998)
// - WHATWG formed by Apple, Mozilla, Opera to develop HTML5 (2004)
// - W3C adopted WHATWG's HTML5 work (2007)
// - Semantic web and microformats movement (mid-2000s)
// - Responsive web design paradigm (2010+)
// - Progressive Web Apps (PWAs) enabled by HTML5 APIs (2015+)
//
// INFLUENCED
// ----------
// - SGML (1986) - HTML's parent language
// - XML (1998) - Influenced XHTML reformulation
// - Markdown (2004) - Simplified markup inspired by HTML
// - JSX (2013) - JavaScript XML syntax for React
// - Pug/Jade (2010) - HTML preprocessor with cleaner syntax
// - Haml (2006) - Ruby HTML abstraction markup language
// - Slim (2010) - Lightweight templating language
// - Emmet (2009) - HTML/CSS abbreviation expansion
// - Web Components (2011) - Custom HTML elements standard
// - AMP HTML (2015) - Accelerated Mobile Pages framework
// - CSS (1996) - Styling language for HTML
// - JavaScript (1995) - Scripting language for HTML interactivity
// - SVG (2001) - XML-based vector graphics embedded in HTML
// - MathML (1998) - Mathematical markup in HTML
// - ARIA (2008) - Accessibility attributes for HTML
// - RSS/Atom (2000s) - XML-based syndication formats
// - Microformats (2005) - Semantic annotations in HTML
// - Schema.org (2011) - Structured data vocabulary
// - Open Graph Protocol (2010) - Social media meta tags
//
// USED FOR
// --------
// - Web pages and web applications
// - Email templates (with inline styles)
// - E-books and digital publications (EPUB)
// - Mobile app interfaces (WebView, hybrid apps)
// - Desktop applications (Electron, NW.js)
// - Progressive Web Apps (PWAs)
// - Single Page Applications (SPAs)
// - Documentation and help systems
// - Game interfaces and HUDs
// - Digital signage and kiosks
// - Smart TV applications
// - In-app browsers and readers
// - RSS/Atom feed content
// - Structured data and semantic markup
//
// KEY FEATURES
// ------------
// - Tag-based markup language with opening and closing tags
// - Case-insensitive tags (convention: lowercase)
// - Nested hierarchical structure (DOM tree)
// - Attributes provide additional information to elements
// - Self-closing tags for void elements (img, br, hr, input)
// - Comments with <!-- --> syntax
// - Character entities for special characters (&amp;, &lt;, &gt;)
// - Semantic elements describe content meaning (header, nav, article)
// - Form elements for user input
// - Multimedia support (audio, video, canvas, svg)
// - Scripting with JavaScript via <script> tags
// - Styling with CSS via <style> tags or style attributes
// - Linking resources (stylesheets, scripts, images)
// - Meta information in <head> section
// - Accessibility features (ARIA attributes, alt text)
// - Data attributes for custom metadata (data-*)
//
// CORE SYNTAX
// -----------
// Basic structure:
//   <!DOCTYPE html>
//   <html>
//     <head>
//       <title>Page Title</title>
//     </head>
//     <body>
//       <h1>Heading</h1>
//       <p>Paragraph text</p>
//     </body>
//   </html>
//
// Elements with attributes:
//   <a href="https://example.com" target="_blank">Link</a>
//   <img src="image.jpg" alt="Description" />
//
// Semantic HTML5:
//   <header>
//     <nav>Navigation</nav>
//   </header>
//   <main>
//     <article>Content</article>
//   </main>
//   <footer>Footer</footer>
//
// Forms:
//   <form action="/submit" method="POST">
//     <input type="text" name="username" required />
//     <button type="submit">Submit</button>
//   </form>
//
// QUIRKS
// ------
// - **Case insensitivity**: Tags work in any case
//   * <DIV>, <div>, <DiV> all valid
//   * Convention: lowercase for readability
//   * XHTML requires lowercase
//
// - **Unclosed tags**: Some browsers auto-close
//   * <p>Text<p>More text → browsers fix it
//   * <li> items don't require closing in HTML (but should)
//   * Void elements never have closing tags
//
// - **Attribute quotes**: Optional in HTML (required in XHTML)
//   * <div class=container> valid HTML
//   * <div class="container"> better practice
//   * Quotes required if value has spaces
//
// - **Boolean attributes**: Presence = true
//   * <input disabled> same as <input disabled="disabled">
//   * checked, selected, readonly, required, autofocus
//   * Can omit value entirely
//
// - **Whitespace handling**: Collapsed to single space
//   * Multiple spaces become one
//   * Newlines treated as spaces
//   * Use &nbsp; or CSS white-space for control
//
// - **Self-closing syntax**: Two valid forms
//   * <br /> XHTML style (with slash)
//   * <br> HTML style (no slash)
//   * Void elements: area, base, br, col, embed, hr, img, input, link, meta, source, track, wbr
//
// - **Implicit tags**: Browsers add missing structure
//   * Missing <html>, <head>, <body> auto-added
//   * <td> without <tr> gets wrapped
//   * Invalid nesting gets fixed (usually)
//
// - **Comment gotchas**: Cannot nest comments
//   * <!-- outer <!-- inner --> outer --> breaks
//   * <!-- -- --> (double dash) can cause issues
//   * Use <!-- comment --> format only
//
// - **Script/style content**: Not parsed as HTML
//   * <script> content is JavaScript
//   * <style> content is CSS
//   * Use CDATA sections in XHTML: <![CDATA[...]]>
//
// - **Entities**: Special character encoding
//   * &lt; for <, &gt; for >, &amp; for &
//   * &nbsp; for non-breaking space
//   * &#number; or &#xhex; for Unicode
//   * Must escape < > & in text content
//
// - **Custom elements**: Must contain hyphen
//   * <my-component> valid custom element
//   * <mycomponent> reserved for standard elements
//   * Web Components standard
//
// - **Data attributes**: Custom metadata
//   * data-* attributes for JavaScript access
//   * <div data-user-id="123">
//   * Access via element.dataset.userId
//
// - **Deprecated elements**: Still work but avoid
//   * <font>, <center>, <marquee>, <blink>
//   * Use CSS instead
//   * <b> → <strong>, <i> → <em> (semantic)
//
// - **DOCTYPE matters**: Triggers standards vs quirks mode
//   * <!DOCTYPE html> for HTML5 (standards mode)
//   * Missing DOCTYPE → quirks mode (old IE behavior)
//   * XHTML DOCTYPE: application/xhtml+xml
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "The dream behind the Web is of a common information space in which we communicate by sharing information" - Tim Berners-Lee
// - "HTML is the lingua franca of the web" - Common saying
// - "View Source taught a generation how to code" - Web developer wisdom
// - "Divitis: The overuse of div elements" - Web development term
// - "Tables are for data, not layouts" - Web standards mantra (post-CSS era)
// - "Semantic HTML is accessible HTML" - A11y community
// - "HTML is forgiving, which is both its strength and weakness" - Developer observation
// - "The web is built on HTML, CSS, and JavaScript - the holy trinity" - Common phrase
// - "HTML5 is not a technology, it's a collection of technologies" - Standards discussion
// - "Use the platform" - Modern web development philosophy (use native HTML)
//
// NOTES ON HTML SYNTAX
// --------------------
// - Case-insensitive but lowercase is standard convention
// - Comments use <!-- comment --> syntax
// - Tags use angle brackets: <tagname>
// - Most tags have opening <tag> and closing </tag>
// - Void elements self-close: <br>, <img>, <input>, <hr>, <meta>, <link>
// - Attributes: name="value" or name='value'
// - Boolean attributes can omit value: disabled, checked, required
// - Character entities: &name; or &#decimal; or &#xhex;
// - Whitespace between tags is collapsed
// - DOCTYPE declaration: <!DOCTYPE html> for HTML5
// - Root element: <html>
// - Head section: <head> (metadata, title, links, scripts, styles)
// - Body section: <body> (visible content)
// - Semantic HTML5: header, nav, main, article, section, aside, footer
// - Block elements: div, p, h1-h6, ul, ol, li, table
// - Inline elements: span, a, strong, em, img, input
// - Forms: form, input, textarea, select, button, label
// - Media: img, audio, video, canvas, svg, picture
// - Scripting: script, noscript
// - Styling: style, link (to external CSS)
// - Tables: table, thead, tbody, tfoot, tr, th, td
// - Lists: ul (unordered), ol (ordered), li (list item), dl, dt, dd
// - Special: br (line break), hr (horizontal rule), wbr (word break opportunity)


darkSyntax.registerLanguage('html', {
  rules: [
    // PRIORITY 100: Comments must come first
    {
      class: 'comment',
      pattern: /<!--[\s\S]*?-->/g,
      priority: 100
    },
    
    // PRIORITY 95: DOCTYPE declaration
    {
      class: 'decorator',
      pattern: /<!DOCTYPE[^>]*>/gi,
      priority: 95
    },
    
    // PRIORITY 90: String attributes (must come before tag names)
    // Double-quoted attributes
    {
      class: 'string',
      pattern: /"(?:[^"\\\n]|\\.)*"/g,  // Added \n
      priority: 90
    },
    
    // Single-quoted attributes
    {
      class: 'string',
      pattern: /'(?:[^'\\\n]|\\.)*'/g,  // Added \n
      priority: 90
    },
    
    // PRIORITY 70: Closing tags (tag name only, not brackets)
    {
      class: 'keyword',
      pattern: /(?<=<\/)([a-zA-Z][a-zA-Z0-9\-]*)(?=>)/g,
      priority: 70
    },
    
    // PRIORITY 65: Self-closing tag names
    {
      class: 'keyword',
      pattern: /(?<=<)([a-zA-Z][a-zA-Z0-9\-]*)(?=[^>]*\/>)/g,
      priority: 65
    },
    
    // PRIORITY 60: Opening tag names (just the tag name part, not the bracket)
    {
      class: 'keyword',
      pattern: /(?<=<)([a-zA-Z][a-zA-Z0-9\-]*)(?=[\s>])/g,
      priority: 60
    },
    
    // PRIORITY 50: Attribute names
    {
      class: 'function',
      pattern: /\b([a-zA-Z][a-zA-Z0-9\-]*)(?=\s*=)/g,
      captureGroup: 1,
      priority: 50
    },
    
    // PRIORITY 45: Common HTML entities
    {
      class: 'number',
      pattern: /&[a-zA-Z]+;|&#[0-9]+;|&#x[0-9a-fA-F]+;/g,
      priority: 45
    },
    
    // PRIORITY 40: Boolean attributes (attributes without values)
    {
      class: 'builtin',
      pattern: /\b(disabled|checked|selected|readonly|required|autofocus|autoplay|controls|loop|muted|multiple|novalidate|formnovalidate|defer|async)\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Event handler attributes
    {
      class: 'decorator',
      pattern: /\b(on[a-z]+)\s*=/g,
      captureGroup: 1,
      priority: 35
    }
  ]
});