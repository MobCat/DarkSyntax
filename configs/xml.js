// darkSyntax/configs/xml.js - XML language configuration
// ========================================================
// XML (1998)
// Extensible Markup Language - The universal data format
//
// Configs
// =======================
// ALIASES: ['xml', 'xsd', 'xsl', 'xslt', 'svg', 'rss', 'atom', 'plist']
// File extensions: .xml, .xsd, .xsl, .xslt, .svg, .rss, .atom, .plist
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Published as W3C Recommendation on February 10, 1998
// - Designed by W3C's XML Working Group led by Tim Bray, Jean Paoli, and C. M. Sperberg-McQueen
// - Evolved from SGML (1986) - simplified subset for web use
// - XML 1.0 specification has remained remarkably stable (5 editions, all backward compatible)
// - XML 1.1 (2004) - minor update for Unicode support, rarely used
// - Succeeded proprietary formats: Microsoft's RTF, Adobe's PostScript for data
// - Peak popularity: 2000-2010 (the "XML everywhere" era)
// - SOAP web services (2000-2010) made XML dominant for APIs
// - RSS feeds (1999) and Atom (2005) brought XML to content syndication
// - Office Open XML (2006) - Microsoft Office's file format
// - SVG (1999) - vector graphics format
// - Android layouts and manifests (2008) - mobile app configuration
// - Maven pom.xml (2004) and Ant build.xml (2000) - Java build tools
// - Gradual decline post-2010 as JSON became preferred for web APIs
// - Still dominant in: configuration files, document formats, SOAP, government systems
//
// INFLUENCED
// ----------
// - JSON (2001) - Created as lightweight alternative to XML's verbosity
// - YAML (2001) - Human-readable alternative, less angle bracket noise
// - Protocol Buffers (2008) - Google's binary alternative for efficiency
// - TOML (2013) - Configuration file format, reaction to XML complexity
// - HTML5 (2014) - Allowed looser syntax after years of XHTML strictness
// - Markdown (2004) - Simple markup without tags
// - JSX (2013) - React's XML-like syntax for JavaScript
// - XAML (2006) - Microsoft's UI markup language for .NET
//
// USED FOR
// --------
// - Configuration files (web.config, app.config, .csproj, pom.xml)
// - Data interchange and APIs (SOAP web services, REST XML responses)
// - Document formats (DOCX, XLSX, ODT, EPUB are ZIP files containing XML)
// - Vector graphics (SVG - Scalable Vector Graphics)
// - Content syndication (RSS, Atom feeds)
// - Build systems (Maven pom.xml, Ant build.xml, MSBuild)
// - Android app development (layouts, manifests, resources)
// - Database data export/import
// - Metadata and schemas (XSD, DTD)
// - Transformations (XSLT for converting XML to other formats)
// - Government and healthcare data exchange (HL7, X12 EDI)
// - Apple property lists (.plist files)
// - Sitemaps for search engines
//
// KEY FEATURES
// ------------
// - Self-describing documents with custom tags
// - Hierarchical tree structure
// - Strict syntax rules (well-formedness)
// - Case-sensitive tag names
// - All tags must be properly closed
// - Attributes must be quoted
// - Support for namespaces to avoid naming conflicts
// - Processing instructions (<?xml version="1.0"?>)
// - Comments (<!-- comment -->)
// - CDATA sections for literal text
// - Entity references (&amp; &lt; &gt; &quot; &apos;)
// - DOCTYPE declarations for validation
// - Schema validation (DTD, XSD)
// - Unicode support
// - Platform and language independent
//
// CORE SYNTAX
// -----------
// Basic structure:
//   <?xml version="1.0" encoding="UTF-8"?>
//   <root>
//       <element attribute="value">content</element>
//   </root>
//
// Tag types:
//   <opening-tag>         Opening tag
//   </closing-tag>        Closing tag
//   <self-closing/>       Self-closing tag
//   <ns:tag>              Namespaced tag
//
// Attributes:
//   <tag name="value" id='123'>
//   Must be quoted (single or double quotes)
//
// Special sections:
//   <!-- Comment -->
//   <![CDATA[Literal <text> here]]>
//   <?processing instruction?>
//   <!DOCTYPE root SYSTEM "file.dtd">
//
// Entity references:
//   &amp;   → &
//   &lt;    → <
//   &gt;    → >
//   &quot;  → "
//   &apos;  → '
//   &#160;  → (decimal character)
//   &#xA0;  → (hex character)
//
// QUIRKS
// ------
// - **Verbosity**: Everything needs opening AND closing tags
//   * <person><name>Alice</name></person> vs JSON: {"person": {"name": "Alice"}}
//   * Led to "XML is like violence" joke: "If it doesn't solve your problem, use more of it"
// - **Whitespace significance varies**: Depends on schema/application
//   * Sometimes whitespace matters, sometimes it's ignored
//   * No consistent rule - application-dependent
// - **Namespace confusion**: xmlns declarations are powerful but complex
//   * <root xmlns:custom="http://example.com/schema">
//   * URL-looking identifiers that aren't URLs
// - **Empty elements ambiguity**: <tag></tag> vs <tag/>
//   * Semantically identical but some parsers/tools treat differently
//   * XML says they're the same, but bugs happen
// - **Attribute vs element debate**: No clear rule when to use which
//   * <person name="Alice"/> vs <person><name>Alice</name></person>
//   * Religious wars fought over this design decision
// - **Character encoding issues**: UTF-8 vs UTF-16 vs ISO-8859-1
//   * Encoding declaration must match actual encoding or parsing fails
//   * "Byte Order Mark" (BOM) can cause mysterious errors
// - **Entity reference overhead**: Must escape &, <, >, ", '
//   * &amp; &lt; &gt; &quot; &apos; everywhere
//   * Or use CDATA: <![CDATA[<script>code & stuff</script>]]>
// - **No native data types**: Everything is a string
//   * "true" vs "1" vs "yes" for booleans - application decides
//   * Numbers are just text: "42" needs parsing
// - **Mixed content complexity**: Text + elements = parser headaches
//   * <p>Some <b>bold</b> text</p> harder to process than pure structure
// - **XML 1.0 vs 1.1 confusion**: 1.1 added features nobody uses
//   * Version="1.1" declaration instantly makes files less compatible
//   * Stick to 1.0 unless you need obscure Unicode control characters
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "XML is like violence. If it doesn't solve your problem, you're not using enough of it." - Anonymous
// - "JSON is XML for the new generation." - Douglas Crockford (creator of JSON)
// - "The problem with XML is that everyone wanted to use it for everything." - Tim Bray (co-inventor of XML)
// - "I've seen things you people wouldn't believe. XML config files with 10,000 lines. I watched angle brackets glitter in the dark near the Tannhäuser Gate." - Blade Runner parody
// - "Friends don't let friends write XML by hand." - Developer wisdom
// - "XML: The angle bracket cartel's gift to humanity" - Sarcastic developer commentary
//
// NOTES ON XML SYNTAX
// -------------------
// - Case-sensitive: <Tag> and <tag> are different
// - All tags must be properly closed
// - Attributes must be quoted (single or double)
// - Processing instructions: <?target instructions?>
// - Comments cannot contain -- inside
// - CDATA sections: <![CDATA[unescaped content]]>
// - Entity references for special characters
// - Namespace prefixes: <ns:element>
// - Well-formedness required (proper nesting, closed tags)
// - UTF-8 encoding is default


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('xml', {
  rules: [
    // PRIORITY 100: Comments (must run first to prevent internal highlighting)
    {
      class: 'comment',
      pattern: /<!--[\s\S]*?-->/g,
      priority: 100
    },
    
    // PRIORITY 95: CDATA sections (literal text, treat as strings)
    {
      class: 'string',
      pattern: /<!\[CDATA\[[\s\S]*?\]\]>/g,
      priority: 95
    },
    
    // PRIORITY 90: DOCTYPE keyword only (allows internal content to be highlighted)
    {
      class: 'decorator',
      pattern: /<!([Dd][Oo][Cc][Tt][Yy][Pp][Ee])\b/g,
      priority: 90,
      captureGroup: 1
    },
    
    // PRIORITY 85: Processing instruction targets (xml, php, etc.)
    // Only highlights the target name, allows attributes inside to be highlighted
    {
      class: 'decorator',
      pattern: /<\?([a-zA-Z_][a-zA-Z0-9_-]*)/g,
      priority: 85,
      captureGroup: 1
    },
    
    // PRIORITY 80: Attribute values (must come before attribute names)
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 80
    },
    {
      class: 'string',
      pattern: /'(?:[^'\\]|\\.)*'/g,
      priority: 80
    },
    
    // PRIORITY 70: Attribute names (inside tags)
    {
      class: 'builtin',
      pattern: /\s([a-zA-Z_:][a-zA-Z0-9_:.-]*)\s*=/g,
      priority: 70,
      captureGroup: 1
    },
    
    // PRIORITY 60: Closing tags (</tagname>)
    {
      class: 'keyword',
      pattern: /<\/([a-zA-Z_:][a-zA-Z0-9_:.-]*)\s*>/g,
      priority: 60,
      captureGroup: 1
    },
    
    // PRIORITY 50: Opening/self-closing tag names
    {
      class: 'keyword',
      pattern: /<([a-zA-Z_:][a-zA-Z0-9_:.-]*)/g,
      priority: 50,
      captureGroup: 1
    },
    
    // PRIORITY 20: Entity references (&amp; &lt; &#160; &#xA0;)
    {
      class: 'number',
      pattern: /&[a-zA-Z]+;|&#\d+;|&#x[0-9a-fA-F]+;/g,
      priority: 20
    }
  ]
});