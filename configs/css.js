// darkSyntax/configs/css.js - CSS language configuration
// =======================================================
// CSS (1996)
// Cascading Style Sheets - Presentation language for HTML and XML
//
// Configs
// =======================
// ALIASES: ['css']
// File extensions: .css
//
// LANGUAGE SYNTAX NOTES
// =====================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - CSS first proposed by Håkon Wium Lie (October 10, 1994) at CERN
// - CSS1 W3C Recommendation released (December 17, 1996)
// - Internet Explorer 3 first browser to support CSS (August 1996)
// - Netscape Navigator 4.0 added partial CSS support (1997)
// - Opera 3.5 released with excellent CSS1 support (November 1998)
// - CSS2 W3C Recommendation published (May 12, 1998)
// - CSS 2.1 Candidate Recommendation (2007, finalized June 2011)
// - CSS3 modular approach begun (1998-1999, ongoing since 2011)
// - First CSS Acid Test published by Todd Fahrner (October 1998)
// - Acid2 test launched by Håkon Wium Lie (2005)
// - Modern CSS features: Grid Layout, Flexbox, Custom Properties
// - CSS4 modules in development (2020s) - no single specification
// - Container queries, :has() selector, color functions evolved
// - Separated content from presentation, revolutionized web design
// - Enabled responsive web design and cross-device compatibility
// - W3C Workshop on Style Sheets held in France (November 1995)
// - Microsoft Internet Explorer drove early CSS adoption
// - Opera browser (1998) proved CSS could be implemented correctly
//
// INFLUENCED
// ----------
// - Sass/SCSS (2006) - Hampton Catlin & Natalie Weizenbaum - Variables, nesting, mixins
// - LESS (2009) - Alexis Sellier - JavaScript-based CSS preprocessor
// - Stylus (2010) - TJ Holowaychuk - Minimalist syntax, Node.js integration
// - PostCSS (2013) - Andrey Sitnik - Plugin-based CSS transformation
// - Styled Components (2016) - Glen Maddern - CSS-in-JS for React
// - Emotion (2017) - CSS-in-JS with better performance
// - Tailwind CSS (2017) - Adam Wathan - Utility-first CSS framework
// - CSS Modules (2015) - Scoped styles for JavaScript
// - JSS (2014) - JavaScript Style Sheets
// - CSS Houdini (ongoing) - Low-level CSS API for browsers
//
// USED FOR
// --------
// - Web page styling and layout (primary purpose)
// - Responsive design for mobile, tablet, desktop
// - Print stylesheets for document formatting
// - User interface design for web applications
// - Animations and transitions
// - Theming and branding consistency
// - Accessibility improvements (screen readers)
// - Email template styling (limited support)
// - Progressive enhancement strategies
// - Design systems and component libraries
//
// KEY FEATURES
// ------------
// - Separation of content (HTML) from presentation (CSS)
// - Cascade: Multiple stylesheets can be applied with specificity rules
// - Selectors: Target HTML elements by type, class, ID, attributes
// - Box model: Margin, border, padding, content structure
// - Responsive design with media queries
// - Flexbox for one-dimensional layouts
// - Grid for two-dimensional layouts
// - CSS Variables (Custom Properties) for theming
// - Animations and transitions for interactive effects
// - Pseudo-classes (:hover, :focus) and pseudo-elements (::before, ::after)
// - Transforms: Rotate, scale, translate, skew
// - Modern color functions: rgb(), hsl(), lch()
// - Container queries for component-level responsiveness
// - Subgrid for nested grid layouts
// - :has() parent selector (CSS4)
//
// CORE SYNTAX
// -----------
// Basic rule structure:
//   selector {
//     property: value;
//     property: value;
//   }
//
// Selectors:
//   element         - h1, p, div
//   .class          - .button, .header
//   #id             - #main, #nav
//   [attribute]     - [type="text"]
//   :pseudo-class   - :hover, :first-child
//   ::pseudo-element - ::before, ::after
//
// Box Model:
//   margin: 10px;
//   border: 1px solid #ccc;
//   padding: 20px;
//   width: 100%;
//
// Responsive Design:
//   @media (max-width: 768px) {
//     body { font-size: 14px; }
//   }
//
// Flexbox:
//   .container {
//     display: flex;
//     justify-content: center;
//     align-items: center;
//   }
//
// Grid:
//   .grid {
//     display: grid;
//     grid-template-columns: repeat(3, 1fr);
//     gap: 20px;
//   }
//
// QUIRKS
// ------
// - **No single-line comments**: Only /* */ block comments exist
//   * Common mistake: Using // will break CSS
//   * Must close every comment or entire stylesheet fails
//
// - **Specificity wars**: More specific selectors override less specific
//   * ID (#main) beats class (.header) beats element (div)
//   * Inline styles override everything except !important
//   * !important should be used sparingly
//
// - **The cascade**: Order matters when specificity is equal
//   * Later rules override earlier rules
//   * External, internal, and inline styles cascade
//   * Inherited properties can be overridden
//
// - **Box model confusion**: width doesn't include padding/border by default
//   * Total width = width + padding + border
//   * Use box-sizing: border-box; to include padding/border
//   * margin doesn't count toward element width
//
// - **Margin collapse**: Vertical margins merge between elements
//   * Top margin of 20px + bottom margin of 30px = 30px (not 50px)
//   * Only applies to vertical margins, not horizontal
//   * Doesn't happen with padding or borders
//
// - **Float behavior**: Floated elements removed from normal flow
//   * Parent containers don't expand for floated children
//   * clearfix hack needed to contain floats
//   * Flexbox/Grid largely replaced float-based layouts
//
// - **Position property**: Different behaviors cause confusion
//   * static: Normal flow (default)
//   * relative: Positioned relative to original position
//   * absolute: Positioned relative to nearest positioned ancestor
//   * fixed: Positioned relative to viewport
//   * sticky: Hybrid of relative and fixed
//
// - **Z-index stacking**: Only works on positioned elements
//   * Must have position: relative/absolute/fixed
//   * Stacking contexts create isolated layers
//   * Parent z-index affects all children
//
// - **Vendor prefixes**: Browser-specific properties needed for new features
//   * -webkit- for Chrome, Safari, newer Edge
//   * -moz- for Firefox
//   * -ms- for old Internet Explorer/Edge
//   * -o- for old Opera
//   * Use autoprefixer tools to add automatically
//
// - **Color formats**: Multiple ways to specify colors
//   * Hex: #FF0000, #F00 (shorthand)
//   * RGB: rgb(255, 0, 0), rgba(255, 0, 0, 0.5)
//   * HSL: hsl(0, 100%, 50%), hsla(0, 100%, 50%, 0.5)
//   * Named: red, blue, transparent
//   * Modern: lch(), oklch() for perceptual uniformity
//
// - **Units confusion**: Many unit types with different behaviors
//   * px: Absolute pixels
//   * em: Relative to parent font-size
//   * rem: Relative to root font-size
//   * %: Relative to parent dimension
//   * vh/vw: Viewport height/width percentage
//   * vmin/vmax: Smaller/larger of vh or vw
//
// - **Browser inconsistencies**: Different rendering across browsers
//   * Reset or normalize CSS recommended
//   * Test across multiple browsers
//   * Use feature detection, not browser detection
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "CSS is simple... until it isn't" - Every CSS developer
// - "This thing could have been owned by one company... Instead we have a web which is slightly disorganised, and there's a lot of rubbish out there, but it's also a wonderful place" - Håkon Wium Lie
// - "The idea that two designs can be averaged to come up with an intermediate style seems utterly wrong to me. What happens when my blue-on-yellow style is combined with somebody else's yellow-on-blue?" - Bert Bos
// - "Web enlightenment has been achieved thanks to the tireless efforts of folk like the W3C, WaSP and the major browser creators" - CSS Zen Garden
// - "Littering a dark and dreary road lay the past relics of browser-specific tags, incompatible DOMs, and broken CSS support" - Dave Shea (CSS Zen Garden)
// - "Tables are for tabular data, not for layout" - Web Standards movement
//
// NOTES ON CSS SYNTAX
// -------------------
// - Case-insensitive for property names and values (mostly)
// - Selectors are case-sensitive when matching HTML
// - No single-line comments (//) - only /* */ block comments
// - Semicolons separate declarations, last one optional
// - Declarations grouped in curly braces { }
// - @-rules control behavior: @media, @import, @keyframes, @font-face
// - Pseudo-classes select element states: :hover, :focus, :nth-child()
// - Pseudo-elements create virtual elements: ::before, ::after
// - Specificity: inline > ID > class > element
// - !important overrides specificity (use sparingly)
// - Inheritance: Some properties inherit from parent (color, font)
// - Box model: content, padding, border, margin
// - Units: px, em, rem, %, vh, vw, pt, cm, mm, in
// - Colors: hex (#FF0000), rgb(), rgba(), hsl(), hsla(), named
// - Modern features: CSS Grid, Flexbox, Custom Properties, Container Queries
// - Vendor prefixes for experimental features: -webkit-, -moz-, -ms-


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('css', {
  rules: [
    // PRIORITY 100: Comments must come first
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,
      priority: 100
    },
    
    // PRIORITY 90: Strings (in content, url(), etc.)
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
    
    // PRIORITY 80: @-rules (must come before selectors)
    {
      class: 'decorator',
      pattern: /@[a-zA-Z-]+/g,
      priority: 80
    },
    
    // PRIORITY 70: Important declarations
    {
      class: 'keyword',
      pattern: /!important\b/g,
      priority: 70
    },
    
    // PRIORITY 70: CSS Custom Properties (variables)
    {
      class: 'variable',
      pattern: /--[a-zA-Z][a-zA-Z0-9_-]*/g,
      priority: 70
    },
    
    // PRIORITY 65: Pseudo-classes and pseudo-elements
    {
      class: 'builtin',
      pattern: /::?[a-zA-Z-]+/g,
      priority: 65
    },
    
    // PRIORITY 62: Universal selector
    {
      class: 'keyword',
      pattern: /\*/g,
      priority: 62
    },
    
    // PRIORITY 60: ID selectors
    {
      class: 'function',
      pattern: /#[a-zA-Z][a-zA-Z0-9_-]*/g,
      priority: 60
    },
    
    // PRIORITY 55: Class selectors
    {
      class: 'function',
      pattern: /\.[a-zA-Z][a-zA-Z0-9_-]*/g,
      priority: 55
    },
    
    // PRIORITY 50: Property names (before colon)
    {
      class: 'keyword',
      pattern: /\b([a-z-]+)(?=\s*:)/g,
      priority: 50
    },
    
    // PRIORITY 45: CSS functions
    {
      class: 'function',
      pattern: /\b([a-z-]+)(?=\()/g,
      priority: 45
    },
    
    // PRIORITY 40: Color values - Hex
    {
      class: 'number',
      pattern: /#[0-9a-fA-F]{3,8}\b/g,
      priority: 40
    },
    
    // PRIORITY 35: Units (px, em, rem, %, etc.)
    {
      class: 'number',
      pattern: /\b\d+\.?\d*(px|em|rem|%|vh|vw|vmin|vmax|ch|ex|cm|mm|in|pt|pc|deg|rad|grad|turn|s|ms|fr)\b/g,
      priority: 35
    },
    
    // PRIORITY 30: Numbers without units
    {
      class: 'number',
      pattern: /\b\d+\.?\d*\b/g,
      priority: 30
    },
    
    // PRIORITY 25: CSS keywords and values
    {
      class: 'builtin',
      pattern: /\b(inherit|initial|unset|revert|auto|none|normal|hidden|visible|block|inline|inline-block|flex|grid|absolute|relative|fixed|sticky|static|left|right|center|top|bottom|bold|italic|underline|solid|dashed|dotted|double|transparent|currentColor|row|column|wrap|nowrap|space-between|space-around|space-evenly|start|end|baseline|stretch|border-box|content-box|cover|contain|repeat|no-repeat|scroll|local|ease|linear|ease-in|ease-out|ease-in-out|forwards|backwards|both|infinite|alternate|reverse|paused|running)\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Named colors
    {
      class: 'builtin',
      pattern: /\b(black|white|red|green|blue|yellow|orange|purple|pink|brown|gray|grey|cyan|magenta|lime|navy|teal|aqua|maroon|olive|silver|fuchsia|indigo|violet|gold|coral|salmon|khaki|crimson|lavender|plum|turquoise|tan|beige|ivory|azure|snow|honeydew|wheat|tomato|orchid|sienna|peru|chocolate)\b/g,
      priority: 20
    }
  ]
});