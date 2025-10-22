// darkSyntax.js - Library core
class DarkSyntax {
  constructor(options = {}) {
    this.configs = {};
    this.initialized = false;
    this.configPath = options.configPath || 'darkSyntax/';
    this.loading = {};
    this.globalShowLineNumbers = options.lineNumbers !== false; // Global default
    
    // Alias mapping system
    this.aliasMap = {};
    this.aliasesLoaded = false;
    this.aliasLoadPromise = null;
    
    // Track missing CSS classes for warnings
    this.missingClasses = new Set();
    this.warnedClasses = new Set();
    
    // Start loading aliases immediately
    this.aliasLoadPromise = this.loadAliasesFile();
  }

  /**
   * Automatically load aliases.js file
   */
  loadAliasesFile() {
    return new Promise((resolve) => {
      const script = document.createElement('script');
      script.src = `${this.configPath}aliases.js`;
      
      script.onload = () => {
        this.aliasesLoaded = true;
        resolve();
      };
      
      script.onerror = () => {
        console.warn('DarkSyntax: aliases.js not found. Language aliases will not be available.');
        this.aliasesLoaded = true;
        resolve();
      };
      
      document.head.appendChild(script);
    });
  }

  /**
   * Initialize language aliases (called by aliases.js)
   */
  initializeAliases(aliasGroups) {
    for (const [canonical, ...aliases] of aliasGroups) {
      this.registerAlias(canonical, ...aliases);
    }
  }

  /**
   * Register a canonical language and its aliases
   */
  registerAlias(canonical, ...aliases) {
    this.aliasMap[canonical] = canonical;
    for (const alias of aliases) {
      this.aliasMap[alias] = canonical;
    }
  }

  /**
   * Resolve any alias to its canonical language name
   */
  resolveLanguage(alias) {
    return this.aliasMap[alias] || alias;
  }

  /**
   * Register a language configuration
   */
  registerLanguage(name, config) {
    config.rules = config.rules.map(rule => ({
      ...rule,
      pattern: rule.pattern.flags.includes('g') 
        ? rule.pattern 
        : new RegExp(rule.pattern.source, rule.pattern.flags + 'g')
    }));
    this.configs[name] = config;
  }

  /**
   * Load a language config file
   */
  async loadLanguage(langAlias) {
    const canonical = this.resolveLanguage(langAlias);
    
    if (this.configs[canonical]) return true;
    if (this.loading[canonical]) return this.loading[canonical];

    this.loading[canonical] = new Promise((resolve, reject) => {
      const script = document.createElement('script');
      script.src = `${this.configPath}configs/${canonical}.js`;

      script.onload = () => {
        this.configs[canonical] ? resolve(true) : resolve(false);
      };

      script.onerror = () => {
        console.warn(`DarkSyntax: Failed to load config for language "${canonical}" (requested as "${langAlias}")`);
        reject(false);
      };

      document.head.appendChild(script);
    });

    return this.loading[canonical];
  }

  /**
   * Check if a CSS class exists in any loaded stylesheet
   */
  checkCSSClass(className) {
    const fullClass = `ds-${className}`;
    let found = false;
    
    try {
      for (const sheet of document.styleSheets) {
        try {
          const rules = sheet.cssRules || sheet.rules;
          if (!rules) continue;
          
          for (const rule of rules) {
            if (rule.selectorText && rule.selectorText.includes(`.${fullClass}`)) {
              found = true;
              break;
            }
          }
          if (found) break;
        } catch (e) {
          // Cross-origin stylesheet, skip
        }
      }
    } catch (e) {
      // Error checking stylesheets
    }
    
    if (!found) {
      this.missingClasses.add(className);
    }
  }

  /**
   * Check if any DarkSyntax theme is loaded
   */
  checkThemeLoaded() {
    let darkSyntaxStylesheet = false;
    
    try {
      for (const sheet of document.styleSheets) {
        // Check if stylesheet URL contains 'darkSyntax'
        if (sheet.href && sheet.href.includes('darkSyntax')) {
          darkSyntaxStylesheet = true;
          
          // Try to verify it has DarkSyntax classes
          try {
            const rules = sheet.cssRules || sheet.rules;
            if (rules) {
              for (const rule of rules) {
                if (rule.selectorText && rule.selectorText.includes('.ds-')) {
                  return true; // Found DarkSyntax theme
                }
              }
            }
          } catch (e) {
            // Cross-origin or blocked, but file exists
            return true;
          }
        }
      }
    } catch (e) {
      // Error checking
    }
    
    if (!darkSyntaxStylesheet) {
      console.error('DarkSyntax: No theme stylesheet found! Make sure to include a theme CSS file like:\n<link rel="stylesheet" href="darkSyntax/themes/darkSyntax-sublime.css">');
      return false;
    }
    
    return true;
  }

  highlight(element, langAlias) {
    const canonical = this.resolveLanguage(langAlias);
    const config = this.configs[canonical];
    
    if (!config) {
      console.warn(`DarkSyntax: No config found for language "${langAlias}" (resolved to "${canonical}")`);
      return;
    }

    const code = element.textContent;

    // Collect all potential matches from all rules
    const allMatches = [];
    for (const rule of config.rules) {
      const pattern = rule.pattern.flags.includes('g') 
        ? rule.pattern 
        : new RegExp(rule.pattern.source, rule.pattern.flags + 'g');
      
      let match;
      while ((match = pattern.exec(code)) !== null) {
        let matchStart, matchEnd;
        
        if (rule.captureGroup !== undefined) {
          const capturedText = match[rule.captureGroup];
          const fullMatch = match[0];
          const offsetInMatch = fullMatch.indexOf(capturedText);
          
          matchStart = match.index + offsetInMatch;
          matchEnd = matchStart + capturedText.length;
        } else {
          matchStart = match.index;
          matchEnd = match.index + match[0].length;
        }

        allMatches.push({
          start: matchStart,
          end: matchEnd,
          className: rule.class,
          priority: rule.priority || 0
        });
      }
    }

    allMatches.sort((a, b) => {
      if (a.start !== b.start) return a.start - b.start;
      if (a.priority !== b.priority) return b.priority - a.priority;
      return (b.end - b.start) - (a.end - a.start);
    });

    const parts = [];
    let lastIndex = 0;
    const usedClasses = new Set();

    for (const match of allMatches) {
      if (match.start >= lastIndex) {
        if (match.start > lastIndex) {
          parts.push(this.escapeHtml(code.substring(lastIndex, match.start)));
        }

        const text = code.substring(match.start, match.end);
        parts.push(`<span class="ds-${match.className}">${this.escapeHtml(text)}</span>`);
        usedClasses.add(match.className);

        lastIndex = match.end;
      }
    }

    if (lastIndex < code.length) {
      parts.push(this.escapeHtml(code.substring(lastIndex)));
    }

    if (config.background) {
      element.style.backgroundColor = config.background;
    }

    element.innerHTML = parts.join('');
    element.classList.add('ds-highlighted');
    
    // Track used classes for later checking
    for (const className of usedClasses) {
      if (!this.warnedClasses.has(className)) {
        this.checkCSSClass(className);
      }
    }
  }

  escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  /**
   * Determine if line numbers should be shown for a given language
   */
  shouldShowLineNumbers(langAlias) {
    const canonical = this.resolveLanguage(langAlias);
    const config = this.configs[canonical];
    
    // If config explicitly sets showLineNumbers, use that value
    if (config && config.showLineNumbers !== undefined) {
      return config.showLineNumbers;
    }
    
    // Otherwise use global default
    return this.globalShowLineNumbers;
  }

  wrapCodeBlock(element, langAlias) {
    const filename = element.getAttribute('filename');
    const noDownload = element.hasAttribute('noDownload');
    const code = element.textContent;

    const wrapper = document.createElement('div');
    wrapper.className = 'ds-wrapper';

    const titleBar = document.createElement('div');
    titleBar.className = 'ds-titlebar';

    const titleLeft = document.createElement('div');
    titleLeft.className = 'ds-title-left';
    if (filename) {
      const fileCode = document.createElement('code');
      fileCode.textContent = filename;
      titleLeft.appendChild(fileCode);
    }

    const titleRight = document.createElement('div');
    titleRight.className = 'ds-title-right';

    if (!noDownload) {
      const downloadBtn = document.createElement('button');
      downloadBtn.className = 'ds-btn ds-download';
      downloadBtn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M7 10l5 5 5-5M12 15V3"/></svg>';
      downloadBtn.title = 'Download code';
      downloadBtn.onclick = () => this.downloadCode(code, filename || 'code.txt');
      titleRight.appendChild(downloadBtn);
    }

    const copyBtn = document.createElement('button');
    copyBtn.className = 'ds-btn ds-copy';
    copyBtn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"/></svg>';
    copyBtn.title = 'Copy code';
    copyBtn.onclick = () => this.copyCode(code, copyBtn);
    titleRight.appendChild(copyBtn);

    titleBar.appendChild(titleLeft);
    titleBar.appendChild(titleRight);

    const content = document.createElement('div');
    content.className = 'ds-content';

    // Check per-config line number setting
    const showLineNumbers = this.shouldShowLineNumbers(langAlias);
    const lines = code.split('\n');
    const lineNumbersDiv = document.createElement('div');
    lineNumbersDiv.className = 'ds-line-numbers';
    lineNumbersDiv.setAttribute('aria-hidden', 'true');

    for (let i = 1; i <= lines.length; i++) {
      const lineNum = document.createElement('span');
      // If showLineNumbers is false, create empty spans to maintain margin
      lineNum.textContent = showLineNumbers ? i : '';
      lineNumbersDiv.appendChild(lineNum);
    }

    content.appendChild(lineNumbersDiv);
    element.classList.add('ds-has-line-numbers');

    element.parentNode.insertBefore(wrapper, element);
    wrapper.appendChild(titleBar);
    wrapper.appendChild(content);
    content.appendChild(element);
  }

  updateLineNumbers(element, code, langAlias) {
    const showLineNumbers = this.shouldShowLineNumbers(langAlias);
    
    if (!showLineNumbers) return false;

    const wrapper = element.closest('.ds-wrapper');
    if (!wrapper) return false;

    const lineNumbersDiv = wrapper.querySelector('.ds-line-numbers');
    if (!lineNumbersDiv) return false;

    const currentLineCount = lineNumbersDiv.children.length;
    const newLines = code.split('\n').length;

    if (newLines > currentLineCount) {
      const fragment = document.createDocumentFragment();
      for (let i = currentLineCount + 1; i <= newLines; i++) {
        const lineNum = document.createElement('span');
        lineNum.textContent = i;
        fragment.appendChild(lineNum);
      }
      lineNumbersDiv.appendChild(fragment);
    } else if (newLines < currentLineCount) {
      while (lineNumbersDiv.children.length > newLines) {
        lineNumbersDiv.lastChild.remove();
      }
    }
    
    return true;
  }

  updateContent(element, code, langAlias) {
    if (!element) return;
    
    element.textContent = code;
    this.updateLineNumbers(element, code, langAlias);
    element.classList.add('ds-needs-highlight');
  }

  highlightThrottled(element, langAlias, wait = 100) {
    if (!this._throttleTimers) this._throttleTimers = new Map();
    
    const key = element;
    
    if (this._throttleTimers.has(key)) {
      return;
    }
    
    const timer = setTimeout(() => {
      this.highlight(element, langAlias);
      this._throttleTimers.delete(key);
    }, wait);
    
    this._throttleTimers.set(key, timer);
  }

  copyCode(code, button) {
    navigator.clipboard.writeText(code).then(() => {
      const originalHTML = button.innerHTML;
      button.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="20 6 9 17 4 12"/></svg>';
      button.classList.add('ds-success');
      setTimeout(() => {
        button.innerHTML = originalHTML;
        button.classList.remove('ds-success');
      }, 2000);
    }).catch(err => {
      console.error('Failed to copy:', err);
    });
  }

  downloadCode(code, filename) {
    const blob = new Blob([code], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
  }

  getRequiredLanguages() {
    const elements = document.querySelectorAll('pre[code]');
    const languages = new Set();
    for (const el of elements) {
      const langAlias = el.getAttribute('code');
      if (langAlias) {
        languages.add(this.resolveLanguage(langAlias));
      }
    }
    return Array.from(languages);
  }

  async init() {
    if (this.initialized) return;

    if (this.aliasLoadPromise) {
      await this.aliasLoadPromise;
    }

    // Check if a theme is loaded
    this.checkThemeLoaded();

    const requiredLangs = this.getRequiredLanguages();
    if (requiredLangs.length === 0) {
      this.initialized = true;
      return;
    }

    await Promise.all(
      requiredLangs.map(lang => this.loadLanguage(lang).catch(() => false))
    );

    const elements = document.querySelectorAll('pre[code]');
    for (const el of elements) {
      const langAlias = el.getAttribute('code');
      const canonical = this.resolveLanguage(langAlias);
      if (this.configs[canonical]) {
        this.wrapCodeBlock(el, langAlias);
        this.highlight(el, langAlias);
      }
    }

    this.initialized = true;
    
    // Report any missing classes at the end (only once)
    if (this.missingClasses.size > 0) {
      console.warn(`DarkSyntax: Missing CSS classes in theme: ${Array.from(this.missingClasses).map(c => 'ds-' + c).join(', ')}`);
    }
  }

  async highlightElement(element) {
    const langAlias = element.getAttribute('code');
    if (!langAlias) return;

    const canonical = this.resolveLanguage(langAlias);
    if (!this.configs[canonical]) {
      await this.loadLanguage(langAlias);
    }

    this.wrapCodeBlock(element, langAlias);
    this.highlight(element, langAlias);
  }

  async refresh() {
    this.initialized = false;
    await this.init();
  }
}

const darkSyntax = new DarkSyntax();

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => darkSyntax.init());
} else {
  darkSyntax.init();
}

if (typeof module !== 'undefined' && module.exports) {
  module.exports = darkSyntax;
} else {
  window.darkSyntax = darkSyntax;
}