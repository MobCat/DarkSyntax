// darkSyntax/configs/quakec.js - QuakeC language configuration
// ==============================================================
// QuakeC (1996)
// Game scripting language for Quake engine
//
// Configs
// =======================
// ALIASES: ['quakec', 'qc']
// File extensions: .qc
// Compiles to: progs.dat (bytecode)
//
// QUAKEC SYNTAX NOTES
// ===================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Created by John Carmack at id Software (1996)
// - First scripting language for a major 3D game engine
// - Shipped with Quake source code (July 1996)
// - Pioneered the concept of mod-friendly game engines
// - Compiled to bytecode (progs.dat) via qcc compiler
// - Enabled legendary mods: Team Fortress, Capture the Flag, Rocket Arena
// - Engine source released under GPL (1999)
// - Quake II and later id games dropped QuakeC for compiled C/C++ DLLs
// - Inspired UnrealScript (1998) and other game scripting languages
//
// INFLUENCED
// ----------
// - UnrealScript (1998) - Epic's game scripting for Unreal Engine
// - Half-Life modding (1998) - Used C++ DLLs but inspired by QuakeC's philosophy
// - Quake III VM (1999) - Evolved to LCC-compiled C bytecode
// - Modern game scripting - Unity C#, Unreal Blueprints, Lua in games
// - Modding culture - Established downloadable mods as industry standard
//
// USED FOR
// --------
// - Game logic in Quake (1996) - weapons, AI, physics, items
// - Quake mods and total conversions
// - Multiplayer game modes (CTF, Team Fortress, etc.)
// - Custom weapons and entities
// - AI behavior and pathfinding
// - Trigger systems and level scripting
// - Server-side game rules
//
// KEY FEATURES
// ------------
// - C-like syntax but heavily restricted
// - Only 4 types: float, vector, string, entity
// - No structs or custom types (except entity fields)
// - No arrays in original (added in later compilers)
// - No pointers (entity is reference type)
// - Compiled to bytecode for portability
// - Built-in functions for game engine access
// - Entity-field system for game objects
// - Frame functions for animation
// - Maximum 8 function parameters
//
// CORE SYNTAX
// -----------
// Types:
//   float health = 100;              // Also used for integers and booleans
//   vector pos = '128 256 64';       // 3D coordinates (note single quotes!)
//   string modelname = "progs/player.mdl";
//   entity target = self;            // Reference to game object
//
// Entity Fields (like struct members):
//   .float health;                   // Declared with dot prefix
//   .vector origin;                  // All entities have these fields
//   .entity enemy;
//   self.health = 100;               // Access via entity reference
//
// Functions:
//   void() PlayerJump = {            // void() = no params, no return
//       ...
//   };
//   
//   float(entity targ) CanDamage = { // float return, entity param
//       ...
//       return TRUE;
//   };
//
// Frame Functions (for animation):
//   void() player_run1 = [$run1, player_run2] {
//       ...
//   };
//
// Built-in Functions (defined in engine):
//   spawn();                         // Create entity
//   remove(entity e);                // Delete entity
//   setorigin(entity e, vector org); // Move entity
//   makevectors(vector ang);         // Calculate direction vectors
//
// QUIRKS
// ------
// - **Vector literals use single quotes**: '0 0 0' NOT "0 0 0"
//   * Strings use double quotes, vectors use single quotes
//   * Easy to confuse!
// - **No arrays in original QuakeC**: Must use numbered variables
//   * weapon1, weapon2, weapon3... instead of weapon[3]
//   * Later compilers (FTEQCC, gmqcc) added arrays
// - **Only 4 types, period**: Can't define structs or classes
//   * Everything is float, vector, string, or entity
//   * No typedef, no struct, no union (until modern compilers)
// - **Entity fields are global**: .float health declared once, all entities have it
//   * Like adding columns to a database table
//   * Can't have per-entity-type fields in original
// - **Temp string buffer hell**: Built-ins return temp strings
//   * Can only hold ONE string at a time
//   * Can't do: centerprint(ftos(x) + ftos(y)); // BREAKS!
//   * Must use temp variables for each string
// - **Self pointer madness**: Most code operates on 'self' entity
//   * Functions assume they work on self
//   * Must save/restore self when calling on other entities
// - **No nested function calls**: Single parameter area
//   * Can't do: func1(func2(x)); // BREAKS!
//   * Must split into temp = func2(x); func1(temp);
// - **Negative constant parsing bug**: "a-5" breaks
//   * Parser sees "a", then "-5" (negative constant)
//   * Must write "a - 5" with spaces
// - **Think functions**: Entities "think" every frame
//   * self.think = MyFunction; // Called next frame
//   * self.nextthink = time + 0.1; // When to think
// - **No type checking**: Float used for int, bool, flags
//   * TRUE and FALSE are just float constants
//   * Bitflags stored in floats
// - **Maximum 8 parameters**: Hard limit in VM
//   * More than 8? You're out of luck
// - **Global variable stomping**: Single return area
//   * Functions can't be called inside other function calls
//   * Results get overwritten
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "It's like C, but worse in every way" - Every QuakeC programmer
// - "Why can't I have arrays?!" - Every QuakeC beginner
// - "Remember: vectors use single quotes!" - Most common advice
// - "The self pointer is your friend... until it isn't" - QuakeC wisdom
// - "QuakeC: where 'vector' means '3 floats' and pain means learning" - Unknown
//
// NOTES ON QUAKEC SYNTAX
// ----------------------
// - C-style // and /* */ comments
// - Case-sensitive
// - Semicolon-terminated statements
// - Entity fields start with dot: .float health;
// - Vectors use single quotes: '64 128 256'
// - Built-in functions from engine (spawn, remove, etc.)
// - Frame functions for animations: [$frame, nextframe]
// - Global 'self' entity for current context
// - Global 'other' entity for collision/touch events
// - Global 'world' entity (entity 0, the map itself)


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('quakec', {
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
    
    // PRIORITY 90: Strings
    {
      class: 'string',
      pattern: /"(?:[^"\\]|\\.)*"/g,
      priority: 90
    },
    
    // PRIORITY 85: Vector literals (single quotes!)
    {
      class: 'string',
      pattern: /'[^']*'/g,
      priority: 85
    },
    
    // PRIORITY 75: Built-in functions (engine-defined)
    {
      class: 'builtin',
      pattern: /\b(spawn|remove|setorigin|setmodel|setsize|makevectors|vectoangles|vectoyaw|sound|ambientsound|centerprint|sprint|bprint|dprint|traceline|droptofloor|checkbottom|pointcontents|changelevel|stuffcmd|setspawnparms|find|nextent|findradius|walkmove|movetogoal|makestatic)\b/g,
      priority: 75
    },
    
    // PRIORITY 70: Keywords
    {
      class: 'keyword',
      pattern: /\b(if|else|while|do|return|local|var)\b/g,
      priority: 70
    },
    
    // PRIORITY 65: Types
    {
      class: 'keyword',
      pattern: /\b(void|float|vector|string|entity)\b/g,
      priority: 65
    },
    
    // PRIORITY 60: Entity field declarations (.float, .vector, etc.)
    {
      class: 'decorator',
      pattern: /\.(float|vector|string|entity|void)\s+[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 60
    },
    
    // PRIORITY 55: Global entity references
    {
      class: 'builtin',
      pattern: /\b(self|other|world|time|frametime|activator)\b/g,
      priority: 55
    },
    
    // PRIORITY 50: Constants
    {
      class: 'boolean',
      pattern: /\b(TRUE|FALSE|NULL)\b/g,
      priority: 50
    },
    
    // PRIORITY 45: Common entity fields (when accessing)
    {
      class: 'variable',
      pattern: /\b(origin|velocity|angles|model|frame|health|weapon|ammo_\w+|enemy|goalentity|movetype|solid|touch|think|nextthink|use)\b/g,
      priority: 45
    },
    
    // PRIORITY 40: Function definitions
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*\{/g,
      captureGroup: 1,
      priority: 40
    },
    
    // PRIORITY 35: Function calls
    {
      class: 'function',
      pattern: /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?=\()/g,
      priority: 35
    },
    
    // PRIORITY 30: Numbers
    {
      class: 'number',
      pattern: /\b\d+\.?\d*/g,
      priority: 30
    },
    
    // PRIORITY 25: Built-in constants (movement types, solid types, etc.)
    {
      class: 'decorator',
      pattern: /\b(MOVETYPE_\w+|SOLID_\w+|CHAN_\w+|ATTN_\w+|CONTENT_\w+|DAMAGE_\w+|FL_\w+|VEC_\w+)\b/g,
      priority: 25
    },
    
    // PRIORITY 20: Frame names (preceded by $)
    {
      class: 'number',
      pattern: /\$[a-zA-Z_][a-zA-Z0-9_]*/g,
      priority: 20
    }
  ]
});