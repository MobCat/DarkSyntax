// darkSyntax/configs/x86-asm.js - x86/x64 Assembly language configuration
// =========================================================================
// x86/x64 Assembly Language (1978)
// Low-level symbolic programming language for Intel x86 architecture family
//
// Configs
// =======================
// ALIASES: ['x86-asm', 'x86', 'x64', 'nasm', 'gas', 'masm']
// File extensions: .asm, .s, .nasm, .masm
// Supports: Intel syntax, AT&T syntax, NASM, GAS, MASM
//
// X86/X64 ASSEMBLY LANGUAGE SYNTAX NOTES
// ======================================
//
// HISTORICAL SIGNIFICANCE
// -----------------------
// - Intel 8086 (1978) - 16-bit processor, introduced x86 architecture
// - Intel 80286 (1982) - Protected mode, 16-bit with 24-bit addressing
// - Intel 80386 (1985) - First 32-bit x86 processor (IA-32)
// - Intel Pentium (1993) - Superscalar architecture, MMX instructions (1997)
// - AMD64/x86-64 (2003) - 64-bit extension by AMD, adopted by Intel as EM64T
// - x86 became the dominant desktop/server architecture (1980s-present)
// - Backwards compatible: 64-bit x86 CPUs can still run 8086 code from 1978
// - Over 40 years of instruction set evolution while maintaining compatibility
//
// x86 assembly is specific to the Intel x86/AMD64 architecture family. Other
// processor architectures (ARM, MIPS, PowerPC, RISC-V) have completely different
// and incompatible assembly languages. This config covers x86 (16/32-bit) and
// x86-64 (64-bit) assembly syntax as used in NASM, MASM, and GAS assemblers.
//
// INFLUENCED
// ----------
// - x86 inline assembly - C (1978), C++ (1985), Rust (2015), Go (2009)
// - x86 JIT compilers - V8 JavaScript (2008), .NET CLR (2002), Java HotSpot (1999)
// - x86 disassemblers - IDA Pro (1990), Ghidra (2019), objdump (1988)
// - x86 emulators - QEMU (2003), Bochs (1994), DOSBox (2002)
// - x86 binary translation - Rosetta 2 (2020), WINE (1993)
// - Compiler backends - GCC x86 (1987), LLVM x86 (2003), MSVC (1985)
//
// USED FOR
// --------
// - Operating system kernels (Linux, Windows, BSD kernels)
// - Bootloaders (GRUB, Windows Boot Manager)
// - Device drivers and firmware
// - Performance-critical code sections (cryptography, codecs)
// - Embedded x86 systems (industrial PCs, legacy systems)
// - Reverse engineering and malware analysis
// - Learning x86 architecture and CPU internals
// - Exploit development and security research
// - Real-time systems requiring precise timing control
//
// KEY FEATURES
// ------------
// - CISC architecture: Complex Instruction Set Computer (vs RISC)
// - Variable-length instructions: 1 to 15 bytes per instruction
// - Rich instruction set: 1000+ instructions across all extensions
// - Backwards compatibility: Code from 1978 still runs on modern CPUs
// - Multiple addressing modes: Immediate, register, memory, indexed, etc.
// - Segment registers: Legacy from 8086, still present but rarely used in 64-bit
// - SIMD extensions: MMX (1997), SSE (1999), AVX (2011), AVX-512 (2016)
// - x87 FPU: Floating-point coprocessor integrated since 80486 (1989)
//
// CORE SYNTAX
// -----------
// Two main syntax families:
//
// Intel Syntax (NASM, MASM):
//   mov eax, 5          ; destination, source
//   add [ebx], eax      ; memory operations
//   call function_name
//
// AT&T Syntax (GAS):
//   movl $5, %eax       ; source, destination (reversed!)
//   addl %eax, (%ebx)   ; % for registers, $ for immediates
//   call function_name
//
// Common elements:
//   - Labels end with colon: main:, loop_start:
//   - Directives start with . (GAS) or uppercase (NASM)
//   - Comments: ; (Intel/NASM), # (AT&T/GAS)
//   - Size specifiers: byte, word, dword, qword
//
// QUIRKS
// ------
// - **Syntax wars**: Intel vs AT&T syntax creates endless confusion
//   * Intel: `mov eax, 5` (destination first)
//   * AT&T: `movl $5, %eax` (source first) - completely reversed!
// - **Register naming chaos**: Same register, 4 different names and sizes
//   * rax (64-bit), eax (32-bit), ax (16-bit), al/ah (8-bit)
// - **Instruction aliases**: Many instructions have multiple valid mnemonics
//   * je and jz are the same instruction (jump if equal/zero)
//   * sal and shl are identical (shift arithmetic/logical left)
// - **Assembler dialect hell**: NASM, MASM, GAS all have different syntax
//   * NASM: `section .data`, MASM: `SECTION DATA`, GAS: `.section .data`
// - **No type safety whatsoever**: Everything is just bytes
//   * Wrong size operand? Undefined behavior or crash
//   * mov byte [rax], 0x12345678 - silently truncates or fails
// - **Calling convention nightmare**: stdcall, cdecl, fastcall, System V AMD64 ABI...
//   * Windows x64 uses Microsoft x64 calling convention
//   * Linux x64 uses System V AMD64 ABI
//   * They're completely incompatible!
// - **Platform-specific chaos**: Windows vs Linux syscalls are totally different
//   * Linux: syscall number in rax
//   * Windows: different syscall mechanism entirely
// - **Documentation overload**: Intel manuals exceed 5,000 pages
//   * x86-64 has over 1,000 distinct instructions
//   * Some instructions have 50+ variants (mov has ~40!)
// - **Endianness**: x86 is little-endian (bytes stored backwards)
//   * 0x12345678 stored as: 78 56 34 12 in memory
// - **Flags register weirdness**: Instructions implicitly modify flags
//   * Can't always predict which flags an instruction changes
//   * cmp doesn't store result, only sets flags for next jump
// - **Segment hell**: Real mode uses segment:offset addressing
//   * Physical address = segment × 16 + offset
//   * Same physical address can have multiple segment:offset combinations
// - **CISC complexity**: Single instruction can do complex operations
//   * `rep movsb` copies entire memory blocks
//   * But makes CPU pipelining and optimization harder
//
// FAMOUS QUOTES & SAYINGS
// -----------------------
// - "Real programmers can write assembly code in any language." - Larry Wall
// - "Any program is open source, if you can read assembly."
// - "There are 10 types of people: those who understand x86 assembly and those who don't."
// - "mov is Turing complete" - Referring to MOV is Turing-complete paper (2013)
// - "x86 is the cockroach of instruction sets - it will survive anything." - Linus Torvalds (paraphrased)
//
// NOTES ON X86/X64 ASSEMBLY SYNTAX
// ---------------------------------
// - Comments use ; (Intel/NASM) or # (AT&T/GAS)
// - Supports both Intel and AT&T syntax
// - Labels end with colon (:)
// - Directives start with . (GAS) or are uppercase (NASM)
// - Registers: 64-bit (rax), 32-bit (eax), 16-bit (ax), 8-bit (al/ah)
// - SIMD registers: xmm (128-bit), ymm (256-bit), zmm (512-bit)
// - Hex: 0x prefix or h suffix
// - Binary: 0b prefix or b suffix
// - Memory addressing in square brackets [rax+8]
//


// LANGUAGE SYNTAX CONFIGURATION FOR DARKSYNTAX
// =============================================
darkSyntax.registerLanguage('x86-asm', {
  rules: [
    // PRIORITY 100: Comments
    {
      class: 'comment',
      pattern: /;.*$/gm,  // Semicolon comments (Intel/NASM)
      priority: 100
    },
    {
      class: 'comment',
      pattern: /#.*$/gm,  // Hash comments (AT&T/GAS)
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\/\/.*$/gm,  // C-style comments (some assemblers)
      priority: 100
    },
    {
      class: 'comment',
      pattern: /\/\*[\s\S]*?\*\//g,  // C-style multi-line
      priority: 100
    },
    
    // PRIORITY 90: Strings
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
    
    // PRIORITY 80: Labels (must come before instructions)
    {
      class: 'function',
      pattern: /^[a-zA-Z_\.][a-zA-Z0-9_\.]*:/gm,
      priority: 80
    },
    
    // PRIORITY 70: Directives/Pseudo-ops
    {
      class: 'decorator',
      pattern: /\.(section|text|data|bss|globl|global|extern|byte|word|long|quad|ascii|asciz|string|align|space|comm|equ|macro|endm|include|if|ifdef|ifndef|else|endif|set)/gi,
      priority: 70
    },
    {
      class: 'decorator',
      pattern: /\b(SECTION|SEGMENT|ENDS|DB|DW|DD|DQ|DT|RESB|RESW|RESD|RESQ|REST|INCBIN|EQU|TIMES|GLOBAL|EXTERN|BITS|ORG|ALIGN)\b/g,
      priority: 70
    },
    
    // PRIORITY 60: Registers - 64-bit
    {
      class: 'builtin',
      pattern: /\b(rax|rbx|rcx|rdx|rsi|rdi|rbp|rsp|r8|r9|r10|r11|r12|r13|r14|r15|rip)\b/gi,
      priority: 60
    },
    
    // PRIORITY 60: Registers - 32-bit
    {
      class: 'builtin',
      pattern: /\b(eax|ebx|ecx|edx|esi|edi|ebp|esp|r8d|r9d|r10d|r11d|r12d|r13d|r14d|r15d|eip)\b/gi,
      priority: 60
    },
    
    // PRIORITY 60: Registers - 16-bit
    {
      class: 'builtin',
      pattern: /\b(ax|bx|cx|dx|si|di|bp|sp|r8w|r9w|r10w|r11w|r12w|r13w|r14w|r15w|ip|cs|ds|es|fs|gs|ss)\b/gi,
      priority: 60
    },
    
    // PRIORITY 60: Registers - 8-bit
    {
      class: 'builtin',
      pattern: /\b(al|ah|bl|bh|cl|ch|dl|dh|sil|dil|bpl|spl|r8b|r9b|r10b|r11b|r12b|r13b|r14b|r15b)\b/gi,
      priority: 60
    },
    
    // PRIORITY 55: XMM/YMM/ZMM registers (SIMD)
    {
      class: 'builtin',
      pattern: /\b(xmm[0-9]|xmm1[0-5]|ymm[0-9]|ymm1[0-5]|zmm[0-9]|zmm1[0-5]|zmm[12][0-9]|zmm3[01])\b/gi,
      priority: 55
    },
    
    // PRIORITY 50: Instructions - Data movement
    {
      class: 'keyword',
      pattern: /\b(mov|movb|movw|movl|movq|movabs|movsx|movsxd|movzx|lea|push|pop|xchg|cmov|cmova|cmovae|cmovb|cmovbe|cmovc|cmove|cmovg|cmovge|cmovl|cmovle|cmovna|cmovnae|cmovnb|cmovnbe|cmovnc|cmovne|cmovng|cmovnge|cmovnl|cmovnle|cmovno|cmovnp|cmovns|cmovnz|cmovo|cmovp|cmovpe|cmovpo|cmovs|cmovz)\b/gi,
      priority: 50
    },
    
    // PRIORITY 50: Instructions - Arithmetic
    {
      class: 'keyword',
      pattern: /\b(add|sub|mul|imul|div|idiv|inc|dec|neg|adc|sbb|cmp|test|and|or|xor|not|shl|shr|sal|sar|rol|ror|rcl|rcr)\b/gi,
      priority: 50
    },
    
    // PRIORITY 50: Instructions - Control flow
    {
      class: 'keyword',
      pattern: /\b(jmp|je|jz|jne|jnz|jg|jnle|jge|jnl|jl|jnge|jle|jng|ja|jnbe|jae|jnb|jb|jnae|jbe|jna|jo|jno|js|jns|jp|jpe|jnp|jpo|jc|jnc|call|ret|retn|retf|iret|loop|loope|loopz|loopne|loopnz|enter|leave)\b/gi,
      priority: 50
    },
    
    // PRIORITY 50: Instructions - Stack and string operations
    {
      class: 'keyword',
      pattern: /\b(pusha|pushad|popa|popad|pushf|pushfd|popf|popfd|movs|movsb|movsw|movsd|movsq|cmps|cmpsb|cmpsw|cmpsd|cmpsq|scas|scasb|scasw|scasd|scasq|lods|lodsb|lodsw|lodsd|lodsq|stos|stosb|stosw|stosd|stosq|rep|repe|repz|repne|repnz)\b/gi,
      priority: 50
    },
    
    // PRIORITY 50: Instructions - System and misc
    {
      class: 'keyword',
      pattern: /\b(nop|hlt|int|syscall|sysenter|sysexit|sysret|cpuid|rdtsc|rdtscp|pause|clc|stc|cmc|cld|std|cli|sti|lahf|sahf|cbw|cwde|cdqe|cwd|cdq|cqo|xlat|in|out|ins|outs)\b/gi,
      priority: 50
    },
    
    // PRIORITY 50: SSE/AVX instructions (common ones)
    {
      class: 'keyword',
      pattern: /\b(movaps|movups|movapd|movupd|movdqa|movdqu|movss|movsd|addps|addpd|addss|addsd|subps|subpd|subss|subsd|mulps|mulpd|mulss|mulsd|divps|divpd|divss|divsd|sqrtps|sqrtpd|sqrtss|sqrtsd|andps|andpd|orps|orpd|xorps|xorpd|cmpps|cmppd|cmpss|cmpsd|minps|minpd|minss|minsd|maxps|maxpd|maxss|maxsd|cvtps2pd|cvtpd2ps|cvtss2sd|cvtsd2ss)\b/gi,
      priority: 50
    },
    
    // PRIORITY 30: Size specifiers
    {
      class: 'keyword',
      pattern: /\b(byte|word|dword|qword|tbyte|ptr)\b/gi,
      priority: 30
    },
    
    // PRIORITY 25: Hexadecimal numbers
    {
      class: 'number',
      pattern: /\b0x[0-9A-Fa-f]+\b/g,
      priority: 25
    },
    {
      class: 'number',
      pattern: /\b[0-9A-Fa-f]+h\b/gi,  // Intel hex notation (e.g., 0FFh)
      priority: 25
    },
    
    // PRIORITY 24: Binary numbers
    {
      class: 'number',
      pattern: /\b0b[01]+\b/g,
      priority: 24
    },
    {
      class: 'number',
      pattern: /\b[01]+b\b/gi,  // Intel binary notation
      priority: 24
    },
    
    // PRIORITY 20: Decimal numbers
    {
      class: 'number',
      pattern: /\b\d+\b/g,
      priority: 20
    },
    
    // PRIORITY 15: Memory references with brackets
    {
      class: 'string',
      pattern: /\[[^\]]+\]/g,  // [rax], [rsp+8], etc.
      priority: 15
    }
  ]
});