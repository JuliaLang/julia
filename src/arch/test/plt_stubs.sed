# Turns the inline-asm templates of plt_stubs.inc into assembler macros taking
# their operands (`${N}`, and on x86 `${N:c}` and `${N:a}`) as op0, op1, op2.
/^\/\//d
/^$/d
s/^static const char \*\([a-z0-9_]*\) =$/.macro \1 op0, op1, op2/
s/\\n";/";/
s/^ *"\(.*\)\\n"$/\1/
s/^ *"\(.*\)";.*$/\1\n.endm/
s/\${\([0-9]\):a}/\\op\1\\()(%rip)/g
s/\${\([0-9]\)\(:c\)\{0,1\}}/\\op\1\\()/g
s/\$\$/$/g
