// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_setjmp / jl_longjmp for Windows (src/arch/<arch>/win_setjmp.S):
// the buffer must hold the callee-saved registers, stack pointer and return
// address at the offsets of _JUMP_BUFFER, which jl_simulate_longjmp
// (src/stackwalk.c) reads, and jl_longjmp must restore them. The restore is
// checked both through ordinary C code (outer) and register by register in
// assembly (roundtrip).

#include <setjmp.h>
#include <string.h>
#include "archtest.h"

#if defined(_WIN32) && (defined(__x86_64__) || defined(__i386__))

extern int __attribute__((returns_twice)) jl_setjmp(jmp_buf buf);
extern void __attribute__((noreturn)) jl_longjmp(jmp_buf buf, int val);
static uintptr_t layout_return_ip;

#if defined(__x86_64__)
// Call jl_setjmp with the callee-saved registers (except rbp, the frame
// pointer) set to known values. The C prologue aligns rsp, so calls from the
// inline assembly reserve only the 32 bytes of shadow space.
static __attribute__((noinline)) void setjmp_with_markers(jmp_buf buf)
{
    __asm__ volatile (
        "movq $0x1001, %%rbx\n\t"
        "movq $0x1002, %%rsi\n\t"
        "movq $0x1003, %%rdi\n\t"
        "movq $0x1004, %%r12\n\t"
        "movq $0x1005, %%r13\n\t"
        "movq $0x1006, %%r14\n\t"
        "movq $0x1007, %%r15\n\t"
        "movq $0x1106, %%rax\n\tmovq %%rax, %%xmm6\n\t"
        "movq $0x1107, %%rax\n\tmovq %%rax, %%xmm7\n\t"
        "movq $0x1108, %%rax\n\tmovq %%rax, %%xmm8\n\t"
        "movq $0x1109, %%rax\n\tmovq %%rax, %%xmm9\n\t"
        "movq $0x110a, %%rax\n\tmovq %%rax, %%xmm10\n\t"
        "movq $0x110b, %%rax\n\tmovq %%rax, %%xmm11\n\t"
        "movq $0x110c, %%rax\n\tmovq %%rax, %%xmm12\n\t"
        "movq $0x110d, %%rax\n\tmovq %%rax, %%xmm13\n\t"
        "movq $0x110e, %%rax\n\tmovq %%rax, %%xmm14\n\t"
        "movq $0x110f, %%rax\n\tmovq %%rax, %%xmm15\n\t"
        "movq %1, %%rcx\n\t"
        "subq $32, %%rsp\n\t"
        "call jl_setjmp\n\t"
        "movq -8(%%rsp), %%rax\n\t"
        "movq %%rax, %0\n\t"
        "addq $32, %%rsp\n\t"
        : "=m"(layout_return_ip) : "m"(buf)
        : "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11",
          "r12", "r13", "r14", "r15", "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5",
          "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13",
          "xmm14", "xmm15", "memory", "cc");
}

static void check_layout(jmp_buf buf)
{
    _JUMP_BUFFER *jb = (_JUMP_BUFFER*)buf;
    CHECK(jb->Rbx == 0x1001);
    CHECK(jb->Rsi == 0x1002);
    CHECK(jb->Rdi == 0x1003);
    CHECK(jb->R12 == 0x1004 && jb->R13 == 0x1005 && jb->R14 == 0x1006 && jb->R15 == 0x1007);
    const SETJMP_FLOAT128 *xmm = &jb->Xmm6;
    for (int i = 0; i < 10; i++)
        CHECK(xmm[i].Part[0] == 0x1106u + i);
    // Snapshot the return slot while the frame is live, and check call alignment.
    CHECK(jb->Rip != 0 && layout_return_ip == jb->Rip);
    CHECK((jb->Rsp & 15) == 8);
}

// Clobber the callee-saved registers, then jl_longjmp(buf, val).
__attribute__((noinline, noreturn, used)) void clobber_and_longjmp(jmp_buf buf, int val)
{
    __asm__ volatile (
        "movq %0, %%rcx\n\t"
        "movl %1, %%edx\n\t"
        "movq $-1, %%rbx\n\tmovq $-1, %%rsi\n\tmovq $-1, %%rdi\n\t"
        "movq $-1, %%r12\n\tmovq $-1, %%r13\n\tmovq $-1, %%r14\n\tmovq $-1, %%r15\n\t"
        "pcmpeqd %%xmm6, %%xmm6\n\tpcmpeqd %%xmm7, %%xmm7\n\tpcmpeqd %%xmm8, %%xmm8\n\t"
        "pcmpeqd %%xmm9, %%xmm9\n\tpcmpeqd %%xmm10, %%xmm10\n\tpcmpeqd %%xmm11, %%xmm11\n\t"
        "pcmpeqd %%xmm12, %%xmm12\n\tpcmpeqd %%xmm13, %%xmm13\n\tpcmpeqd %%xmm14, %%xmm14\n\t"
        "pcmpeqd %%xmm15, %%xmm15\n\t"
        "subq $32, %%rsp\n\t"
        "call jl_longjmp\n\t"
        "ud2\n\t"
        :: "m"(buf), "m"(val)
        : "rbx", "rcx", "rdx", "rsi", "rdi", "r12", "r13", "r14", "r15", "xmm6", "xmm7",
          "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", "memory");
    __builtin_unreachable();
}
#else
static __attribute__((noinline)) void setjmp_with_markers(jmp_buf buf)
{
    __asm__ volatile (
        "movl $0x1001, %%ebx\n\t"
        "movl $0x1002, %%esi\n\t"
        "movl $0x1003, %%edi\n\t"
        "pushl %1\n\t"
        "call _jl_setjmp\n\t"
        "movl -4(%%esp), %%eax\n\t"
        "movl %%eax, %0\n\t"
        "addl $4, %%esp\n\t"
        : "=m"(layout_return_ip) : "m"(buf)
        : "eax", "ebx", "ecx", "edx", "esi", "edi", "memory", "cc");
}

static void check_layout(jmp_buf buf)
{
    _JUMP_BUFFER *jb = (_JUMP_BUFFER*)buf;
    CHECK(jb->Ebx == 0x1001);
    CHECK(jb->Esi == 0x1002);
    CHECK(jb->Edi == 0x1003);
    CHECK(jb->Eip != 0 && layout_return_ip == jb->Eip);
}

__attribute__((noinline, noreturn, used)) void clobber_and_longjmp(jmp_buf buf, int val)
{
    __asm__ volatile (
        "movl %0, %%eax\n\t"
        "movl %1, %%ecx\n\t"
        "movl $-1, %%ebx\n\tmovl $-1, %%esi\n\tmovl $-1, %%edi\n\t"
        "pushl %%ecx\n\t"
        "pushl %%eax\n\t"
        "call _jl_longjmp\n\t"
        "ud2\n\t"
        :: "m"(buf), "m"(val)
        : "eax", "ebx", "ecx", "esi", "edi", "memory");
    __builtin_unreachable();
}
#endif

// jl_setjmp with markers in the callee-saved registers, jl_longjmp back from
// clobber_and_longjmp, which overwrites them, and record the registers as
// jl_setjmp returns the second time. (Values the compiler keeps in registers
// would not show this: it keeps nothing in registers across jl_setjmp.)
jmp_buf rt_buf;
int rt_val, rt_ret;
#if defined(__x86_64__)
uint64_t rt_regs[17]; // rbx, rsi, rdi, r12-r15, then xmm6-xmm15 (low halves)
static const uint64_t rt_expected[17] = {
    0x1001, 0x1002, 0x1003, 0x1004, 0x1005, 0x1006, 0x1007,
    0x1106, 0x1107, 0x1108, 0x1109, 0x110a, 0x110b, 0x110c, 0x110d, 0x110e, 0x110f};
static __attribute__((noinline)) int roundtrip(int val)
{
    rt_val = val;
    __asm__ volatile (
        "movq $0x1001, %%rbx\n\tmovq $0x1002, %%rsi\n\tmovq $0x1003, %%rdi\n\t"
        "movq $0x1004, %%r12\n\tmovq $0x1005, %%r13\n\tmovq $0x1006, %%r14\n\tmovq $0x1007, %%r15\n\t"
        "movq $0x1106, %%rax\n\tmovq %%rax, %%xmm6\n\t"
        "movq $0x1107, %%rax\n\tmovq %%rax, %%xmm7\n\t"
        "movq $0x1108, %%rax\n\tmovq %%rax, %%xmm8\n\t"
        "movq $0x1109, %%rax\n\tmovq %%rax, %%xmm9\n\t"
        "movq $0x110a, %%rax\n\tmovq %%rax, %%xmm10\n\t"
        "movq $0x110b, %%rax\n\tmovq %%rax, %%xmm11\n\t"
        "movq $0x110c, %%rax\n\tmovq %%rax, %%xmm12\n\t"
        "movq $0x110d, %%rax\n\tmovq %%rax, %%xmm13\n\t"
        "movq $0x110e, %%rax\n\tmovq %%rax, %%xmm14\n\t"
        "movq $0x110f, %%rax\n\tmovq %%rax, %%xmm15\n\t"
        "leaq rt_buf(%%rip), %%rcx\n\t"
        "subq $32, %%rsp\n\t"
        "call jl_setjmp\n\t"
        "addq $32, %%rsp\n\t"
        "testl %%eax, %%eax\n\t"
        "jnz 1f\n\t"
        "leaq rt_buf(%%rip), %%rcx\n\t"
        "movl rt_val(%%rip), %%edx\n\t"
        "subq $32, %%rsp\n\t"
        "call clobber_and_longjmp\n"
        "1:\n\t"
        "movl %%eax, rt_ret(%%rip)\n\t"
        "leaq rt_regs(%%rip), %%rax\n\t"
        "movq %%rbx, 0(%%rax)\n\tmovq %%rsi, 8(%%rax)\n\tmovq %%rdi, 16(%%rax)\n\t"
        "movq %%r12, 24(%%rax)\n\tmovq %%r13, 32(%%rax)\n\tmovq %%r14, 40(%%rax)\n\tmovq %%r15, 48(%%rax)\n\t"
        "movq %%xmm6, 56(%%rax)\n\tmovq %%xmm7, 64(%%rax)\n\tmovq %%xmm8, 72(%%rax)\n\t"
        "movq %%xmm9, 80(%%rax)\n\tmovq %%xmm10, 88(%%rax)\n\tmovq %%xmm11, 96(%%rax)\n\t"
        "movq %%xmm12, 104(%%rax)\n\tmovq %%xmm13, 112(%%rax)\n\tmovq %%xmm14, 120(%%rax)\n\t"
        "movq %%xmm15, 128(%%rax)\n\t"
        ::: "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11",
          "r12", "r13", "r14", "r15", "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5",
          "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13",
          "xmm14", "xmm15", "memory", "cc");
    for (int i = 0; i < 17; i++)
        CHECK(rt_regs[i] == rt_expected[i]);
    CHECK((((_JUMP_BUFFER*)rt_buf)->Rsp & 15) == 8);
    return rt_ret;
}
#else
uint32_t rt_regs[3]; // ebx, esi, edi
static __attribute__((noinline)) int roundtrip(int val)
{
    rt_val = val;
    __asm__ volatile (
        "movl $0x1001, %%ebx\n\tmovl $0x1002, %%esi\n\tmovl $0x1003, %%edi\n\t"
        "pushl $_rt_buf\n\t"
        "call _jl_setjmp\n\t"
        "addl $4, %%esp\n\t"
        "testl %%eax, %%eax\n\t"
        "jnz 1f\n\t"
        "pushl _rt_val\n\t"
        "pushl $_rt_buf\n\t"
        "call _clobber_and_longjmp\n"
        "1:\n\t"
        "movl %%eax, _rt_ret\n\t"
        "movl %%ebx, _rt_regs\n\tmovl %%esi, _rt_regs+4\n\tmovl %%edi, _rt_regs+8\n\t"
        ::: "eax", "ebx", "ecx", "edx", "esi", "edi", "memory", "cc");
    CHECK(rt_regs[0] == 0x1001 && rt_regs[1] == 0x1002 && rt_regs[2] == 0x1003);
    return rt_ret;
}
#endif

// The same property through the C ABI alone: outer() keeps values in
// callee-saved registers across its call to middle(), which calls jl_setjmp
// and then deep(). deep() uses many callee-saved registers itself, saving the
// caller's values in its prologue, and leaves by jl_longjmp, skipping its
// epilogue: only jl_longjmp's restore brings outer()'s values back. (deep()
// computes from different inputs, so that its registers hold different
// values.)
static jmp_buf nat_buf;
static volatile intptr_t nat_seed = 3, deep_seed = 1000;
static volatile double nat_fseed = 1.5, deep_fseed = 1000.5;
extern void opaque(volatile void *p);
__attribute__((noinline)) void opaque(volatile void *p) { (void)p; __asm__ volatile ("" ::: "memory"); }

static __attribute__((noinline, noreturn)) void deep(void)
{
    intptr_t a = deep_seed + 100, b = deep_seed + 200, c = deep_seed + 300, d = deep_seed + 400,
        e = deep_seed + 500, f = deep_seed + 600, g = deep_seed + 700, h = deep_seed + 800,
        i = deep_seed + 900, j = deep_seed + 1000, k = deep_seed + 1100, l = deep_seed + 1200;
    double x = deep_fseed + 100, y = deep_fseed + 200, z = deep_fseed + 300, u = deep_fseed + 400,
        v = deep_fseed + 500, w = deep_fseed + 600, p = deep_fseed + 700, q = deep_fseed + 800,
        r = deep_fseed + 900, s = deep_fseed + 1000;
    opaque(&a); // keep them live across a call: in all the callee-saved registers
    volatile double sink = a + b + c + d + e + f + g + h + i + j + k + l +
        x + y + z + u + v + w + p + q + r + s;
    (void)sink;
    jl_longjmp(nat_buf, 1);
}

static __attribute__((noinline)) int middle(void)
{
    if (jl_setjmp(nat_buf) == 0)
        deep();
    return 1;
}

static __attribute__((noinline)) int outer(void)
{
    intptr_t a = nat_seed * 3, b = nat_seed * 5, c = nat_seed * 7, d = nat_seed * 11,
        e = nat_seed * 13, f = nat_seed * 17, g = nat_seed * 19, h = nat_seed * 23,
        i = nat_seed * 29, j = nat_seed * 31, k = nat_seed * 37, l = nat_seed * 41;
    double x = nat_fseed * 3, y = nat_fseed * 5, z = nat_fseed * 7, u = nat_fseed * 11,
        v = nat_fseed * 13, w = nat_fseed * 17, p = nat_fseed * 19, q = nat_fseed * 23,
        r = nat_fseed * 29, s = nat_fseed * 31;
    middle();
    return a == 9 && b == 15 && c == 21 && d == 33 && e == 39 && f == 51 && g == 57 &&
        h == 69 && i == 87 && j == 93 && k == 111 && l == 123 &&
        x == 4.5 && y == 7.5 && z == 10.5 && u == 16.5 && v == 19.5 && w == 25.5 &&
        p == 28.5 && q == 34.5 && r == 43.5 && s == 46.5;
}

int main(void)
{
    jmp_buf layout;
    memset(layout, 0xa5, sizeof(layout));
    setjmp_with_markers(layout);
    check_layout(layout);
    CHECK(outer());
    CHECK(roundtrip(5) == 5);
    // as for longjmp, a value of 0 is returned as 1
    CHECK(roundtrip(0) == 1);
    ARCHTEST_DONE("win_setjmp");
}

#else

int main(void)
{
    printf("win_setjmp: skipped on this platform\n");
    return 0;
}

#endif
