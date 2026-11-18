// This file is a part of Julia. License is MIT: https://julialang.org/license

// Asynchronous unwinding checks for the tests of the assembly in src/arch and
// cli/arch: run code so that a callback sees the thread's state at every
// instruction it executes in given functions, as a profiler's signal could,
// and unwind from there with the unwind info and by frame pointers.
//
// On x86, the trap flag stops the thread after every instruction (but not
// before the first instruction after a signal returns, which instead is
// replaced by a breakpoint). Elsewhere, each instruction is replaced in turn
// by a trap, and the code is run again until it reaches it; the code must be
// safe to abandon at any instruction and run again.

#ifndef JL_ARCHSTEP_H
#define JL_ARCHSTEP_H

#include "archtest.h"

#if defined(__linux__) && (defined(__x86_64__) || defined(__i386__) || defined(__aarch64__) || \
                           defined(__arm__) || (defined(__riscv) && __riscv_xlen == 64) || \
                           (defined(__powerpc64__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__))
#define HAVE_ARCHSTEP 1

#include <elf.h>
#include <fcntl.h>
#include <link.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <ucontext.h>
#include <unistd.h>

// The registers of an interrupted context
#if defined(__x86_64__)
#define UC_PC(uc) ((uc)->uc_mcontext.gregs[REG_RIP])
#define UC_SP(uc) ((uc)->uc_mcontext.gregs[REG_RSP])
#define UC_FP(uc) ((uc)->uc_mcontext.gregs[REG_RBP])
#elif defined(__i386__)
#define UC_PC(uc) ((uc)->uc_mcontext.gregs[REG_EIP])
#define UC_SP(uc) ((uc)->uc_mcontext.gregs[REG_ESP])
#define UC_FP(uc) ((uc)->uc_mcontext.gregs[REG_EBP])
#elif defined(__aarch64__)
#define UC_PC(uc) ((uc)->uc_mcontext.pc)
#define UC_SP(uc) ((uc)->uc_mcontext.sp)
#define UC_FP(uc) ((uc)->uc_mcontext.regs[29])
#elif defined(__arm__)
#define UC_PC(uc) ((uc)->uc_mcontext.arm_pc)
#define UC_SP(uc) ((uc)->uc_mcontext.arm_sp)
#define UC_FP(uc) ((uc)->uc_mcontext.arm_fp)
#elif defined(__riscv)
#define UC_PC(uc) ((uc)->uc_mcontext.__gregs[REG_PC])
#define UC_SP(uc) ((uc)->uc_mcontext.__gregs[REG_SP])
#define UC_FP(uc) ((uc)->uc_mcontext.__gregs[REG_S0])
#elif defined(__powerpc64__)
#define UC_PC(uc) ((uc)->uc_mcontext.gp_regs[32])
#define UC_SP(uc) ((uc)->uc_mcontext.gp_regs[1])
#define UC_FP(uc) ((uc)->uc_mcontext.gp_regs[1])
#endif

typedef struct {
    const char *name;
    uintptr_t lo, hi; // the function's code, from its symbol
    int thumb;
    int samples;      // instructions sampled
    int entry_sampled;
    // data in the code (such as ARM literal pools), from the mapping symbols
    uintptr_t data_lo[8], data_hi[8];
    int ndata;
} step_fn_t;

// Find a function's code in this executable's symbol table (unlike dladdr,
// this also finds hidden and static functions, and their sizes).
static int bias_cb(struct dl_phdr_info *info, size_t size, void *data)
{
    (void)size;
    *(uintptr_t*)data = info->dlpi_addr; // the executable comes first
    return 1;
}

static int step_find_function(step_fn_t *fn, const char *name)
{
    memset(fn, 0, sizeof(*fn));
    fn->name = name;
    int fd = open("/proc/self/exe", O_RDONLY);
    if (fd < 0)
        return 0;
    off_t size = lseek(fd, 0, SEEK_END);
    const char *file = (const char*)mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (file == MAP_FAILED)
        return 0;
    uintptr_t bias = 0;
    dl_iterate_phdr(bias_cb, &bias);
    const ElfW(Ehdr) *eh = (const ElfW(Ehdr)*)file;
    const ElfW(Shdr) *sh = (const ElfW(Shdr)*)(file + eh->e_shoff);
    int found = 0;
    for (int i = 0; i < eh->e_shnum && !found; i++) {
        if (sh[i].sh_type != SHT_SYMTAB)
            continue;
        const ElfW(Sym) *syms = (const ElfW(Sym)*)(file + sh[i].sh_offset);
        const char *strs = file + sh[sh[i].sh_link].sh_offset;
        size_t n = sh[i].sh_size / sizeof(ElfW(Sym));
        for (size_t j = 0; j < n; j++) {
            if (ELF64_ST_TYPE(syms[j].st_info) == STT_FUNC && syms[j].st_size != 0 &&
                strcmp(strs + syms[j].st_name, name) == 0) {
                uintptr_t addr = bias + syms[j].st_value;
#if defined(__arm__)
                fn->thumb = addr & 1;
                addr &= ~(uintptr_t)1;
#endif
                fn->lo = addr;
                fn->hi = addr + syms[j].st_size;
                found = 1;
                break;
            }
        }
    }
    // mapping symbols: `$d` starts data, `$a`, `$t` and `$x` code
    for (int i = 0; i < eh->e_shnum && found; i++) {
        if (sh[i].sh_type != SHT_SYMTAB)
            continue;
        const ElfW(Sym) *syms = (const ElfW(Sym)*)(file + sh[i].sh_offset);
        const char *strs = file + sh[sh[i].sh_link].sh_offset;
        size_t n = sh[i].sh_size / sizeof(ElfW(Sym));
        for (size_t j = 0; j < n && fn->ndata < 8; j++) {
            const char *sym = strs + syms[j].st_name;
            uintptr_t addr = bias + syms[j].st_value;
            if (sym[0] != '$' || sym[1] != 'd' || (sym[2] != 0 && sym[2] != '.') ||
                addr < fn->lo || addr >= fn->hi)
                continue;
            uintptr_t end = fn->hi;
            for (size_t k = 0; k < n; k++) {
                const char *other = strs + syms[k].st_name;
                uintptr_t at = bias + syms[k].st_value;
                if (other[0] == '$' && (other[1] == 'a' || other[1] == 't' || other[1] == 'x') &&
                    (other[2] == 0 || other[2] == '.') && at > addr && at < end)
                    end = at;
            }
            fn->data_lo[fn->ndata] = addr;
            fn->data_hi[fn->ndata] = end;
            fn->ndata++;
        }
    }
    munmap((void*)file, size);
    if (!found)
        fprintf(stderr, "%s: not found in the symbol table\n", name);
    return found;
}

// The callback for each sample: the interrupted context, and the function it
// was interrupted in.
typedef void (*step_sample_t)(ucontext_t *uc, step_fn_t *fn);

static step_fn_t *step_fns;
static int step_nfns;
static step_sample_t step_sample;
static sigjmp_buf step_back;
static char step_altstack[256 * 1024] __attribute__((aligned(16)));

#if defined(__x86_64__) || defined(__i386__)
// Every instruction traps (single-step) while the trap flag is set; the
// handler runs with it clear, and sigreturn sets it again.
static void step_handler(int sig, siginfo_t *info, void *_uc)
{
    (void)sig; (void)info;
    ucontext_t *uc = (ucontext_t*)_uc;
    uintptr_t pc = UC_PC(uc);
    for (int i = 0; i < step_nfns; i++) {
        if (pc >= step_fns[i].lo && pc < step_fns[i].hi) {
            step_fns[i].samples++;
            step_fns[i].entry_sampled |= pc == step_fns[i].lo;
            step_sample(uc, &step_fns[i]);
        }
    }
}

// int3 traps after itself: put the instruction back and sample as if
// interrupted before it.
static step_fn_t *step_bp_fn;
static unsigned char step_bp_saved;
static void step_bp_handler(int sig, siginfo_t *info, void *_uc)
{
    (void)sig; (void)info;
    ucontext_t *uc = (ucontext_t*)_uc;
    if (UC_PC(uc) - 1 != step_bp_fn->lo) {
        fprintf(stderr, "unexpected trap at %p\n", (void*)UC_PC(uc));
        abort();
    }
    *(volatile unsigned char*)step_bp_fn->lo = step_bp_saved;
    UC_PC(uc) = step_bp_fn->lo;
    step_bp_fn->samples++;
    step_bp_fn->entry_sampled = 1;
    step_sample(uc, step_bp_fn);
    siglongjmp(step_back, 1);
}

static inline void step_set_tf(int on)
{
#if defined(__x86_64__)
    if (on)
        __asm__ volatile ("pushfq\n\torq $0x100, (%%rsp)\n\tpopfq" ::: "memory", "cc");
    else
        __asm__ volatile ("pushfq\n\tandq $~0x100, (%%rsp)\n\tpopfq" ::: "memory", "cc");
#else
    if (on)
        __asm__ volatile ("pushfl\n\torl $0x100, (%%esp)\n\tpopfl" ::: "memory", "cc");
    else
        __asm__ volatile ("pushfl\n\tandl $~0x100, (%%esp)\n\tpopfl" ::: "memory", "cc");
#endif
}

// For code in the run that must not be stepped through (it may block the
// trap's signal, as libunwind does).
static inline void step_pause(void)
{
    step_set_tf(0);
}
static inline void step_resume(void)
{
    step_set_tf(1);
}

// Run `run` once, sampling every instruction executed in `fns`.
static void step_functions(step_fn_t *fns, int nfns, void (*run)(void), step_sample_t sample)
{
    step_fns = fns; step_nfns = nfns; step_sample = sample;
    stack_t ss = { .ss_sp = step_altstack, .ss_size = sizeof(step_altstack), .ss_flags = 0 };
    sigaltstack(&ss, NULL);
    struct sigaction act, old;
    memset(&act, 0, sizeof(act));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = step_handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigaction(SIGTRAP, &act, &old);
    step_set_tf(1);
    run();
    step_set_tf(0);
    // a function entered by a signal return was not sampled at its entry
    act.sa_sigaction = step_bp_handler;
    sigaction(SIGTRAP, &act, NULL);
    long page = sysconf(_SC_PAGESIZE);
    for (int i = 0; i < nfns; i++) {
        step_fn_t *fn = &fns[i];
        if (fn->samples == 0 || fn->entry_sampled)
            continue;
        uintptr_t lo = fn->lo & ~(uintptr_t)(page - 1);
        mprotect((void*)lo, fn->hi - lo, PROT_READ | PROT_WRITE | PROT_EXEC);
        step_bp_fn = fn;
        step_bp_saved = *(volatile unsigned char*)fn->lo;
        *(volatile unsigned char*)fn->lo = 0xcc; // int3
        if (!sigsetjmp(step_back, 1))
            run();
        *(volatile unsigned char*)fn->lo = step_bp_saved;
    }
    sigaction(SIGTRAP, &old, NULL);
}
#else
// Each instruction in turn is replaced by a trap; reaching it samples, puts
// the instruction back and abandons the run.
#if defined(__aarch64__)
static const uint32_t step_trap = 0xd4200000;    // brk #0
#elif defined(__arm__)
static const uint32_t step_trap = 0xe7f000f0;    // udf #0 (ARM)
static const uint16_t step_trap_thumb = 0xde00;  // udf #0 (Thumb)
#elif defined(__riscv)
static const uint16_t step_trap = 0x9002;        // c.ebreak
#elif defined(__powerpc64__)
static const uint32_t step_trap = 0x7fe00008;    // trap
#endif
static uintptr_t step_at;
static step_fn_t *step_fn;
static unsigned char step_saved[4];
static size_t step_len;

static size_t step_insn_len(const step_fn_t *fn, uintptr_t pc)
{
#if defined(__arm__)
    if (fn->thumb) {
        uint16_t h = *(const uint16_t*)pc;
        return (h >> 11) >= 0x1d ? 4 : 2;
    }
#elif defined(__riscv)
    (void)fn;
    return (*(const uint16_t*)pc & 3) == 3 ? 4 : 2;
#endif
    (void)fn; (void)pc;
    return 4;
}

static void step_patch(uintptr_t pc, const void *bytes, size_t len)
{
    memcpy((void*)pc, bytes, len);
    __builtin___clear_cache((char*)pc, (char*)pc + len);
}

static void step_handler(int sig, siginfo_t *info, void *_uc)
{
    (void)sig; (void)info;
    ucontext_t *uc = (ucontext_t*)_uc;
    if (UC_PC(uc) != step_at) {
        fprintf(stderr, "unexpected trap at %p\n", (void*)UC_PC(uc));
        abort();
    }
    step_patch(step_at, step_saved, step_len);
    step_fn->samples++;
    step_sample(uc, step_fn);
    siglongjmp(step_back, 1);
}

static inline void step_pause(void) {}
static inline void step_resume(void) {}

static void step_functions(step_fn_t *fns, int nfns, void (*run)(void), step_sample_t sample)
{
    step_fns = fns; step_nfns = nfns; step_sample = sample;
    stack_t ss = { .ss_sp = step_altstack, .ss_size = sizeof(step_altstack), .ss_flags = 0 };
    sigaltstack(&ss, NULL);
    struct sigaction act, old_trap, old_ill;
    memset(&act, 0, sizeof(act));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = step_handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigaction(SIGTRAP, &act, &old_trap);
    sigaction(SIGILL, &act, &old_ill);
    long page = sysconf(_SC_PAGESIZE);
    for (int i = 0; i < nfns; i++) {
        step_fn_t *fn = &fns[i];
        uintptr_t lo = fn->lo & ~(uintptr_t)(page - 1);
        mprotect((void*)lo, fn->hi - lo, PROT_READ | PROT_WRITE | PROT_EXEC);
        for (uintptr_t pc = fn->lo; pc < fn->hi; pc += step_insn_len(fn, pc)) {
            int data = 0;
            for (int d = 0; d < fn->ndata; d++)
                if (pc >= fn->data_lo[d] && pc < fn->data_hi[d])
                    data = 1;
            if (data)
                continue;
            step_at = pc;
            step_fn = fn;
            const void *trap = &step_trap;
            step_len = sizeof(step_trap);
#if defined(__arm__)
            if (fn->thumb) {
                trap = &step_trap_thumb;
                step_len = sizeof(step_trap_thumb);
            }
#endif
            memcpy(step_saved, (void*)pc, step_len);
            step_patch(pc, trap, step_len);
            if (!sigsetjmp(step_back, 1))
                run();
            // not reached in this run: the instruction is not executed
            if (memcmp((void*)pc, step_saved, step_len) != 0)
                step_patch(pc, step_saved, step_len);
        }
    }
    sigaction(SIGTRAP, &old_trap, NULL);
    sigaction(SIGILL, &old_ill, NULL);
}
#endif

// Unwinding from an interrupted context. Each returns the return addresses of
// the frames above the interrupted one (as in archtest.h), or -1 if the
// unwinder faulted.
static sigjmp_buf step_fault_back;
static volatile int step_faulted;
static void step_fault_handler(int sig, siginfo_t *info, void *uc)
{
    (void)sig; (void)info; (void)uc;
    step_faulted = 1;
    siglongjmp(step_fault_back, 1);
}

static int step_guarded(void (*f)(ucontext_t*, backtrace_t*), ucontext_t *uc, backtrace_t *bt)
{
    struct sigaction act, old_segv, old_bus;
    memset(&act, 0, sizeof(act));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = step_fault_handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_NODEFER;
    sigaction(SIGSEGV, &act, &old_segv);
    sigaction(SIGBUS, &act, &old_bus);
    step_faulted = 0;
    if (!sigsetjmp(step_fault_back, 0))
        f(uc, bt);
    sigaction(SIGSEGV, &old_segv, NULL);
    sigaction(SIGBUS, &old_bus, NULL);
    return step_faulted ? -1 : 0;
}

// With the unwind info, by libgcc: unwind through the signal frame into the
// interrupted code, and keep the frames after the interrupted one. (Not on
// ARM, where its unwinder (EHABI) cannot unwind through a signal frame, nor
// on PowerPC, where it does not (through qemu's, at least), nor on RISC-V,
// where it does not end the call chain at an undefined return address.)
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
#define HAVE_STEP_UNWIND 1
static void step_unwind_(ucontext_t *uc, backtrace_t *bt)
{
    backtrace_t all;
    get_backtrace(&all);
    int i = backtrace_index(&all, UC_PC(uc));
    bt->n = 0;
    if (i < 0) {
        bt->n = -1; // the interrupted frame was not found
        return;
    }
    for (int j = i + 1; j < all.n; j++) {
        bt->ips[bt->n] = all.ips[j];
        bt->cfas[bt->n++] = all.cfas[j];
    }
    bt->ended = all.ended;
}
static int step_unwind(ucontext_t *uc, backtrace_t *bt)
{
    if (step_guarded(step_unwind_, uc, bt) < 0)
        return -1;
    return bt->n;
}
#endif

// With the unwind info, by libunwind, as Julia's profiler unwinds a thread it
// has interrupted (src/stackwalk.c): from the interrupted context, keeping
// each frame's own stack pointer, and whether the unwinder ended the call
// chain (rather than gave up on it).
// (Not on ARM, where libunwind cannot unwind from the prologue of C code.)
#if defined(ARCHTEST_LIBUNWIND) && !defined(__arm__)
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#define HAVE_STEP_LIBUNWIND 1
static unw_context_t *step_unw_context(ucontext_t *uc)
{
    return (unw_context_t*)uc;
}
static void step_libunwind_(ucontext_t *uc, backtrace_t *bt)
{
    unw_cursor_t cursor;
    bt->n = 0;
    bt->ended = 0;
    if (unw_init_local2(&cursor, step_unw_context(uc), UNW_INIT_SIGNAL_FRAME) != 0) {
        bt->n = -1;
        return;
    }
    int r;
    while ((r = unw_step(&cursor)) > 0 && bt->n < (int)(sizeof(bt->ips) / sizeof(bt->ips[0]))) {
        unw_word_t ip, sp;
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0 || unw_get_reg(&cursor, UNW_REG_SP, &sp) < 0)
            return;
        bt->ips[bt->n] = ip;
        bt->cfas[bt->n] = 0;
        bt->sps[bt->n++] = sp;
    }
    bt->ended = r == 0 || bt->n == (int)(sizeof(bt->ips) / sizeof(bt->ips[0]));
}
static int step_libunwind(ucontext_t *uc, backtrace_t *bt)
{
    if (step_guarded(step_libunwind_, uc, bt) < 0)
        return -1;
    return bt->n;
}
// The frames above the caller of this, by libunwind.
static __attribute__((noinline)) void step_libunwind_here(backtrace_t *bt)
{
    unw_context_t ctx;
    unw_cursor_t cursor;
    bt->n = 0;
    unw_getcontext(&ctx);
    if (unw_init_local(&cursor, &ctx) != 0 || unw_step(&cursor) <= 0)
        return;
    int r;
    while ((r = unw_step(&cursor)) > 0 && bt->n < (int)(sizeof(bt->ips) / sizeof(bt->ips[0]))) {
        unw_word_t ip, sp;
        unw_get_reg(&cursor, UNW_REG_IP, &ip);
        unw_get_reg(&cursor, UNW_REG_SP, &sp);
        bt->ips[bt->n] = ip;
        bt->cfas[bt->n] = 0;
        bt->sps[bt->n++] = sp;
    }
    bt->ended = r == 0;
}
#endif

// By frame pointers, starting from the interrupted frame pointer.
static uintptr_t step_ok_lo[2], step_ok_hi[2];
// The memory the frame-pointer walk may read: the main thread's stack, and
// another (such as a new task's) if any. (From the C library rather than from
// pthread_getattr_np, which reports the emulator's own stack under qemu.)
extern void *__libc_stack_end;
static void step_set_stacks(uintptr_t other_lo, uintptr_t other_hi)
{
    struct rlimit lim;
    uintptr_t size = 8 << 20;
    if (getrlimit(RLIMIT_STACK, &lim) == 0 && lim.rlim_cur != RLIM_INFINITY && lim.rlim_cur < (64 << 20))
        size = lim.rlim_cur;
    // the stack ends a little above __libc_stack_end (the arguments and
    // environment), which the walk never needs
    step_ok_hi[0] = (uintptr_t)__libc_stack_end;
    step_ok_lo[0] = step_ok_hi[0] - size;
    step_ok_lo[1] = other_lo;
    step_ok_hi[1] = other_hi;
}
static int step_fp_ok(uintptr_t p)
{
    for (int i = 0; i < 2; i++)
        if (p >= step_ok_lo[i] && p + 2 * sizeof(void*) <= step_ok_hi[i])
            return 1;
    return 0;
}
static void step_fp_unwind_(ucontext_t *uc, backtrace_t *bt)
{
    uintptr_t fp = UC_FP(uc);
    bt->n = 0;
    while (fp != 0 && bt->n < (int)(sizeof(bt->ips) / sizeof(bt->ips[0]))) {
        uintptr_t next, ra, rec;
#if defined(__riscv)
        rec = fp - 2 * sizeof(void*);
#elif defined(__arm__)
        rec = fp - sizeof(void*);
#else
        rec = fp;
#endif
        // as frame-pointer unwinders do, stop at a link that is not into a
        // stack (past main, into C library code that keeps no frame records)
        if (!step_fp_ok(rec))
            break;
#if defined(__riscv)
        next = ((uintptr_t*)rec)[0];
        ra = ((uintptr_t*)rec)[1];
#elif defined(__arm__)
        next = ((uintptr_t*)rec)[0];
        ra = ((uintptr_t*)rec)[1];
#elif defined(__powerpc64__)
        next = ((uintptr_t*)rec)[0];
        if (next == 0)
            break;
        if (!step_fp_ok(next))
            break;
        ra = ((uintptr_t*)next)[2];
#else
        next = ((uintptr_t*)rec)[0];
        ra = ((uintptr_t*)rec)[1];
#endif
        if (ra == 0)
            break;
#if defined(__arm__)
        ra &= ~(uintptr_t)1;
#endif
        bt->ips[bt->n++] = ra;
        fp = next;
    }
}
static int step_fp_unwind(ucontext_t *uc, backtrace_t *bt)
{
    if (step_guarded(step_fp_unwind_, uc, bt) < 0)
        return -1;
    return bt->n;
}

// The frames an unwinder should find above the interrupted one, as a range of
// return addresses for each (one address, or any into a function).
typedef struct {
    uintptr_t lo[16], hi[16];
    uintptr_t cfa[16]; // and the CFA below it (as in backtrace_t), if known (not 0)
    uintptr_t sp[16];  // and its own stack pointer, if known (not 0)
    int n;
    int open; // more frames may follow (the chain was cut short)
} step_chain_t;

static void step_chain_add_frame(step_chain_t *c, uintptr_t lo, uintptr_t hi, uintptr_t cfa, uintptr_t sp)
{
    if (c->n == 16) {
        c->open = 1;
        return;
    }
    c->lo[c->n] = lo;
    c->hi[c->n] = hi;
    c->cfa[c->n] = cfa;
    c->sp[c->n] = sp;
    c->n++;
}
static void step_chain_add(step_chain_t *c, uintptr_t lo, uintptr_t hi)
{
    step_chain_add_frame(c, lo, hi, 0, 0);
}
// The frames that an unwinder found, as expected for unwinding by the same
// unwinder from below them.
static void step_chain_add_found(step_chain_t *c, const backtrace_t *bt, int from)
{
    for (int i = from; i < bt->n && bt->ips[i] != 0; i++)
        step_chain_add_frame(c, bt->ips[i], bt->ips[i] + 1, bt->cfas[i], bt->sps[i]);
}
static void step_chain_add_ip(step_chain_t *c, uintptr_t ip)
{
    step_chain_add(c, ip, ip + 1);
}
// Any return address into a function: after a call that does not return, it
// can be the end of the function.
static void step_chain_add_return_into(step_chain_t *c, const step_fn_t *fn)
{
    step_chain_add(c, fn->lo + 1, fn->hi + 1);
}
static int step_chain_has(const step_chain_t *c, int i, uintptr_t ip)
{
    return i < c->n && ip >= c->lo[i] && ip < c->hi[i];
}

// The frames found must be the first frames of `expected`, at least `need` of
// them (or all), followed only by a null return address if anything: a frame
// not in `expected`, out of order, or with another stack pointer (a CFA below
// it unwound wrongly, even if to the right return address) is not really there.
// The unwinder must also have ended the chain, rather than given up on it.
static int step_chain_ok(const backtrace_t *bt, const step_chain_t *expected, int need)
{
    if (bt->n < 0 || !bt->ended)
        return 0;
    int n = bt->n;
    if (n > 0 && bt->ips[n - 1] == 0)
        n--;
    if (n < need && n < expected->n)
        return 0;
    for (int i = 0; i < n; i++) {
        if (i >= expected->n) {
            if (!expected->open)
                return 0;
            continue;
        }
        if (!step_chain_has(expected, i, bt->ips[i]))
            return 0;
        if (expected->cfa[i] != 0 && bt->cfas[i] != 0 && bt->cfas[i] != expected->cfa[i])
            return 0;
        if (expected->sp[i] != 0 && bt->sps[i] != 0 && bt->sps[i] != expected->sp[i])
            return 0;
    }
    return 1;
}

// A frame-pointer walk may skip frames that have not set up their frame
// record (a leaf, or a function interrupted in its prologue), but must not
// make any up.
static int step_fp_chain_ok(const backtrace_t *bt, const step_chain_t *expected)
{
    if (bt->n < 0)
        return 0;
    int j = 0;
    for (int i = 0; i < bt->n; i++) {
        while (j < expected->n && !step_chain_has(expected, j, bt->ips[i]))
            j++;
        if (j == expected->n)
            return expected->open;
        j++;
    }
    return 1;
}

// Which unwinders the samples are checked with
static void step_print_unwinders(const char *test)
{
    printf("%s: unwinding checked with", test);
#if defined(HAVE_STEP_LIBUNWIND)
    printf(" libunwind,");
#elif !defined(__arm__)
    printf(" (not libunwind: pass LIBUNWIND_PREFIX),");
#endif
#ifdef HAVE_STEP_UNWIND
    printf(" libgcc,");
#endif
#ifdef HAVE_FP_BACKTRACE
    printf(" frame pointers");
#else
    printf(" (not frame pointers)");
#endif
    printf("\n");
}

static void step_print_expected(const step_chain_t *c)
{
    fprintf(stderr, "    expected:");
    for (int i = 0; i < c->n; i++) {
        if (c->hi[i] == c->lo[i] + 1)
            fprintf(stderr, " %p", (void*)c->lo[i]);
        else
            fprintf(stderr, " (%p-%p]", (void*)(c->lo[i] - 1), (void*)(c->hi[i] - 1));
        if (c->cfa[i])
            fprintf(stderr, "@%p", (void*)c->cfa[i]);
        if (c->sp[i])
            fprintf(stderr, "@sp=%p", (void*)c->sp[i]);
    }
    fprintf(stderr, "%s\n", c->open ? " ..." : "");
}

static void step_print_chain(const char *what, const backtrace_t *bt, int cfas)
{
    fprintf(stderr, "    %s:", what);
    if (bt->n < 0)
        fprintf(stderr, " (the unwinder faulted, or did not reach the interrupted frame)");
    for (int i = 0; i < bt->n; i++) {
        fprintf(stderr, " %p", (void*)bt->ips[i]);
        if (cfas && bt->cfas[i])
            fprintf(stderr, "@%p", (void*)bt->cfas[i]);
        if (cfas && bt->sps[i])
            fprintf(stderr, "@sp=%p", (void*)bt->sps[i]);
    }
    if (cfas && bt->n >= 0 && !bt->ended)
        fprintf(stderr, " (the unwinder gave up)");
    fprintf(stderr, "\n");
}

#endif
#endif
