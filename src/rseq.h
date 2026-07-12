// This file is a part of Julia. License is MIT: https://julialang.org/license

// Restartable-sequence (Linux rseq(2)) support.
//
// A restartable sequence is a short, contiguous instruction range registered with
// the kernel through a per-thread `struct rseq` area: if the thread is preempted,
// migrated, or takes a signal while its instruction pointer is inside the range,
// the kernel redirects it to an abort handler instead of resuming mid-sequence.
// Everything before the range's final instruction must therefore be re-runnable;
// the final instruction is the single committing store. Paired with
// MEMBARRIER_CMD_PRIVATE_EXPEDITED_RSEQ -- which issues the usual expedited memory
// barrier *and* aborts the in-progress restartable sequence of every thread of the
// process -- this gives an asymmetric fence whose slow side never has to wait for a
// preempted fast side: an aborted sequence simply restarts and re-reads whatever it
// guards.
//
// The first consumer is the global-binding re-type protocol (#62154): the commit
// windows of jl_binding_begin_commit / emit_binding_commit_begin are semantically
// restartable sequences (re-runnable flag/world checks followed by one committing
// store), so on capable systems the runtime store paths -- and the compiled
// fast-path Set commits, which typed_store emits as the same inline-asm critical
// section behind a runtime dispatch on the thread's registration (see
// emit_rseq_guarded_set in cgutils.cpp) -- run them as real rseq critical sections.
// jl_retype_flag_partitions then uses the RSEQ-flavored membarrier: a critical
// section in flight at fence time is aborted and its restart observes the just-set
// PARTITION_FLAG_RETYPE_WRITE, so the drain loop never has to wait out a
// *descheduled* window holder for these paths. The ptls->bnd_commit_window protocol remains for
// the compiled RMW commits (their trusted loads sit under safepoint-containing
// regions that an abort could not re-run) and for threads without a registration,
// so the drain stays; on rseq systems it just almost never has anything to wait
// for. The correctness argument is the one modeled in
// doc/src/devdocs/binding-retype/BindingRetype.tla, with the sequence abort playing
// the role of the drain for these threads: a commit that retired before the fence
// is visible to the re-declaration's validation, and one aborted by it re-reads the
// flag.
//
// Registration comes in two flavors, resolved per thread by jl_rseq_init_thread:
//  - glibc >= 2.35 registers an rseq area for every thread it creates; we detect it
//    through the (weak) __rseq_offset/__rseq_size symbols and use that area. glibc
//    registers with its architecture's canonical signature, which JL_RSEQ_SIG
//    matches, so our abort blocks validate against it.
//  - otherwise we self-register ptls->rseq_storage with the rseq syscall (the build
//    environment's old glibc has no rseq support, but the running kernel usually
//    does). The kernel unregisters automatically at thread exit; the storage lives
//    in ptls, which is never freed before then.
// Either way, a thread with ptls->rseq == NULL (old kernel, no RSEQ-flavored
// membarrier, JULIA_RSEQ=0, or an unsupported platform) must use the fallback
// protocol of its consumer. Critical sections additionally require hand-written,
// architecture-specific assembly, currently implemented for x86_64 and aarch64
// (JL_HAVE_RSEQ_CS below; on aarch64 the RMW commits need LSE single-instruction
// atomics and are additionally gated on HWCAP_ATOMICS at run time -- armv8.0's
// LL/SC loops cannot place a post-commit boundary directly after their conditional
// store). Other architectures fall back to the portable protocol.

#ifndef JL_RSEQ_H
#define JL_RSEQ_H

#ifdef __cplusplus
extern "C" {
#endif

// The per-architecture rseq signature (canonical values shared by the kernel
// selftests, librseq, and -- crucially -- glibc's own registration, whose area we
// reuse when present): the 4 bytes immediately preceding every abort label,
// validated by the kernel before it redirects execution there so that stray/forged
// rseq_cs descriptors cannot turn arbitrary code addresses into jump targets. On
// aarch64 the value encodes an (never-executed) `brk #0x45e0` so disassembly stays
// sane. JL_RSEQ_SIG_ASM must stay in sync: it is what the assembly blocks splice in
// front of their abort labels.
#if defined(_CPU_AARCH64_)
#define JL_RSEQ_SIG 0xd428bc00U
#define JL_RSEQ_SIG_ASM ".inst 0xd428bc00"
#else // x86 (and the default for future ports that use the generic signature)
#define JL_RSEQ_SIG 0x53053053U
#define JL_RSEQ_SIG_ASM ".long 0x53053053"
#endif

#if defined(_OS_LINUX_)

// Process-wide availability: kernel rseq + the RSEQ-flavored expedited membarrier
// (without which an in-flight critical section could not be fenced) + not disabled
// via JULIA_RSEQ=0. Computed once, before any thread registers.
int jl_rseq_process_enabled(void) JL_NOTSAFEPOINT;
// Resolve/perform this thread's registration (called from jl_init_threadtls).
void jl_rseq_init_thread(jl_ptls_t ptls) JL_NOTSAFEPOINT;
// The heavy side: expedited membarrier + abort every thread's in-progress critical
// section. Returns 0 on success, -1 if unavailable (callers fall back to
// jl_membarrier plus their drain). Implemented in signals-unix.c.
int jl_membarrier_rseq(void) JL_NOTSAFEPOINT;

#else

#define jl_rseq_init_thread(ptls) ((void)(ptls))

#endif // _OS_LINUX_

// Whether the calling thread can run rseq critical sections (for tests/diagnostics).
JL_DLLEXPORT int jl_rseq_available(void) JL_NOTSAFEPOINT;

#if defined(_OS_LINUX_) && (defined(_CPU_X86_64_) || defined(_CPU_AARCH64_))
#define JL_HAVE_RSEQ_CS 1

// Whether the RMW critical sections (xchg / cmpxchg commits) are available on this
// thread's hardware: always on x86_64; on aarch64 they require LSE
// single-instruction atomics (swpal / casal), detected once via HWCAP_ATOMICS
// (jl_rseq_lse, set in rseq.c before any thread registers). The plain-store commit
// has no such requirement.
#if defined(_CPU_AARCH64_)
extern int jl_rseq_lse;
#define jl_rseq_have_rmw() (jl_rseq_lse)
#else
#define jl_rseq_have_rmw() 1
#endif

// The guarded-commit critical sections used by the binding re-type protocol. Shape
// (matching jl_binding_begin_commit's check):
//
//     entry:  publish the __rseq_cs descriptor into the thread's rseq area
//     start:  load *flagw; bail if (word & mask) != 0
//             <commit instruction>           (store / xchg / lock cmpxchg)
//     post_commit:
//
// `flagw` is the `kind` word of the binding partition the stored value was validated
// against, and `mask` its PARTITION_FLAG_RETYPE_WRITE bit. A preemption, migration,
// signal, or RSEQ-flavored membarrier anywhere in [start, post_commit) restarts at
// `entry`, re-reading the flag. The bail path branches out of the range (the kernel
// lazily clears a stale rseq_cs whose range no longer contains the interrupted IP).
// Each function returns 1 when the commit instruction executed, 0 when the check
// diverted; nothing else runs inside the range, so the sections are safepoint-free
// by construction.
//
// Ordering: the flag is monotone (never cleared), so a relaxed load suffices -- the
// visibility cut that makes a clear read trustworthy is the re-declaration's IPI,
// not a pairing on the flag itself. The plain-store commit must still be a release
// (readers acquire the value slot): free on x86, stlr on aarch64; the RMW commits
// use the acquire-release LSE forms (swpal / casal).

#if defined(_CPU_X86_64_)

// Commit `val` into `*slot` (release). Mirrors jl_checked_assignment's fast path.
static inline int jl_rseq_guarded_store(jl_rseq_t *rs,
        _Atomic(size_t) *flagw, size_t mask,
        _Atomic(void*) *slot, void *val) JL_NOTSAFEPOINT
{
    int ok;
    uint64_t scratch;
    __asm__ __volatile__ (
        ".pushsection __rseq_cs, \"aw\"\n\t"
        ".balign 32\n\t"
        "3:\n\t"
        ".long 0x0, 0x0\n\t"            // version, flags
        ".quad 1f, (2f - 1f), 4f\n\t"   // start_ip, post_commit_offset, abort_ip
        ".popsection\n\t"
        "0:\n\t"
        "leaq 3b(%%rip), %[scratch]\n\t"
        "movq %[scratch], %[rseq_cs]\n\t"
        "1:\n\t"
        "movq %[flagw], %[scratch]\n\t"
        "testq %[mask], %[scratch]\n\t"
        "jnz 5f\n\t"
        "movq %[val], %[slot]\n\t"      // the commit
        "2:\n\t"
        "movl $1, %[ok]\n\t"
        "jmp 6f\n\t"
        JL_RSEQ_SIG_ASM "\n\t"           // validated by the kernel at abort_ip - 4
        "4:\n\t"
        "jmp 0b\n\t"                    // abort: re-publish the descriptor and retry
        "5:\n\t"
        "xorl %[ok], %[ok]\n\t"
        "6:\n\t"
        : [ok] "=&r" (ok), [scratch] "=&r" (scratch),
          [rseq_cs] "=m" (rs->rseq_cs),
          [slot] "=m" (*(void *volatile *)slot)
        : [flagw] "m" (*(volatile size_t *)flagw), [mask] "r" (mask),
          [val] "r" (val)
        : "memory", "cc");
    return ok;
}

// Exchange `*slot` with `*valp` (sequentially consistent; `xchg` with a memory
// operand has an implicit lock prefix). Mirrors jl_checked_swap's fast path; the
// displaced value is returned in `*valp`.
static inline int jl_rseq_guarded_xchg(jl_rseq_t *rs,
        _Atomic(size_t) *flagw, size_t mask,
        _Atomic(void*) *slot, void **valp) JL_NOTSAFEPOINT
{
    int ok;
    uint64_t scratch;
    void *val = *valp;
    __asm__ __volatile__ (
        ".pushsection __rseq_cs, \"aw\"\n\t"
        ".balign 32\n\t"
        "3:\n\t"
        ".long 0x0, 0x0\n\t"
        ".quad 1f, (2f - 1f), 4f\n\t"
        ".popsection\n\t"
        "0:\n\t"
        "leaq 3b(%%rip), %[scratch]\n\t"
        "movq %[scratch], %[rseq_cs]\n\t"
        "1:\n\t"
        "movq %[flagw], %[scratch]\n\t"
        "testq %[mask], %[scratch]\n\t"
        "jnz 5f\n\t"
        "xchgq %[val], %[slot]\n\t"     // the commit; old value lands in %[val]
        "2:\n\t"
        "movl $1, %[ok]\n\t"
        "jmp 6f\n\t"
        JL_RSEQ_SIG_ASM "\n\t"
        "4:\n\t"
        "jmp 0b\n\t"
        "5:\n\t"
        "xorl %[ok], %[ok]\n\t"
        "6:\n\t"
        : [ok] "=&r" (ok), [scratch] "=&r" (scratch), [val] "+r" (val),
          [rseq_cs] "=m" (rs->rseq_cs),
          [slot] "+m" (*(void *volatile *)slot)
        : [flagw] "m" (*(volatile size_t *)flagw), [mask] "r" (mask)
        : "memory", "cc");
    if (ok)
        *valp = val;
    return ok;
}

// Compare-exchange `*slot` from `*expectedp` to `newv` (sequentially consistent).
// Mirrors the cmpswap fast paths of jl_checked_replace/modify/assignonce. Returns 1
// when the compare-exchange instruction executed -- with `*successp` its outcome and
// `*expectedp` updated to the witnessed value on failure -- and 0 when the guard
// checks diverted (nothing executed).
static inline int jl_rseq_guarded_cmpxchg(jl_rseq_t *rs,
        _Atomic(size_t) *flagw, size_t mask,
        _Atomic(void*) *slot, void **expectedp, void *newv, int *successp) JL_NOTSAFEPOINT
{
    int ok;
    uint64_t scratch;
    uint8_t success = 0;
    void *expected = *expectedp;
    __asm__ __volatile__ (
        ".pushsection __rseq_cs, \"aw\"\n\t"
        ".balign 32\n\t"
        "3:\n\t"
        ".long 0x0, 0x0\n\t"
        ".quad 1f, (2f - 1f), 4f\n\t"
        ".popsection\n\t"
        "0:\n\t"
        "leaq 3b(%%rip), %[scratch]\n\t"
        "movq %[scratch], %[rseq_cs]\n\t"
        "1:\n\t"
        "movq %[flagw], %[scratch]\n\t"
        "testq %[mask], %[scratch]\n\t"
        "jnz 5f\n\t"
        "lock cmpxchgq %[newv], %[slot]\n\t"  // the commit; old value lands in %rax
        "2:\n\t"
        "sete %[succ]\n\t"
        "movl $1, %[ok]\n\t"
        "jmp 6f\n\t"
        JL_RSEQ_SIG_ASM "\n\t"
        "4:\n\t"
        "jmp 0b\n\t"
        "5:\n\t"
        "xorl %[ok], %[ok]\n\t"
        "6:\n\t"
        : [ok] "=&r" (ok), [scratch] "=&r" (scratch), [succ] "+r" (success),
          [expected] "+a" (expected),
          [rseq_cs] "=m" (rs->rseq_cs),
          [slot] "+m" (*(void *volatile *)slot)
        : [flagw] "m" (*(volatile size_t *)flagw), [mask] "r" (mask),
          [newv] "r" (newv)
        : "memory", "cc");
    if (ok) {
        *successp = success;
        if (!success)
            *expectedp = expected;
    }
    return ok;
}

#elif defined(_CPU_AARCH64_)

// Commit `val` into `*slot` (stlr, release). Available on all armv8 (no LSE
// requirement: the commit is a plain store-release).
static inline int jl_rseq_guarded_store(jl_rseq_t *rs,
        _Atomic(size_t) *flagw, size_t mask,
        _Atomic(void*) *slot, void *val) JL_NOTSAFEPOINT
{
    int ok;
    uint64_t scratch;
    __asm__ __volatile__ (
        ".pushsection __rseq_cs, \"aw\"\n\t"
        ".balign 32\n\t"
        "3:\n\t"
        ".long 0x0, 0x0\n\t"            // version, flags
        ".quad 1f, (2f - 1f), 4f\n\t"   // start_ip, post_commit_offset, abort_ip
        ".popsection\n\t"
        "0:\n\t"
        "adrp %[scratch], 3b\n\t"
        "add %[scratch], %[scratch], :lo12:3b\n\t"
        "str %[scratch], %[rseq_cs]\n\t"
        "1:\n\t"
        "ldr %[scratch], %[flagw]\n\t"
        "tst %[scratch], %[mask]\n\t"
        "b.ne 5f\n\t"
        "stlr %[val], [%[slotp]]\n\t"       // the commit (release)
        "2:\n\t"
        "mov %w[ok], #1\n\t"
        "b 6f\n\t"
        JL_RSEQ_SIG_ASM "\n\t"              // validated by the kernel at abort_ip - 4
        "4:\n\t"
        "b 0b\n\t"                          // abort: re-publish the descriptor and retry
        "5:\n\t"
        "mov %w[ok], #0\n\t"
        "6:\n\t"
        : [ok] "=&r" (ok), [scratch] "=&r" (scratch),
          [rseq_cs] "=Q" (rs->rseq_cs)
        : [flagw] "Q" (*(volatile size_t *)flagw), [mask] "r" (mask),
          [val] "r" (val), [slotp] "r" (slot)
        : "memory", "cc");
    return ok;
}

// Exchange `*slot` with `*valp` (swpal: acquire-release). Requires LSE
// (jl_rseq_have_rmw); the displaced value is returned in `*valp`.
static inline int jl_rseq_guarded_xchg(jl_rseq_t *rs,
        _Atomic(size_t) *flagw, size_t mask,
        _Atomic(void*) *slot, void **valp) JL_NOTSAFEPOINT
{
    int ok;
    uint64_t scratch;
    void *val = *valp;
    void *old;
    __asm__ __volatile__ (
        ".arch_extension lse\n\t"
        ".pushsection __rseq_cs, \"aw\"\n\t"
        ".balign 32\n\t"
        "3:\n\t"
        ".long 0x0, 0x0\n\t"
        ".quad 1f, (2f - 1f), 4f\n\t"
        ".popsection\n\t"
        "0:\n\t"
        "adrp %[scratch], 3b\n\t"
        "add %[scratch], %[scratch], :lo12:3b\n\t"
        "str %[scratch], %[rseq_cs]\n\t"
        "1:\n\t"
        "ldr %[scratch], %[flagw]\n\t"
        "tst %[scratch], %[mask]\n\t"
        "b.ne 5f\n\t"
        "swpal %[val], %[old], [%[slotp]]\n\t"  // the commit; displaced value in %[old]
        "2:\n\t"
        "mov %w[ok], #1\n\t"
        "b 6f\n\t"
        JL_RSEQ_SIG_ASM "\n\t"
        "4:\n\t"
        "b 0b\n\t"
        "5:\n\t"
        "mov %w[ok], #0\n\t"
        "6:\n\t"
        : [ok] "=&r" (ok), [scratch] "=&r" (scratch), [old] "=&r" (old),
          [rseq_cs] "=Q" (rs->rseq_cs)
        : [flagw] "Q" (*(volatile size_t *)flagw), [mask] "r" (mask),
          [val] "r" (val), [slotp] "r" (slot)
        : "memory", "cc");
    if (ok)
        *valp = old;
    return ok;
}

// Compare-exchange `*slot` from `*expectedp` to `newv` (casal: acquire-release).
// Requires LSE (jl_rseq_have_rmw). Returns 1 when the compare-exchange executed --
// with `*successp` its outcome and `*expectedp` updated to the witnessed value on
// failure -- and 0 when the guard checks diverted.
static inline int jl_rseq_guarded_cmpxchg(jl_rseq_t *rs,
        _Atomic(size_t) *flagw, size_t mask,
        _Atomic(void*) *slot, void **expectedp, void *newv, int *successp) JL_NOTSAFEPOINT
{
    int ok;
    uint64_t scratch;
    uint32_t success = 0;
    void *exp = *expectedp;
    void *origexp = exp;
    __asm__ __volatile__ (
        ".arch_extension lse\n\t"
        ".pushsection __rseq_cs, \"aw\"\n\t"
        ".balign 32\n\t"
        "3:\n\t"
        ".long 0x0, 0x0\n\t"
        ".quad 1f, (2f - 1f), 4f\n\t"
        ".popsection\n\t"
        "0:\n\t"
        "adrp %[scratch], 3b\n\t"
        "add %[scratch], %[scratch], :lo12:3b\n\t"
        "str %[scratch], %[rseq_cs]\n\t"
        "1:\n\t"
        "ldr %[scratch], %[flagw]\n\t"
        "tst %[scratch], %[mask]\n\t"
        "b.ne 5f\n\t"
        "casal %[exp], %[newv], [%[slotp]]\n\t" // the commit; witnessed value in %[exp]
        "2:\n\t"
        "cmp %[exp], %[origexp]\n\t"
        "cset %w[succ], eq\n\t"
        "mov %w[ok], #1\n\t"
        "b 6f\n\t"
        JL_RSEQ_SIG_ASM "\n\t"
        "4:\n\t"
        "b 0b\n\t"
        "5:\n\t"
        "mov %w[ok], #0\n\t"
        "6:\n\t"
        : [ok] "=&r" (ok), [scratch] "=&r" (scratch), [succ] "+r" (success),
          [exp] "+r" (exp),
          [rseq_cs] "=Q" (rs->rseq_cs)
        : [flagw] "Q" (*(volatile size_t *)flagw), [mask] "r" (mask),
          [newv] "r" (newv), [origexp] "r" (origexp), [slotp] "r" (slot)
        : "memory", "cc");
    if (ok) {
        *successp = (int)success;
        if (!success)
            *expectedp = exp;
    }
    return ok;
}

#endif // architecture

#endif // JL_HAVE_RSEQ_CS

#ifdef __cplusplus
}
#endif

#endif // JL_RSEQ_H
