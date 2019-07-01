// This file is a part of Julia. License is MIT: https://julialang.org/license

// #include'd from interpreter.c

// Backtrace support
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_) || defined(_OS_WINDOWS_)
extern uintptr_t __start_jl_interpreter_frame_val;
uintptr_t __start_jl_interpreter_frame = (uintptr_t)&__start_jl_interpreter_frame_val;
extern uintptr_t __stop_jl_interpreter_frame_val;
uintptr_t __stop_jl_interpreter_frame = (uintptr_t)&__stop_jl_interpreter_frame_val;

#define SECT_INTERP JL_SECTION("jl_interpreter_frame_val")
#if defined(_CPU_X86_) && defined(_OS_WINDOWS_)
#define MANGLE(x) "@" x "@8"
#else
#define MANGLE(x) x
#endif

#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
#if defined(_CPU_ARM_)
#  define ASM_FUNCTION_TYPE "%function"
#else
#  define ASM_FUNCTION_TYPE "@function"
#endif
#define ASM_ENTRY                                               \
    ".text\n"                                                   \
    ".p2align 4,0x90\n"                                         \
    ".global enter_interpreter_frame\n"                         \
    ".type enter_interpreter_frame," ASM_FUNCTION_TYPE "\n"
#if defined(_OS_LINUX_)
#define ASM_END ".previous\n"
#else
#define ASM_END
#endif
#else
#define ASM_ENTRY                               \
    ".text\n"                                   \
    ".globl enter_interpreter_frame\n"
#define ASM_END
#endif

#elif defined(_OS_DARWIN_)
extern uintptr_t __start_jl_interpreter_frame_val __asm("section$start$__TEXT$__jif");
uintptr_t __start_jl_interpreter_frame = (uintptr_t)&__start_jl_interpreter_frame_val;
extern uintptr_t __stop_jl_interpreter_frame_val __asm("section$end$__TEXT$__jif");
uintptr_t __stop_jl_interpreter_frame = (uintptr_t)&__stop_jl_interpreter_frame_val;

#define SECT_INTERP JL_SECTION("__TEXT,__jif")

#define MANGLE(x) "_" x
#define ASM_ENTRY \
    ".section __TEXT,__text,regular,pure_instructions\n" \
    ".globl _enter_interpreter_frame\n"
#define ASM_END ".previous"

#else
#define SECT_INTERP
#define NO_INTERP_BT
#warning "Interpreter backtraces not implemented for this platform"
#endif

#define STR(x) #x
#define XSTR(x) STR(x)

// This function is special. The unwinder looks for this function to find interpreter
// stack frames.
#ifdef _CPU_X86_64_

// Instructions: Make sure that MAX_INTERP_STATE_SIZE is a multiple of
//               alignof(struct interpreter_state) and larger than
//               sizeof(struct interpreter_state). Additionally, make sure that
//               MAX_INTERP_STATE_SIZE+STACK_PADDING+8 is a multiple of 16 to
//               ensure the proper stack alignment.
#define MAX_INTERP_STATE_SIZE 72
#define STACK_PADDING 0

static_assert(sizeof(interpreter_state) <= MAX_INTERP_STATE_SIZE, "Stack layout invariants violated.");
static_assert(MAX_INTERP_STATE_SIZE % alignof(interpreter_state) == 0, "Stack layout invariants violated");
static_assert(((MAX_INTERP_STATE_SIZE + STACK_PADDING + 8) % 16) == 0, "Stack layout invariants violated");

#ifdef _OS_WINDOWS_
size_t TOTAL_STACK_PADDING = STACK_PADDING + 32;
#else
size_t TOTAL_STACK_PADDING = STACK_PADDING;
#endif

asm(
    ASM_ENTRY
    MANGLE("enter_interpreter_frame") ":\n"
    ".cfi_startproc\n"
    "\tsubq $" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING)", %rsp\n"
    ".cfi_def_cfa_offset " XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING)" + 8\n"
#ifdef _OS_WINDOWS_
#define ARG1_REG "rcx"
#else
#define ARG1_REG "rdi"
#endif
    "\tmovq %" ARG1_REG ", %rax\n"
    "\tleaq " XSTR(STACK_PADDING) "(%rsp), %" ARG1_REG "\n"
    // Zero out the src and mi fields
    "\tmovq $0, 0(%" ARG1_REG ")\n"
    "\tmovq $0, 8(%" ARG1_REG ")\n"
#ifdef _OS_WINDOWS_
    // Make space for the register parameter area
    "\tsubq $32, %rsp\n"
#endif
    // The L here conviences the OS X linker not to terminate the unwind info early
    "Lenter_interpreter_frame_start_val:\n"
    "\tcallq *%rax\n"
    "Lenter_interpreter_frame_end_val:\n"
#ifdef _OS_WINDOWS_
    "\taddq $32, %rsp\n"
#endif
    "\taddq $" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING)", %rsp\n"
#ifndef _OS_DARWIN_
    // Somehow this throws off compact unwind info on OS X
    ".cfi_def_cfa_offset 8\n"
#endif
    "\tretq\n"
    ".cfi_endproc\n"
    ASM_END
    );

#define CALLBACK_ABI

#elif defined(_CPU_X86_)

#define MAX_INTERP_STATE_SIZE 36
#ifdef _OS_WINDOWS_
#define STACK_PADDING 4
#else
#define STACK_PADDING 8
#endif

size_t TOTAL_STACK_PADDING = STACK_PADDING;

static_assert(sizeof(interpreter_state) <= MAX_INTERP_STATE_SIZE, "Stack layout invariants violated");
static_assert(MAX_INTERP_STATE_SIZE % alignof(interpreter_state) == 0, "Stack layout invariants violated");
#ifndef _OS_WINDOWS_
static_assert((MAX_INTERP_STATE_SIZE + STACK_PADDING + 4) % 16 == 0, "Stack layout invariants violated");
#endif

asm(
     ASM_ENTRY
     MANGLE("enter_interpreter_frame") ":\n"
     ".cfi_startproc\n"
#ifdef _OS_WINDOWS_
/*
 * On win32, we set -mincoming-stack-boundary=2. This causes GCC to emit stack
 * realignment gadgets into the prologue of every function. Unfortunately for
 * us there are two different kinds of such gadgets and since we don't know
 * which one the target function is going to use, we can't use the same trick
 * as everywhere else. From https://gcc.gnu.org/ml/gcc/2007-12/msg00503.html,
 * the two prologues are:
 *
 *     pushl     %ebp
 *     movl      %esp, %ebp
 *     andl      $-16, %esp
 *
 * and
 *
 *     pushl     %edi                     // Save callee save reg edi
 *     leal      8(%esp), %edi            // Save address of parameter frame
 *     andl      $-16, %esp               // Align local stack
 *     pushl     $4(%edi)                 //  save return address
 *     pushl     %ebp                     //  save old ebp
 *     movl      %esp, %ebp               //  point ebp to pseudo frame
 *
 * From the perspective of the unwinder, the first case looks like a regular
 * (without the realignment gadget) stack frame. However, for the second one,
 * the compiler deliberately constructs a "fake stack frame" that has an
 * incorrect stack address for the previous frame. To work around all of this,
 * use ebp based addressing on win32
 */
#define FP_CAPTURE_OFFSET MAX_INTERP_STATE_SIZE
#define ENTRY_OFFSET 8
    "\tpushl %ebp\n"
    ".cfi_def_cfa_offset " XSTR(ENTRY_OFFSET)"\n"
    "\tmovl %esp, %ebp\n"
#else
#define ENTRY_OFFSET 4
#endif
     "\tsubl $" XSTR(MAX_INTERP_STATE_SIZE) ", %esp\n"
     ".cfi_def_cfa_offset " XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(ENTRY_OFFSET) "\n"
     "\tmovl %ecx, %eax\n"
     "\tmovl %esp, %ecx\n"
     // Zero out the src and mi fields
     "\tmovl $0, (%esp)\n"
     "\tmovl $0, 4(%esp)\n"
     // Restore 16 byte stack alignment
     // Technically not necessary on windows, because we don't assume this
     // alignment, but let's be nice if we ever start doing that.
     "\tsubl $" XSTR(STACK_PADDING) ", %esp\n"
     ".cfi_def_cfa_offset " XSTR(STACK_PADDING) " + " XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(ENTRY_OFFSET) "\n"
     "Lenter_interpreter_frame_start_val:\n"
     "\tcalll *%eax\n"
     "Lenter_interpreter_frame_end_val:\n"
     "\taddl $" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING) ", %esp\n"
#ifdef _OS_WINDOWS_
     ".cfi_def_cfa_offset 8\n"
     "\tpopl %ebp\n"
#endif
     ".cfi_def_cfa_offset 4\n"
     "\tret\n"
     ".cfi_endproc\n"
     ASM_END
     );

#define CALLBACK_ABI  __attribute__((fastcall))
static_assert(sizeof(interpreter_state) <= MAX_INTERP_STATE_SIZE, "Update assembly code above");

#elif defined(_CPU_AARCH64_)

#define MAX_INTERP_STATE_SIZE 64
#define STACK_PADDING 16

// Check that the interpreter state can fit
static_assert(sizeof(interpreter_state) <= MAX_INTERP_STATE_SIZE,
              "Stack layout invariants violated.");
// Check that the alignment of the type is satisfied
// (16 is stack alignment at function boundary)
static_assert(alignof(interpreter_state) <= 16, "Stack layout invariants violated");
static_assert(STACK_PADDING % alignof(interpreter_state) == 0,
              "Stack layout invariants violated");
// Check that ABI stack alignment requirement is maintained.
static_assert(((MAX_INTERP_STATE_SIZE + STACK_PADDING) % 16) == 0,
              "Stack layout invariants violated");
// Check that the padding is large enough for lr.
static_assert(STACK_PADDING >= sizeof(void*), "Stack layout invariants violated");

size_t TOTAL_STACK_PADDING = STACK_PADDING;

asm(
    ASM_ENTRY
    MANGLE("enter_interpreter_frame") ":\n"
    ".cfi_startproc\n"
    // Save lr
    "\tstr x30, [sp, #-(" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING) ")]!\n"
    "\t.cfi_def_cfa_offset (" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING) ")\n"
    "\t.cfi_offset 30, -(" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING) ")\n"
    "\tmov x2, x0\n"
    // Zero out the src and mi fields
    "\tstp xzr, xzr, [sp, " XSTR(STACK_PADDING) "]\n"
    "\tadd x0, sp, " XSTR(STACK_PADDING) "\n"
    "Lenter_interpreter_frame_start_val:\n"
    "\tblr x2\n"
    "Lenter_interpreter_frame_end_val:\n"
    "\tldr x30, [sp], (" XSTR(MAX_INTERP_STATE_SIZE) " + " XSTR(STACK_PADDING) ")\n"
    "\t.cfi_restore 30\n"
    "\t.cfi_def_cfa_offset 0\n"
    "\tret\n"
    ".cfi_endproc\n"
    ASM_END
    );

#define CALLBACK_ABI

#elif defined(_CPU_ARM_)

#define MAX_INTERP_STATE_SIZE 48

// Check that the interpreter state can fit
static_assert(sizeof(interpreter_state) <= MAX_INTERP_STATE_SIZE,
              "Stack layout invariants violated.");
// Check that the alignment of the type is satisfied
// (16 is what we realign the stack to)
static_assert(alignof(interpreter_state) <= 16, "Stack layout invariants violated");

size_t TOTAL_STACK_PADDING = 0;

asm(
    ASM_ENTRY
    MANGLE("enter_interpreter_frame") ":\n"
    ".fnstart\n"
    "\tpush {fp, lr}\n"
    "\t.save {fp, lr}\n"
    "\t.setfp fp, sp, #4\n"
    "\tadd fp, sp, #4\n"
    "\tmov r2, r0\n"
    // Reserve enough space and realign stack to 16bytes
    // The realignment is for consistency with every other architectures.
    // It isn't strictly necessary since we currently do not rely on it.
    "\tsub sp, sp, #" XSTR(MAX_INTERP_STATE_SIZE) "\n"
    "\tbic sp, sp, #15\n"
    // Zero out the src and mi field
    "\tmov ip, #0\n"
    "\tstr ip, [sp]\n"
    "\tstr ip, [sp, #4]\n"
    "\tmov r0, sp\n"
    "Lenter_interpreter_frame_start_val:\n"
    "\tblx r2\n"
    "Lenter_interpreter_frame_end_val:\n"
    "\tsub sp, fp, #4\n"
    "\tpop {fp, pc}\n"
    "\t.fnend\n"
    ASM_END
    );

#define CALLBACK_ABI

#elif defined(_CPU_PPC64_)
/**
 * Implementation notes:
 *
 * This needs to follow the PPC ELFv2 ABI. Which means that there is a localentry
 * and a global entry. The local entry expects r2/TOC to be set correctly, while
 * the global entry expects r12 to be set to the function address, and from there
 * restores r2/TOC. The function pointer we are getting passed point to the global
 * entry and thus we need to set r12 correctly.
 *
 * - LR is stored in the caller
 * - r1/SP is a back-chain that needs to be atomically updated
 */

#define MAX_INTERP_STATE_SIZE 64
#define MIN_STACK 32
#define STACK_PADDING 0
#define STACK_SIZE (MIN_STACK + MAX_INTERP_STATE_SIZE + STACK_PADDING)

size_t TOTAL_STACK_PADDING = MIN_STACK;

// Check that the interpreter state can fit
static_assert(sizeof(interpreter_state) <= MAX_INTERP_STATE_SIZE,
              "Stack layout invariants violated.");
// Check that the alignment of the type is satisfied
static_assert(alignof(interpreter_state) <= 16, "Stack layout invariants violated");
// Check that ABI stack alignment requirement is maintained.
static_assert(STACK_SIZE % 16 == 0, "Stack layout invariants violated");
static_assert(MIN_STACK  % 16 == 0, "Stack layout invariants violated");

asm(
    ASM_ENTRY
    MANGLE("enter_interpreter_frame") ":\n"
    "\taddis 2, 12, .TOC.-enter_interpreter_frame@ha\n"
    "\taddi 2, 2, .TOC.-enter_interpreter_frame@l\n"
    "\t.localentry enter_interpreter_frame, .-enter_interpreter_frame\n"
    ".cfi_startproc\n"
    // store LR
    "\tmflr 0\n"
    "\tstd 0, 16(1)\n"
    ".cfi_offset lr, 16\n"
    // set up stack frame
    "\tstdu 1, -" XSTR(STACK_SIZE) "(1)\n"
    ".cfi_adjust_cfa_offset " XSTR(STACK_SIZE) "\n"
    "\tmtctr 3\n" // move arg1 (func pointer) to ctr
    "\tmr 12, 3\n" // move func pointer to r12 if we jump to global entry point
    "\tcal 3, " XSTR(MIN_STACK) "(1)\n" // move pointer to INTERP_STATE to arg1
    // zero out src and mi field
    "\tli 6, 0\n"
    "\tstd 6, 0(3)\n"
    "\tstd 6, 8(3)\n"
    // store TOC
    "\tstd 2, 24(1)\n"
    "Lenter_interpreter_frame_start_val:\n"
    "\tbctrl\n"
    "Lenter_interpreter_frame_end_val:\n"
    // restore TOC
    "\tld 2, 24(1)\n"
    // restore stack frame
    "\tld 1, 0(1)\n"
    // restore LR
    "\tld 0, 16(1)\n"
    "\tmtlr 0\n"
    ".cfi_same_value lr\n"
    "\tblr\n"
    ".cfi_endproc\n"
    ASM_END
    );

#define CALLBACK_ABI

#else
#warning "Interpreter backtraces not implemented for this platform"
#define NO_INTERP_BT
#endif

#ifndef NO_INTERP_BT
extern uintptr_t enter_interpreter_frame_start_val asm("Lenter_interpreter_frame_start_val");
extern uintptr_t enter_interpreter_frame_end_val asm("Lenter_interpreter_frame_end_val");
uintptr_t enter_interpreter_frame_start = (uintptr_t)&enter_interpreter_frame_start_val;
uintptr_t enter_interpreter_frame_end = (uintptr_t)&enter_interpreter_frame_end_val;

JL_DLLEXPORT int jl_is_interpreter_frame(uintptr_t ip)
{
    return __start_jl_interpreter_frame <= ip && ip <= __stop_jl_interpreter_frame;
}

JL_DLLEXPORT int jl_is_enter_interpreter_frame(uintptr_t ip)
{
    return enter_interpreter_frame_start <= ip && ip <= enter_interpreter_frame_end;
}

JL_DLLEXPORT size_t jl_capture_interp_frame(uintptr_t *data, uintptr_t sp, uintptr_t fp, size_t space_remaining)
{
#ifdef FP_CAPTURE_OFFSET
    interpreter_state *s = (interpreter_state *)(fp-FP_CAPTURE_OFFSET);
#else
    interpreter_state *s = (interpreter_state *)(sp+TOTAL_STACK_PADDING);
#endif
    if (space_remaining <= 1)
        return 0;
    // Sentinel value to indicate an interpreter frame
    data[0] = JL_BT_INTERP_FRAME;
    data[1] = s->mi ? (uintptr_t)s->mi : s->src ? (uintptr_t)s->src : (uintptr_t)jl_nothing;
    data[2] = (uintptr_t)s->ip;
    return 2;
}

extern void * CALLBACK_ABI enter_interpreter_frame(void * CALLBACK_ABI (*callback)(interpreter_state *, void *), void *arg);
#else
JL_DLLEXPORT int jl_is_interpreter_frame(uintptr_t ip)
{
    return 0;
}

JL_DLLEXPORT int jl_is_enter_interpreter_frame(uintptr_t ip)
{
    return 0;
}

JL_DLLEXPORT size_t jl_capture_interp_frame(uintptr_t *data, uintptr_t sp, uintptr_t fp, size_t space_remaining)
{
    return 0;
}
#define CALLBACK_ABI
void *enter_interpreter_frame(void *(*callback)(interpreter_state *, void *), void *arg) {
    interpreter_state state = {};
    return callback(&state, arg);
}
#endif
