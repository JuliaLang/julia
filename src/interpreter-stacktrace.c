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
#define ASM_ENTRY                               \
    ".text\n"                                   \
    ".p2align 4,0x90\n"                         \
    ".global enter_interpreter_frame\n"         \
    ".type enter_interpreter_frame,@function\n"
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


// This function is special. The unwinder looks for this function to find interpreter
// stack frames.
#ifdef _CPU_X86_64_

#ifdef _OS_WINDOWS_
size_t STACK_PADDING = 40;
#else
size_t STACK_PADDING = 8;
#endif

asm(
    ASM_ENTRY
    MANGLE("enter_interpreter_frame") ":\n"
    ".cfi_startproc\n"
    // sizeof(struct interpreter_state) is 44, but we need to be 8 byte aligned,
    // so subtract 48. For compact unwind info, we need to only have one subq,
    // so combine in the stack realignment for a total of 56 bytes.
    "\tsubq $56, %rsp\n"
    ".cfi_def_cfa_offset 64\n"
#ifdef _OS_WINDOWS_
    "\tmovq %rcx, %rax\n"
    "\tleaq 8(%rsp), %rcx\n"
#else
     "\tmovq %rdi, %rax\n"
     "\tleaq 8(%rsp), %rdi\n"
#endif
    // Zero out the src field
    "\tmovq $0, 8(%rsp)\n"
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
    "\taddq $56, %rsp\n"
#ifndef _OS_DARWIN_
    // Somehow this throws off compact unwind info on OS X
    ".cfi_def_cfa_offset 8\n"
#endif
    "\tretq\n"
    ".cfi_endproc\n"
    ASM_END
    );

#define CALLBACK_ABI
static_assert(sizeof(interpreter_state) <= 48, "Update assembly code above");

#elif defined(_CPU_X86_)

size_t STACK_PADDING = 12;
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
#define FP_CAPTURE_OFFSET 32
    "\tpushl %ebp\n"
    ".cfi_def_cfa_offset 8\n"
    "\tmovl %esp, %ebp\n"
#endif
     // sizeof(struct interpreter_state) is 32
     "\tsubl $32, %esp\n"
#ifdef _OS_WINDOWS_
     ".cfi_def_cfa_offset 40\n"
#else
     ".cfi_def_cfa_offset 36\n"
#endif
     "\tmovl %ecx, %eax\n"
     "\tmovl %esp, %ecx\n"
     // Zero out the src field
     "\tmovl $0, (%esp)\n"
     // Restore 16 byte stack alignment
#ifdef _OS_WINDOWS_
     // Technically not necessary, because we don't assume this alignment, but
     // let's be nice if we ever start doing that.
     "\tsubl $8, %esp\n"
#else
     "\tsubl $12, %esp\n"
#endif
     ".cfi_def_cfa_offset 48\n"
     "Lenter_interpreter_frame_start_val:\n"
     "\tcalll *%eax\n"
     "Lenter_interpreter_frame_end_val:\n"
#ifdef _OS_WINDOWS_
     "\taddl $40, %esp\n"
     ".cfi_def_cfa_offset 8\n"
     "\tpopl %ebp\n"
#else
     "\taddl $44, %esp\n"
#endif
     ".cfi_def_cfa_offset 4\n"
     "\tret\n"
     ".cfi_endproc\n"
     ASM_END
     );

#define CALLBACK_ABI  __attribute__((fastcall))
static_assert(sizeof(interpreter_state) <= 32, "Update assembly code above");

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
    interpreter_state *s = (interpreter_state *)(sp+STACK_PADDING);
#endif
    if (space_remaining <= 1 || s->src == 0)
        return 0;
    // Sentinel value to indicate an interpreter frame
    data[0] = (uintptr_t)-1;
    data[1] = (uintptr_t)s->src;
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
void *NOINLINE enter_interpreter_frame(void *(*callback)(interpreter_state *, void *), void *arg) {
    interpreter_state state = {};
    return callback(&state, arg);
}
#endif
