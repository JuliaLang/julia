// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef ENABLE_TIMINGS
#define JL_TIMING(owner)
#else

#ifdef __cplusplus
extern "C" {
#endif
void jl_print_timings(void);
extern jl_timing_block_t *jl_root_timing;
void jl_timing_block_start(jl_timing_block_t *cur_block);
void jl_timing_block_stop(jl_timing_block_t *cur_block);
#ifdef __cplusplus
}
#endif

#ifndef __cplusplus
#define JL_TIMING(owner)
#else

static inline uint64_t rdtscp(void)
{
    uint64_t rax,rdx;
    asm volatile ( "rdtscp\n" : "=a" (rax), "=d" (rdx) : : "rcx" );
    return (rdx << 32) + rax;
}

#define JL_TIMING_OWNERS          \
        X(ROOT),                  \
        X(GC),                    \
        X(LOWERING),              \
        X(PARSING),               \
        X(INFERENCE),             \
        X(CODEGEN),               \
        X(METHOD_LOOKUP_SLOW),    \
        X(METHOD_LOOKUP_FAST),    \
        X(LLVM_OPT),              \
        X(LLVM_MODULE_FINISH),    \
        X(LLVM_EMIT),             \
        X(METHOD_LOOKUP_COMPILE), \
        X(TYPE_CACHE_LOOKUP),     \
        X(TYPE_CACHE_INSERT),     \
        X(STAGED_FUNCTION),       \
        X(MACRO_INVOCATION),      \
        X(AST_COMPRESS),          \
        X(AST_UNCOMPRESS),        \
        X(SYSIMG_LOAD),           \
        X(SYSIMG_DUMP),

enum jl_timing_owners {
#define X(name) JL_TIMING_ ## name
    JL_TIMING_OWNERS
#undef X
    JL_TIMING_LAST
};

extern uint64_t jl_timing_data[(int)JL_TIMING_LAST];
extern const char *jl_timing_names[(int)JL_TIMING_LAST];
#define JL_TIMING(owner) jl_timing_block_t __timing_block(JL_TIMING_ ## owner)

struct _jl_timing_block_t {
    jl_timing_block_t *prev;
    uint64_t total;
    uint64_t t0;
    int owner;
#ifdef JL_DEBUG_BUILD
    bool running;
#endif
    _jl_timing_block_t(int owner) {
        uint64_t t = init(owner);
        jl_timing_block_t **prevp = jl_current_task ? &jl_current_task->timing_stack : &jl_root_timing;
        prev = *prevp;
        if (prev)
            prev->stop(t);
        *prevp = this;
    }
    _jl_timing_block_t() {
        init(JL_TIMING_ROOT);
        prev = NULL;
    }
    uint64_t init(int owner) {
        uint64_t t = rdtscp();
        this->owner = owner;
        total = 0;
#ifdef JL_DEBUG_BUILD
        running = false;
#endif
        start(t);
        return t;
    }
    inline void stop(uint64_t t) {
#ifdef JL_DEBUG_BUILD
        assert(running);
        running = false;
#endif
        total += t - t0;
    }
    inline void start(uint64_t t) {
#ifdef JL_DEBUG_BUILD
        assert(!running);
        running = true;
#endif
        t0 = t;
    }
    ~_jl_timing_block_t() {
        uint64_t t = rdtscp();
        stop(t);
        jl_timing_data[owner] += total;
        jl_timing_block_t **pcur = jl_current_task ? &jl_current_task->timing_stack : &jl_root_timing;
        assert(*pcur == this);
        *pcur = prev;
        if (prev)
            prev->start(t);
    }
};

#endif
#endif
