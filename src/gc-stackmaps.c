static htable_t addr_to_stackmap;
static arraylist_t pending_stackmaps;
static unw_context_t gc_current_task_context;
static unw_addr_space_t task_addr_space;
static unw_accessors_t *local_accessors;
typedef struct {
    jl_task_t *task;
    jl_jmp_buf ctx;
    uintptr_t stacktop;
} task_unwind_ctx_t;

void jl_gc_register_stackmaps(uint8_t *s)
{
    if (s) {
        arraylist_push(&pending_stackmaps, s);
    }
}

typedef struct {
    uint8_t type;
    uint8_t _reserved;
    uint16_t dwarf_reg;
    int32_t offset;
} __attribute__((__packed__)) stackmap_loc_t;
typedef struct {
    uint64_t addr;
    uint64_t ssize;
} __attribute__((__packed__)) stackmap_func_t;


void gc_parse_stackmaps(uint8_t *s)
{
    // see http://llvm.org/docs/StackMaps.html#stack-map-format
    uint8_t version = *s;
    if (version != 1) {
        fprintf(stderr, "unsupported LLVM stackmaps version %d\n", version);
        abort();
    }
    s += 4;
    uint32_t nfunc = *(uint32_t*)s, nconst = *((uint32_t*)s+1), nrec = *((uint32_t*)s+2);
    s += 12;
    stackmap_func_t *functions = (stackmap_func_t*)s;
#if 0
    for (size_t i = 0; i < nfunc; i++) {
        uint64_t fbase = *(uint64_t*)s;
        uint64_t ssize = *((uint64_t*)s+1);
        printf("F %p %p %d\n", fbase, ssize, frec);
        s += 16;
    }
#else
    s += 16*nfunc;
#endif
    s += nconst * 8;
    uint64_t last_id;
    for (size_t i = 0; i < nrec; i++) {
        uint64_t id = *(uint64_t*)s;
        // keep "functions" pointing to the current function
        if (i > 0 && id != last_id)
            functions++;
        s += 8;
        uint32_t rel_ip = *(uint32_t*)s;
        s += 4;
        s += 2;
        //printf("R %p %p %d %p\n", id, functions[2], rel_ip, functions[0] + rel_ip);
        uintptr_t ip = functions->addr + rel_ip;
        void** bp = ptrhash_bp(&addr_to_stackmap, (void*)ip);
        if (*bp != HT_NOTFOUND) {
            fprintf(stderr, "Duplicate entry in stackmaps for ip %p\n", (void*)ip);
            abort();
        }
        *bp = s;

        uint16_t nloc = *(uint16_t*)s;
        s += 2;
#if 0
        for (size_t j = 0; j < nloc; j++) {
            uint8_t loc_typ = *(uint8_t*)s;
            s += 2;
            uint16_t dwarf_reg = *(uint16_t*)s;
            s += 2;
            int32_t offset = *(int32_t*)s;
            s += 4;
            printf("\tloc %d %d %d\n", loc_typ, dwarf_reg, offset);
        }
#else
        s += nloc*8;
#endif
        uint16_t nliveout = *((uint16_t*)s+1);
        s += 4;
        s += nliveout * 4;
        if (((uintptr_t)s) % 8) {
            s += 4;
        }
        last_id = id;
    }
}


int task_find_proc_info(unw_addr_space_t as,
                        unw_word_t ip, unw_proc_info_t *pip,
                        int need_unwind_info, void *arg)
{
    return local_accessors->find_proc_info(unw_local_addr_space,
                                           ip, pip, need_unwind_info, NULL);
}

void task_put_unwind_info(unw_addr_space_t as,
                          unw_proc_info_t *pip, void *arg)
{
    local_accessors->put_unwind_info(unw_local_addr_space,
                                     pip, NULL);
}

int task_get_dyn_info_list_addr(unw_addr_space_t as,
                                unw_word_t *dilap, void *arg)
{
    return local_accessors->get_dyn_info_list_addr(unw_local_addr_space,
                                                   dilap, NULL);
}

#ifdef COPY_STACKS
uintptr_t task_remap_memory(task_unwind_ctx_t *ctx, uintptr_t addr)
{
    jl_task_t *task = ctx->task;
    jl_tls_states_t *ptls = jl_get_ptls_states();
    uintptr_t stacktop = (uintptr_t)ptls->stackbase - task->ssize;
    if (addr >=  stacktop && addr <= ptls->stackbase) {
        addr += task->stkbuf - stacktop;
    }
    return addr;
}

int task_access_mem(unw_addr_space_t as,
                    unw_word_t addr, unw_word_t *valp,
                    int write, void *arg)
{
    task_unwind_ctx_t *ctx = (task_unwind_ctx_t*)arg;
    addr = task_remap_memory(ctx, addr);
    if (write) {
#ifdef JL_DEBUG_BUILD
        fprintf(stderr, "libunwind tried to write to memory at %p\n", (void*)addr);
        abort();
#endif
        return -UNW_EINVAL;
    }
    
    *valp = *(uintptr_t*)addr;
    return 0;
}
#else
#error Stackmaps not implemented for !COPY_STACKS
#endif // COPY_STACKS

static intptr_t ptr_demangle(intptr_t p)
{
    intptr_t ret;
    asm(" movq %1, %%rax;\n"
        " rorq $17, %%rax;"
        " xorq %%fs:0x30, %%rax;"
        " movq %%rax, %0;"
        : "=r"(ret) : "r"(p) : "%rax" );
    return ret;
}

int task_access_reg(unw_addr_space_t as,
                    unw_regnum_t regnum, unw_word_t *valp,
                    int write, void *arg)
{
    task_unwind_ctx_t *ctx = (task_unwind_ctx_t*)arg;
    if (write) {
#ifdef JL_DEBUG_BUILD
        fprintf(stderr, "libunwind tried to write to register %d\n", regnum);
        abort();
#endif
        return -UNW_EINVAL;
    }
    uintptr_t *jmpbuf_regs = (uintptr_t*)ctx->ctx;
    switch (regnum) {
    case UNW_REG_IP:
        *valp = ptr_demangle(jmpbuf_regs[7]);
        break;
    case UNW_REG_SP:
        *valp = ptr_demangle(jmpbuf_regs[6]);
        break;
    case UNW_X86_64_RBP:
        *valp = ptr_demangle(jmpbuf_regs[1]);
        break;
    default:
#ifdef JL_DEBUG_BUILD
        fprintf(stderr, "libunwind asked for register %d\n", regnum);
        abort();
#endif
        return -UNW_ENOINFO;
    }
    return 0;
}
int task_access_fpreg(unw_addr_space_t as,
                      unw_regnum_t regnum, unw_fpreg_t *fpvalp,
                      int write, void *arg)
{
#ifdef JL_DEBUG_BUILD
    fprintf(stderr, "libunwind called access_fpreg(%d)\n", regnum);
    abort();
#endif
    return -UNW_ENOINFO;
}
int task_resume(unw_addr_space_t as,
                unw_cursor_t *cp, void *arg)
{
#ifdef JL_DEBUG_BUILD
    fprintf(stderr, "libunwind called resume\n");
    abort();
#endif
    return -UNW_ENOINFO;
}
int task_get_proc_name(unw_addr_space_t as,
                       unw_word_t addr, char *bufp,
                       size_t buf_len, unw_word_t *offp,
                       void *arg)
{
#ifdef JL_DEBUG_BUILD
    fprintf(stderr, "libunwind called get_proc_name\n");
    abort();
#endif
    return -UNW_ENOINFO;
}

void gc_parse_pending_stackmaps(void)
{
    for (int i = 0; i < pending_stackmaps.len; i++)
        gc_parse_stackmaps((uint8_t*)pending_stackmaps.items[i]);
    pending_stackmaps.len = 0;
}

#ifdef GC_DEBUG_STACKMAPS
void gc_unwind_task(jl_task_t *task, arraylist_t *frames)
#else
void gc_unwind_task(jl_task_t *task, int d)
#endif
{
    bt_cursor_t cursor;
    task_unwind_ctx_t ctx;
    if (task == jl_current_task) {
        if (unw_init_local(&cursor, &gc_current_task_context) < 0)
            abort();
    }
    else {
        ctx.task = task;
        memcpy(&ctx.ctx, task->ctx, sizeof(void*)*8);
        if (unw_init_remote(&cursor, task_addr_space, &ctx) < 0)
            abort();
    }
    //printf("Looking for %p\n\n", roots);
#ifdef GC_DEBUG_STACKMAPS
    int frame_idx = 0;
#endif
    uintptr_t ip;
    while (1) {
        if (unw_step(&cursor) < 0) {
            break;
        }
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0)
            abort();
        if (!ip) break;
#if 0
        uintptr_t sp = 0, bp = 0;
        unw_get_reg(&cursor, UNW_REG_SP, &sp);
        unw_get_reg(&cursor, UNW_X86_64_RBP, &bp);
        printf("FRAME ip:%p sp:%p bp:%p\n", ip, sp, bp);
#endif
        void **sm_bp = ptrhash_bp(&addr_to_stackmap, (void*)ip);
        if (*sm_bp != HT_NOTFOUND) {
            uint8_t *s = (uint8_t*)*sm_bp;
            uint16_t nloc = *(uint16_t*)s;
            s += 2;
            stackmap_loc_t *locs = (stackmap_loc_t*)s;
            // the first 3 entries are additional metadata emitted by
            // the statepoint lowering. check that they are what we expect.
            if (nloc != 4) {
                fprintf(stderr, "Too many location entries in stack map : %d\n", nloc);
                abort();
            }
            if (!(locs[0].type == 4 &&
                  locs[1].type == 4 &&
                  locs[2].type == 4 && locs[2].offset == 1)) {
                fprintf(stderr, "Unexpected statepoint metadata\n");
                abort();
            }
            stackmap_loc_t *loc = &locs[3];
            if (loc->type != 2) { // register relative address
                fprintf(stderr, "Unsupported stack map location type %d\n", loc->type);
                abort();
            }
            unw_regnum_t reg;
            switch (loc->dwarf_reg) {
            case 7: // %rsp
                reg = UNW_REG_SP;
                break;
            case 6: // %rbp
                reg = UNW_X86_64_RBP;
                break;
            default:
                fprintf(stderr, "Unsupported register %d\n", loc->dwarf_reg);
                abort();
            }
            uintptr_t gcframe;
            if (unw_get_reg(&cursor, reg, &gcframe) < 0) {
                fprintf(stderr, "Could not read required reg %d from frame (ip:%p)\n", loc->dwarf_reg, (void*)ip);
                abort();
            }
            gcframe = gcframe + loc->offset;
            if (task != jl_current_task)
                gcframe = task_remap_memory(&ctx, gcframe);

#ifdef GC_DEBUG_STACKMAPS
            if (frame_idx >= frames->len) {
                fprintf(stderr, "Too many gc frames in unwinder\n");
                abort();
            }
            void *shadow_frame = frames->items[frame_idx];
            if (gcframe != shadow_frame) {
                fprintf(stderr, "Could not find gc frame %p (found %p instead)\n", shadow_frame, (void*)gcframe);
                abort();
            } else {
                frame_idx++;
            }
#else
            int isjit = gc_mark_frame((jl_gcframe_t*)gcframe, 0, d);
            assert(isjit);
#endif
        }
    }
#ifdef GC_DEBUG_STACKMAPS
    if (frame_idx != frames->len) {
        fprintf(stderr, "Not enough gc frames in unwinder\n");
        abort();
    } else {
        printf("OK %d !\n", frame_idx);
    }
#endif
}

void gc_stackmaps_init(void)
{
    local_accessors = unw_get_accessors(unw_local_addr_space);
    unw_accessors_t task_accessors = {
        task_find_proc_info,
        task_put_unwind_info,
        task_get_dyn_info_list_addr,
        task_access_mem,
        task_access_reg,
        task_access_fpreg,
        task_resume,
        task_get_proc_name
    };
    task_addr_space = unw_create_addr_space(&task_accessors, 0);
    htable_new(&addr_to_stackmap, 512);
    arraylist_new(&pending_stackmaps, 512);
}
