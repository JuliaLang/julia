// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <string.h>
#include <stdio.h>

#include "julia.h"
#include "julia_internal.h"
#include "processor.h"
#include "julia_assert.h"

#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

#include "support/ios.h"
#include "support/strhash.h"

// --- library symbol lookup ---
jl_value_t *jl_libdl_dlopen_func JL_GLOBALLY_ROOTED;

// map from user-specified lib names to handles
static htable_t libMap;
static jl_mutex_t libmap_lock;

void *jl_get_library_(const char *f_lib, int throw_err)
{
    if (f_lib == NULL)
        return jl_RTLD_DEFAULT_handle;
    if (f_lib == JL_EXE_LIBNAME)
        return jl_exe_handle;
    if (f_lib == JL_LIBJULIA_INTERNAL_DL_LIBNAME)
        return jl_libjulia_internal_handle;
    if (f_lib == JL_LIBJULIA_DL_LIBNAME)
        return jl_libjulia_handle;
    JL_LOCK(&libmap_lock);
    // This is the only operation we do on the map, which doesn't invalidate
    // any references or iterators.
    void **map_slot = strhash_bp(&libMap, (void*)f_lib);
    void *hnd = *map_slot;
    if (hnd == HT_NOTFOUND) {
        hnd = jl_load_dynamic_library(f_lib, JL_RTLD_DEFAULT, throw_err);
        if (hnd != NULL)
            *map_slot = hnd;
    }
    JL_UNLOCK(&libmap_lock);
    return hnd;
}

JL_DLLEXPORT
void *jl_load_and_lookup(const char *f_lib, const char *f_name, _Atomic(void*) *hnd)
{
    void *handle = jl_atomic_load_acquire(hnd);
    if (!handle)
        jl_atomic_store_release(hnd, (handle = jl_get_library(f_lib)));
    void * ptr;
    jl_dlsym(handle, f_name, &ptr, 1, 1);
    return ptr;
}

// jl_load_and_lookup, but with library computed at run time on first call
JL_DLLEXPORT
void *jl_lazy_load_and_lookup(jl_value_t *lib_val, jl_value_t *f_name)
{
    void *lib_ptr;
    const char *fname_str;

    if (jl_is_symbol(f_name))
        fname_str = jl_symbol_name((jl_sym_t*)f_name);
    else if (jl_is_string(f_name))
        fname_str = jl_string_data(f_name);
    else
        jl_type_error("cglobal/ccall function name", (jl_value_t*)jl_symbol_type, f_name);

    if (lib_val) {
        if (jl_is_symbol(lib_val))
            lib_ptr = jl_get_library(jl_symbol_name((jl_sym_t*)lib_val));
        else if (jl_is_string(lib_val))
            lib_ptr = jl_get_library(jl_string_data(lib_val));
        else if (jl_libdl_dlopen_func != NULL) {
            lib_ptr = jl_unbox_voidpointer(jl_apply_generic(jl_libdl_dlopen_func, &lib_val, 1));
        } else
            jl_type_error("cglobal/ccall", (jl_value_t*)jl_symbol_type, lib_val);
    }
    else {
        // If the user didn't supply a library name, try to find it now from the runtime value of f_name
        lib_ptr = jl_get_library(jl_dlfind(fname_str));
    }

    void *ptr;
    jl_dlsym(lib_ptr, fname_str, &ptr, 1, 1);
    return ptr;
}

// miscellany

JL_DLLEXPORT
jl_value_t *jl_get_JIT(void)
{
    const char *JITName = "ORCJIT";
    return jl_pchar_to_string(JITName, strlen(JITName));
}

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN 256
#endif

// Form a file name from a pattern made by replacing tokens,
// similar to many of those provided by ssh_config TOKENS:
//
//           %%    A literal `%'.
//           %p    The process PID
//           %d    Local user's home directory.
//           %i    The local user ID.
//           %L    The local hostname.
//           %l    The local hostname, including the domain name.
//           %u    The local username.
JL_DLLEXPORT char *jl_format_filename(const char *output_pattern) JL_NOTSAFEPOINT
{
    ios_t buf;
    ios_mem(&buf, 128);
    int special = 0;
    char hostname[MAXHOSTNAMELEN + 1];
    uv_passwd_t pwd;
    int got_pwd = 0;
    for (const char *p = output_pattern; *p; p++) {
        char c = *p;
        if (special) {
            if (!got_pwd && (c == 'i' || c == 'd' || c == 'u')) {
                int r = uv_os_get_passwd(&pwd);
                if (r == 0)
                    got_pwd = 1;
            }
            switch (c) {
            case 'p':
                ios_printf(&buf, "%d", (int)uv_os_getpid());
                break;
            case 'd':
                if (got_pwd)
                    ios_puts(pwd.homedir, &buf);
                break;
            case 'i':
                if (got_pwd)
                    ios_printf(&buf, "%ld", (long)pwd.uid);
                break;
            case 'l':
            case 'L':
                if (gethostname(hostname, sizeof(hostname)) == 0) {
                    hostname[sizeof(hostname) - 1] = '\0'; /* Null terminate, just to be safe. */
                    ios_puts(hostname, &buf);
                }
#ifndef _OS_WINDOWS_
                if (c == 'l' && getdomainname(hostname, sizeof(hostname)) == 0) {
                    hostname[sizeof(hostname) - 1] = '\0'; /* Null terminate, just to be safe. */
                    ios_puts(hostname, &buf);
                }
#endif
                break;
            case 'u':
                if (got_pwd)
                    ios_puts(pwd.username, &buf);
                break;
            default:
                ios_putc(c, &buf);
                break;
            }
            special = 0;
        }
        else if (c == '%') {
            special = 1;
        }
        else {
            ios_putc(c, &buf);
        }
    }
    if (got_pwd)
        uv_os_free_passwd(&pwd);
    size_t len;
    // ios_take_buffer nul-terminates and transfers ownership to caller
    return ios_take_buffer(&buf, &len);
}


static uv_mutex_t trampoline_lock; // for accesses to the cache and freelist

static void *trampoline_freelist;

static void *trampoline_alloc(void) JL_NOTSAFEPOINT // lock taken by caller
{
    const int sz = 64; // oversized for most platforms. todo: use precise value?
    if (!trampoline_freelist) {
        int last_errno = errno;
#ifdef _OS_WINDOWS_
        DWORD last_error = GetLastError();
        void *mem = VirtualAlloc(NULL, jl_page_size,
                MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        if (mem == NULL)
            jl_throw(jl_memory_exception);
        SetLastError(last_error);
#else
        void *mem = mmap(0, jl_page_size, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        errno = last_errno;
        if (mem == MAP_FAILED)
            jl_throw(jl_memory_exception);
#endif
        errno = last_errno;
        void *next = NULL;
        assert(sz < jl_page_size);
        for (size_t i = 0; i + sz <= jl_page_size; i += sz) {
            void **curr = (void**)((char*)mem + i);
            *curr = next;
            next = (void*)curr;
        }
        trampoline_freelist = next;
    }
    void *tramp = trampoline_freelist;
    trampoline_freelist = *(void**)tramp;
    return tramp;
}

static void trampoline_free(void *tramp) JL_NOTSAFEPOINT    // lock taken by caller
{
    *(void**)tramp = trampoline_freelist;
    trampoline_freelist = tramp;
}

static void trampoline_deleter(void **f) JL_NOTSAFEPOINT
{
    void *tramp = f[0];
    void *fobj = f[1];
    void *cache = f[2];
    void *nval = f[3];
    f[0] = NULL;
    f[2] = NULL;
    f[3] = NULL;
    uv_mutex_lock(&trampoline_lock);
    if (tramp)
        trampoline_free(tramp);
    if (fobj && cache)
        ptrhash_remove((htable_t*)cache, fobj);
    if (nval)
        free(nval);
    uv_mutex_unlock(&trampoline_lock);
}

typedef void *(*init_trampoline_t)(void *tramp, void **nval) JL_NOTSAFEPOINT;

// Use of `cache` is not clobbered in JL_TRY
JL_GCC_IGNORE_START("-Wclobbered")
JL_DLLEXPORT
jl_value_t *jl_get_cfunction_trampoline(
    // dynamic inputs:
    jl_value_t *fobj,
    jl_datatype_t *result_type,
    // call-site constants:
    htable_t *cache, // weakref htable indexed by (fobj, vals)
    jl_svec_t *fill,
    init_trampoline_t init_trampoline,
    jl_unionall_t *env,
    jl_value_t **vals)
{
    // lookup (fobj, vals) in cache
    uv_mutex_lock(&trampoline_lock);
    if (!cache->table)
        htable_new(cache, 1);
    if (fill != jl_emptysvec) {
        htable_t **cache2 = (htable_t**)ptrhash_bp(cache, (void*)vals);
        cache = *cache2;
        if ((void*)cache == HT_NOTFOUND) {
            cache = htable_new((htable_t*)malloc_s(sizeof(htable_t)), 1);
            *cache2 = cache;
        }
    }
    void *tramp = ptrhash_get(cache, (void*)fobj);
    uv_mutex_unlock(&trampoline_lock);
    if (tramp != HT_NOTFOUND) {
        assert((jl_datatype_t*)jl_typeof(tramp) == result_type);
        return (jl_value_t*)tramp;
    }

    // not found, allocate a new one
    size_t n = jl_svec_len(fill);
    void **nval = (void**)malloc_s(sizeof(void*) * (n + 1));
    nval[0] = (void*)fobj;
    jl_value_t *result;
    JL_TRY {
        for (size_t i = 0; i < n; i++) {
            jl_value_t *sparam_val = jl_instantiate_type_in_env(jl_svecref(fill, i), env, vals);
            if (sparam_val != (jl_value_t*)jl_any_type)
                if (!jl_is_concrete_type(sparam_val) || !jl_is_immutable(sparam_val))
                    sparam_val = NULL;
            nval[i + 1] = (void*)sparam_val;
        }
        int permanent =
            (result_type == jl_voidpointer_type) ||
            jl_is_concrete_type(fobj) ||
            (((jl_datatype_t*)jl_typeof(fobj))->instance == fobj);
        if (jl_is_unionall(fobj)) {
            jl_value_t *uw = jl_unwrap_unionall(fobj);
            if (jl_is_datatype(uw) && ((jl_datatype_t*)uw)->name->wrapper == fobj)
                permanent = 1;
        }
        if (permanent) {
            jl_task_t *ct = jl_current_task;
            result = jl_gc_permobj(ct->ptls, sizeof(jl_taggedvalue_t) + jl_datatype_size(result_type), result_type, 0);
            memset(result, 0, jl_datatype_size(result_type));
        }
        else {
            result = jl_new_struct_uninit(result_type);
        }
        if (result_type != jl_voidpointer_type) {
            assert(jl_datatype_size(result_type) == sizeof(void*) * 4);
            ((void**)result)[1] = (void*)fobj;
        }
        if (!permanent) {
            jl_task_t *ct = jl_current_task;
            jl_gc_add_ptr_finalizer(ct->ptls, result, (void*)(uintptr_t)&trampoline_deleter);
            ((void**)result)[2] = (void*)cache;
            ((void**)result)[3] = (void*)nval;
        }
    }
    JL_CATCH {
        free(nval);
        jl_rethrow();
    }
    uv_mutex_lock(&trampoline_lock);
    tramp = trampoline_alloc();
    ((void**)result)[0] = tramp;
    init_trampoline(tramp, nval);
    ptrhash_put(cache, (void*)fobj, result);
    uv_mutex_unlock(&trampoline_lock);
    return result;
}
JL_GCC_IGNORE_STOP

static inline const char *name_from_method_instance(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    assert(jl_is_method_instance(mi));
    return jl_is_method(mi->def.method) ? jl_symbol_name(mi->def.method->name) : "top-level scope";
}

static jl_mutex_t cfun_lock;

// (get_abi_converter / method table mutating thread)
// release jl_world_counter
// release theFptr
// release last_world_v
//
// (dispatch site)
// acquire last_world_v
// acquire theFptr
// read jl_world_counter
//
// The above ordering requirements are intended to guarantee that if the
// dispatch site observes last_world == jl_world_counter then the loaded
// fptr is consistent with both of them, meaning it was published for
// exactly that world.
// The resolved CodeInstance behind a trampoline's `last_invoked` (Union{CodeInstance,
// ABIAdapter}): the ABIAdapter's target CI, the bare CodeInstance itself, or NULL (unresolved /
// no method).
static jl_code_instance_t *invoked_ci(jl_value_t *last_invoked) JL_NOTSAFEPOINT
{
    if (last_invoked == NULL)
        return NULL;
    if (jl_is_abi_adapter(last_invoked))
        return ((jl_abi_adapter_t*)last_invoked)->ci;
    return (jl_code_instance_t*)last_invoked; // a bare CodeInstance (declared ABI matched its specptr)
}

// Resolve (JIT or re-validate) the ABI adapter for a @cfunction/@ccallable dispatch
// trampoline at the current world, publishing the resulting adapter in `tr->fptr`. Called
// by the inline call-site poll (emit_abi_call) when its cached `last_world` is stale.
JL_DLLEXPORT
void *jl_resolve_trampoline(jl_task_t *ct, jl_dispatch_trampoline_t *tr)
{
    jl_value_t *sigt = tr->sigt;
    JL_GC_PROMISE_ROOTED(sigt);
    jl_value_t *rt = tr->rt;
    JL_GC_PROMISE_ROOTED(rt);
    size_t nargs = jl_nparams(sigt);
    jl_abi_t from_abi = { sigt, rt, nargs, tr->specsig, /*is_opaque_closure*/0 };
    jl_value_t *mi;
    jl_code_instance_t *codeinst;
    size_t world;
    JL_LOCK(&cfun_lock);
    do {
        size_t last_world_v = jl_atomic_load_relaxed(&tr->last_world);
        void *f = jl_atomic_load_relaxed(&tr->fptr);
        jl_value_t *last_invoked = tr->last_invoked;
        JL_GC_PROMISE_ROOTED(last_invoked); // retained by the adapter/MI cache or an image root
        jl_code_instance_t *last_ci = invoked_ci(last_invoked);
        world = jl_atomic_load_acquire(&jl_world_counter);
        ct->world_age = world;
        if (f != NULL && world == last_world_v) {
            JL_UNLOCK(&cfun_lock);
            return f;
        }
        mi = jl_get_specialization1((jl_tupletype_t*)sigt, world);
        // Re-validate WITHOUT inferring: if `last_invoked` still matches the dispatch result for
        // this world -- its CodeInstance's mi matches the freshly looked-up `mi` (or both indicate
        // "no method") and that CI is still valid at `world` -- restore `fptr` straight from
        // `last_invoked` (an ABIAdapter carries its compiled thunk's `fptr`; a bare CodeInstance
        // means the declared ABI matched its specptr, so re-derive that). `jl_get_specialization1`
        // is a method-table lookup (no inference/compilation), so this lets an AOT-compiled adapter
        // be wired on the first call after load -- and survive a later world bump -- without
        // re-JITing and without an adapter-cache lookup.
        if (last_invoked != NULL) {
            int still_valid = last_ci == NULL
                ? (mi == jl_nothing)
                : ((jl_value_t*)jl_get_ci_mi(last_ci) == mi &&
                   jl_atomic_load_relaxed(&last_ci->max_world) >= world);
            if (still_valid) {
                void *nf = f;
                if (nf == NULL) { // first call after load: restore fptr directly from `last_invoked`
                    if (jl_is_abi_adapter(last_invoked)) {
                        nf = ((jl_abi_adapter_t*)last_invoked)->fptr;
                    }
                    else {
                        void *tgt; int ts; jl_callptr_t inv;
                        nf = jl_abi_adapter_resolve_target(from_abi, last_ci, &tgt, &ts, &inv);
                    }
                }
                if (nf != NULL) {
                    jl_atomic_store_release(&tr->fptr, nf);
                    jl_atomic_store_release(&tr->last_world, world);
                    JL_UNLOCK(&cfun_lock);
                    return nf;
                }
            }
        }
        JL_UNLOCK(&cfun_lock);
        // slow: infer the target outside the lock (this is very slow)
        codeinst = mi != jl_nothing ? jl_type_infer((jl_method_instance_t*)mi, world, SOURCE_MODE_ABI, jl_options.trim) : NULL;
        // Compile the target now (the JIT path's job) so its specptr is available to the specptr
        // shortcut in jl_abi_adapter_resolve_target. SOURCE_MODE_ABI only infers and sets up the
        // ABI -- it can leave `invoke` as the wait-for-compiled sentinel, which the resolver (kept
        // deliberately codegen-free so it is shared with the no-codegen fallback, #61949) reads
        // back as unavailable and then hands a dynamic-dispatch adapter for an ABI the target's own
        // specptr already satisfies. Doing the compile here keeps the resolver a pure metadata
        // lookup while still taking the (adapter-free) shortcut whenever it applies.
        if (codeinst != NULL)
            jl_compile_codeinst(codeinst);
        JL_LOCK(&cfun_lock);
    } while (jl_atomic_load_acquire(&jl_world_counter) != world); // restart if the world moved under us
    // double-check another thread didn't already install for this world
    size_t lwv = jl_atomic_load_relaxed(&tr->last_world);
    void *f = jl_atomic_load_relaxed(&tr->fptr);
    if (f != NULL && world == lwv) {
        JL_UNLOCK(&cfun_lock);
        return f;
    }
    // If the world advanced but the dispatch target is unchanged from the one `fptr` was
    // built for, the existing adapter is still correct -- reuse it (no re-JIT).
    if (f != NULL && codeinst == invoked_ci(tr->last_invoked)) {
        jl_atomic_store_release(&tr->last_world, world);
        JL_UNLOCK(&cfun_lock);
        return f;
    }
    // @cfunction warns when the declared C return type cannot match the resolved target's
    // rettype (skip must-not-return targets, occasionally required by the C API for error
    // callbacks even though memory errors are then likely).
    if (codeinst != NULL) {
        jl_value_t *astrt = codeinst->rettype;
        if (astrt != (jl_value_t*)jl_bottom_type &&
            jl_type_intersection(astrt, rt) == jl_bottom_type)
            jl_printf(JL_STDERR, "WARNING: cfunction: return type of %s does not match\n",
                      name_from_method_instance((jl_method_instance_t*)mi));
    }
    // Resolve the adapter and record `last_invoked` as a Union{CodeInstance, ABIAdapter}: a bare
    // CodeInstance when the declared ABI already matches its specptr (no thunk needed), else the
    // interned ABIAdapter record (jl_jit_abi_converter -> the ABIAdapter cache; a NULL codeinst
    // yields the shared dynamic-dispatch adapter). The trampoline points at whichever it resolved
    // to, so a later re-validation restores `fptr` straight from it.
    void *tgt; int ts; jl_callptr_t inv;
    void *shortcut = jl_abi_adapter_resolve_target(from_abi, codeinst, &tgt, &ts, &inv);
    jl_value_t *new_invoked;
    if (shortcut != NULL) {
        f = shortcut;
        new_invoked = (jl_value_t*)codeinst; // bare CI: no adapter thunk required
    }
    else {
        f = jl_jit_abi_converter(ct, from_abi, codeinst);
        new_invoked = (jl_value_t*)jl_lookup_abi_adapter(sigt, rt, codeinst, tr->specsig, /*is_opaque_closure*/0, nargs);
    }
    tr->last_invoked = new_invoked; // published before fptr/last_world; roots the target
    if (new_invoked)
        jl_gc_wb(tr, new_invoked);
    // Publish `fptr` with *release*: the call-site poll acquire-loads it, and this release
    // pairs with that acquire so observing this fptr also makes our prior acquire-load of
    // `jl_world_counter` visible -- closing the window where a lock-free reader could pair a
    // newer fptr with a stale counter. `last_world` (release, stored after fptr) gives the
    // reader the lower bound.
    jl_atomic_store_release(&tr->fptr, f);
    jl_atomic_store_release(&tr->last_world, world);
    JL_UNLOCK(&cfun_lock);
    return f;
}

void jl_init_runtime_ccall(void)
{
    JL_MUTEX_INIT(&libmap_lock, "libmap_lock");
    strhash_new(&libMap, 16);
    uv_mutex_init(&trampoline_lock);
}
