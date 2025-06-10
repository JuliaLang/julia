// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include <map>
#include <string>
#include <llvm/ADT/StringMap.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/raw_ostream.h>

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

using namespace llvm;

// --- library symbol lookup ---

// map from user-specified lib names to handles
static std::map<std::string, void*> libMap;
static jl_mutex_t libmap_lock;
extern "C"
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
    void **map_slot = &libMap[f_lib];
    void *hnd = *map_slot;
    if (hnd == NULL) {
        hnd = jl_load_dynamic_library(f_lib, JL_RTLD_DEFAULT, throw_err);
        if (hnd != NULL)
            *map_slot = hnd;
    }
    JL_UNLOCK(&libmap_lock);
    return hnd;
}

extern "C" JL_DLLEXPORT
void *jl_load_and_lookup(const char *f_lib, const char *f_name, _Atomic(void*) *hnd)
{
    void *handle = jl_atomic_load_acquire(hnd);
    if (!handle)
        jl_atomic_store_release(hnd, (handle = jl_get_library(f_lib)));
    void * ptr;
    jl_dlsym(handle, f_name, &ptr, 1);
    return ptr;
}

// jl_load_and_lookup, but with library computed at run time on first call
extern "C" JL_DLLEXPORT
void *jl_lazy_load_and_lookup(jl_value_t *lib_val, const char *f_name)
{
    void *lib_ptr;

    if (jl_is_symbol(lib_val))
        lib_ptr = jl_get_library(jl_symbol_name((jl_sym_t*)lib_val));
    else if (jl_is_string(lib_val))
        lib_ptr = jl_get_library(jl_string_data(lib_val));
    else if (jl_libdl_dlopen_func != NULL) {
        // Call `dlopen(lib_val)`; this is the correct path for the `LazyLibrary` case,
        // but it also takes any other value, and so we define `dlopen(x::Any) = throw(TypeError(...))`.
        lib_ptr = jl_unbox_voidpointer(jl_apply_generic(jl_libdl_dlopen_func, &lib_val, 1));
    } else
        jl_type_error("ccall", (jl_value_t*)jl_symbol_type, lib_val);
    void *ptr;
    jl_dlsym(lib_ptr, f_name, &ptr, 1);
    return ptr;
}

// miscellany

extern "C" JL_DLLEXPORT
jl_value_t *jl_get_JIT(void)
{
    const std::string& HostJITName = "ORCJIT";
    return jl_pchar_to_string(HostJITName.data(), HostJITName.size());
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
std::string jl_format_filename(StringRef output_pattern)
{
    std::string buf;
    raw_string_ostream outfile(buf);
    bool special = false;
    char hostname[MAXHOSTNAMELEN + 1];
    uv_passwd_t pwd;
    bool got_pwd = false;
    for (auto c : output_pattern) {
        if (special) {
            if (!got_pwd && (c == 'i' || c == 'd' || c == 'u')) {
                int r = uv_os_get_passwd(&pwd);
                if (r == 0)
                    got_pwd = true;
            }
            switch (c) {
            case 'p':
                outfile << uv_os_getpid();
                break;
            case 'd':
                if (got_pwd)
                    outfile << pwd.homedir;
                break;
            case 'i':
                if (got_pwd)
                    outfile << pwd.uid;
                break;
            case 'l':
            case 'L':
                if (gethostname(hostname, sizeof(hostname)) == 0) {
                    hostname[sizeof(hostname) - 1] = '\0'; /* Null terminate, just to be safe. */
                    outfile << hostname;
                }
#ifndef _OS_WINDOWS_
                if (c == 'l' && getdomainname(hostname, sizeof(hostname)) == 0) {
                    hostname[sizeof(hostname) - 1] = '\0'; /* Null terminate, just to be safe. */
                    outfile << hostname;
                }
#endif
                break;
            case 'u':
                if (got_pwd)
                    outfile << pwd.username;
                break;
            default:
                outfile << c;
                break;
            }
            special = false;
        }
        else if (c == '%') {
            special = true;
        }
        else {
            outfile << c;
        }
    }
    if (got_pwd)
        uv_os_free_passwd(&pwd);
    return outfile.str();
}

extern "C" JL_DLLEXPORT char *jl_format_filename(const char *output_pattern)
{
    return strdup(jl_format_filename(StringRef(output_pattern)).c_str());
}


static uv_mutex_t trampoline_lock; // for accesses to the cache and freelist

static void *trampoline_freelist;

static void *trampoline_alloc() JL_NOTSAFEPOINT // lock taken by caller
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
extern "C" JL_DLLEXPORT
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
        if (cache == HT_NOTFOUND) {
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
                permanent = true;
        }
        if (permanent) {
            result = jl_gc_permobj(sizeof(jl_taggedvalue_t) + jl_datatype_size(result_type), result_type, 0);
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

struct cfuncdata_t {
    _Atomic(void *) fptr;
    _Atomic(size_t) last_world;
    jl_code_instance_t** plast_codeinst;
    jl_code_instance_t* last_codeinst;
    void *unspecialized;
    jl_value_t *const *const declrt;
    jl_value_t *const *const sigt;
    size_t flags;
};

extern "C" JL_DLLEXPORT
void *jl_jit_abi_converter_fallback(jl_task_t *ct, void *unspecialized, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, int specsig,
                                    jl_code_instance_t *codeinst, jl_callptr_t invoke, void *target, int target_specsig)
{
    if (unspecialized)
        return unspecialized;
    jl_errorf("cfunction not available in this build of Julia");
}

static const inline char *name_from_method_instance(jl_method_instance_t *li) JL_NOTSAFEPOINT
{
    return jl_is_method(li->def.method) ? jl_symbol_name(li->def.method->name) : "top-level scope";
}

static jl_mutex_t cfun_lock;
// release jl_world_counter
// store theFptr
// release last_world_v
//
// acquire last_world_v
// read theFptr
// acquire jl_world_counter
extern "C" JL_DLLEXPORT
void *jl_get_abi_converter(jl_task_t *ct, void *data)
{
    cfuncdata_t *cfuncdata = (cfuncdata_t*)data;
    jl_value_t *sigt = *cfuncdata->sigt;
    JL_GC_PROMISE_ROOTED(sigt);
    jl_value_t *declrt = *cfuncdata->declrt;
    JL_GC_PROMISE_ROOTED(declrt);
    bool specsig = cfuncdata->flags & 1;
    size_t nargs = jl_nparams(sigt);
    jl_method_instance_t *mi;
    jl_code_instance_t *codeinst;
    size_t world;
    // check first, while behind this lock, of the validity of the current contents of this cfunc thunk
    JL_LOCK(&cfun_lock);
    do {
        size_t last_world_v = jl_atomic_load_relaxed(&cfuncdata->last_world);
        void *f = jl_atomic_load_relaxed(&cfuncdata->fptr);
        jl_code_instance_t *last_ci = cfuncdata->plast_codeinst ? *cfuncdata->plast_codeinst : nullptr;
        world = jl_atomic_load_acquire(&jl_world_counter);
        ct->world_age = world;
        if (world == last_world_v) {
            JL_UNLOCK(&cfun_lock);
            return f;
        }
        mi = jl_get_specialization1((jl_tupletype_t*)sigt, world, 0);
        if (f != nullptr) {
            if (last_ci == nullptr) {
                if (mi == nullptr) {
                    jl_atomic_store_release(&cfuncdata->last_world, world);
                    JL_UNLOCK(&cfun_lock);
                    return f;
                }
            }
            else {
                if (jl_get_ci_mi(last_ci) == mi && jl_atomic_load_relaxed(&last_ci->max_world) >= world) { // same dispatch and source
                    jl_atomic_store_release(&cfuncdata->last_world, world);
                    JL_UNLOCK(&cfun_lock);
                    return f;
                }
            }
        }
        JL_UNLOCK(&cfun_lock);
        // next, try to figure out what the target should look like (outside of the lock since this is very slow)
        codeinst = mi ? jl_type_infer(mi, world, SOURCE_MODE_ABI) : nullptr;
        // relock for the remainder of the function
        JL_LOCK(&cfun_lock);
    } while (jl_atomic_load_acquire(&jl_world_counter) != world); // restart entirely, since jl_world_counter changed thus jl_get_specialization1 might have changed
    // double-check if the values were set on another thread
    size_t last_world_v = jl_atomic_load_relaxed(&cfuncdata->last_world);
    void *f = jl_atomic_load_relaxed(&cfuncdata->fptr);
    if (world == last_world_v) {
        JL_UNLOCK(&cfun_lock);
        return f; // another thread fixed this up while we were away
    }
    auto assign_fptr = [cfuncdata, world, codeinst](void *f) {
        cfuncdata->plast_codeinst = &cfuncdata->last_codeinst;
        cfuncdata->last_codeinst = codeinst;
        jl_atomic_store_relaxed(&cfuncdata->fptr, f);
        jl_atomic_store_release(&cfuncdata->last_world, world);
        JL_UNLOCK(&cfun_lock);
        return f;
    };
    jl_callptr_t invoke = nullptr;
    if (codeinst != NULL) {
        jl_value_t *astrt = codeinst->rettype;
        if (astrt != (jl_value_t*)jl_bottom_type &&
            jl_type_intersection(astrt, declrt) == jl_bottom_type) {
            // Do not warn if the function never returns since it is
            // occasionally required by the C API (typically error callbacks)
            // even though we're likely to encounter memory errors in that case
            jl_printf(JL_STDERR, "WARNING: cfunction: return type of %s does not match\n", name_from_method_instance(mi));
        }
        uint8_t specsigflags;
        jl_read_codeinst_invoke(codeinst, &specsigflags, &invoke, &f, 1);
        if (invoke != nullptr) {
            if (invoke == jl_fptr_const_return_addr) {
                return assign_fptr(jl_jit_abi_converter(ct, cfuncdata->unspecialized, declrt, sigt, nargs, specsig, codeinst, invoke, nullptr, false));
            }
            else if (invoke == jl_fptr_args_addr) {
                assert(f);
                if (!specsig && jl_subtype(astrt, declrt))
                    return assign_fptr(f);
                return assign_fptr(jl_jit_abi_converter(ct, cfuncdata->unspecialized, declrt, sigt, nargs, specsig, codeinst, invoke, f, false));
            }
            else if (specsigflags & 0b1) {
                assert(f);
                if (specsig && jl_egal(mi->specTypes, sigt) && jl_egal(declrt, astrt))
                    return assign_fptr(f);
                return assign_fptr(jl_jit_abi_converter(ct, cfuncdata->unspecialized, declrt, sigt, nargs, specsig, codeinst, invoke, f, true));
            }
        }
    }
    f = jl_jit_abi_converter(ct, cfuncdata->unspecialized, declrt, sigt, nargs, specsig, codeinst, invoke, nullptr, false);
    if (codeinst == nullptr)
        cfuncdata->unspecialized = f;
    return assign_fptr(f);
}

void jl_init_runtime_ccall(void)
{
    JL_MUTEX_INIT(&libmap_lock, "libmap_lock");
    uv_mutex_init(&trampoline_lock);
}
