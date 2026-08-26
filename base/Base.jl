# This file is a part of Julia. License is MIT: https://julialang.org/license

const start_base_include = time_ns()

include("reflection.jl")
include("refpointer.jl")

# now replace the Pair constructor (relevant for NamedTuples) with one that calls our Base.convert
delete_method(which(Pair{Any,Any}, (Any, Any)))
@eval function (P::Type{Pair{A, B}})(@nospecialize(a), @nospecialize(b)) where {A, B}
    @inline
    return $(Expr(:new, :P, :(a isa A ? a : convert(A, a)), :(b isa B ? b : convert(B, b))))
end

# The REPL stdlib hooks into Base using this Ref
const REPL_MODULE_REF = Ref{Module}(Base)
process_sysimg_args!()

include(strcat(BUILDROOT, "build_h.jl"))     # include($BUILDROOT/base/build_h.jl)
include(strcat(BUILDROOT, "version_git.jl")) # include($BUILDROOT/base/version_git.jl)

# Initialize DL_LOAD_PATH as early as possible.  We are defining things here in
# a slightly more verbose fashion than usual, because we're running so early.
let os = ccall(:jl_get_UNAME, Any, ())
    if os === :Darwin || os === :Apple
        if DARWIN_FRAMEWORK
            push!(DL_LOAD_PATH, "@loader_path/Frameworks")
        end
        push!(DL_LOAD_PATH, "@loader_path")
    end
end

# metaprogramming
include("meta.jl")
using .Meta
using .Meta: is_id_char

# Strings
include("multimedia.jl")
using .Multimedia

include("char.jl")
function array_new_memory(mem::Memory{UInt8}, newlen::Int)
    # add an optimization to array_new_memory for StringVector
    if (@assume_effects :total @ccall jl_genericmemory_owner(mem::Any,)::Any) === mem
        # TODO: when implemented, this should use a memory growing call
        return typeof(mem)(undef, newlen)
    else
        # If data is in a String, keep it that way.
        # When implemented, this could use jl_gc_expand_string(oldstr, newlen) as an optimization
        str = _string_n(newlen)
        return (@assume_effects :total !:consistent @ccall jl_string_to_genericmemory(str::Any,)::Memory{UInt8})
    end
end
include("strings/basic.jl")
include("strings/string.jl")
include("strings/substring.jl")
include("strings/cstring.jl")

include("cartesian.jl")
using .Cartesian
include("hashing.jl")
include("osutils.jl")

# subarrays
include("subarray.jl")
include("views.jl")

# String views
include("strings/stringview.jl")

# numeric operations
include("div.jl")
include("twiceprecision.jl")
include("complex.jl")
include("rational.jl")
include("multinverses.jl")
using .MultiplicativeInverses
include("abstractarraymath.jl")
include("arraymath.jl")
include("slicearray.jl")

# SIMD loops
sizeof(s::String) = Core.sizeof(s)  # needed by gensym as called from simdloop
include("simdloop.jl")
using .SimdLoop

# map-reduce operators
include("reduce.jl")

## core structures
include("reshapedarray.jl")
include("reinterpretarray.jl")

# Some type
include("some.jl")

include("dict.jl")
include("set.jl")

# Dynamic scopes (types only; the ScopedValues API is included much later)
include("scope.jl")
# Cancellation tokens (the `cancel` keyword-argument machinery is used from
# the I/O layer onwards)
include("cancellation.jl")

# Core I/O
include("io.jl")
include("iobuffer.jl")

# Concurrency (part 1)
include("linked_list.jl")
include("park.jl")
include("condition.jl")
include("threads.jl")
include("lock.jl")

# strings & printing
include("intfuncs.jl")
include("strings/strings.jl")

#=
isdebugbuild is defined here as this is imported in libdl.jl (included in libc.jl)
=#
"""
    isdebugbuild()

Return `true` if julia is a debug version.
"""
function isdebugbuild()
    return ccall(:jl_is_debugbuild, Cint, ()) != 0
end

# Enable dynamic library loading
module Sys end # Sys is populated in stages during bootstrap
Core.eval(Sys, :(include("osinfo.jl")))
module Filesystem end # Filesystem is populated in stages during bootstrap
Core.eval(Filesystem, :(include("path.jl")))
using .Filesystem
include("libc.jl") # Libdl (include in libc.jl) is required for regex.jl
using .Libc: getpid, gethostname, time, memcpy, memset, memmove, memcmp

# More strings & printing
include("regex.jl")
include("parse.jl")
include("shell.jl")
const IRShow = Compiler.IRShow # an alias for compatibility
include("stacktraces.jl")
using .StackTraces
include("show.jl")
include("arrayshow.jl")
include("methodshow.jl")

# multidimensional arrays
include("multidimensional.jl")

include("broadcast.jl")
using .Broadcast
using .Broadcast: broadcasted, broadcasted_kwsyntax, materialize, materialize!,
                  broadcast_preserving_zero_d, andand, oror

# missing values
include("missing.jl")

# version
include("version.jl")

# system & environment
Core.eval(Sys, :(include("sysinfo.jl")))

const USING_STOCK_GC = occursin("stock", GC.gc_active_impl())

# These used to be in build_h.jl and are retained for backwards compatibility.
# NOTE: keep in sync with `libblastrampoline_jll.libblastrampoline`.
const libblas_name = "libblastrampoline" * (Sys.iswindows() ? "-5" : "")
const liblapack_name = libblas_name

# Concurrency (part 2)
# Note that `atomics.jl` here should be deprecated
Core.eval(Threads, :(include("atomics.jl")))
include("channels.jl")
include("partr.jl")
include("task.jl")
include("threads_overloads.jl")
include("weakdict.jl")

# ScopedValues
include("scopedvalues.jl")

# Logging
include("logging/logging.jl")
using .CoreLogging

include("env.jl")

# functions defined in Random
function rand end
function randn end

# I/O
include("libuv.jl")
include("asyncevent.jl")
include("iostream.jl")
include("stream.jl")
Core.eval(Filesystem, :(include("filesystem.jl")))
include("cmd.jl")
include("process.jl")
include("terminfo.jl")
include("Terminals.jl") # Moved from REPL to reduce invalidations
include("secretbuffer.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
using .Math
const (√)=sqrt
const (∛)=cbrt
const (∜)=fourthroot

# now switch to a simple, race-y TLS, relative include for the rest of Base
delete_method(which(include, (Module, String)))
let SOURCE_PATH = ""
    global function include(mod::Module, path::String)
        prev = SOURCE_PATH::String
        path = normpath(joinpath(dirname(prev), path))
        Core.println(path)
        ccall(:jl_uv_flush, Nothing, (Ptr{Nothing},), Core.io_pointer(Core.stdout))
        push!(_included_files, (mod, abspath(path)))
        SOURCE_PATH = path
        result = Core.include(mod, path)
        SOURCE_PATH = prev
        return result
    end
end

# reduction along dims
include("reducedim.jl")  # macros in this file rely on string.jl
include("accumulate.jl")

include("permuteddimsarray.jl")
using .PermutedDimsArrays

# Combinatorics
include("sort.jl")
using .Sort

# Fast math
include("fastmath.jl")
using .FastMath

function deepcopy_internal end

# enums
include("Enums.jl")
using .Enums

# BigInts
include("gmp.jl")
using .GMP

# float printing: requires BigInt
include("ryu/Ryu.jl")
using .Ryu

# BigFloats
include("mpfr.jl")
using .MPFR

include("combinatorics.jl")

# irrational mathematical constants
include("irrationals.jl")
include("mathconstants.jl")
using .MathConstants: ℯ, π, pi

# experimental API's
include("experimental.jl")

# utilities
include("deepcopy.jl")
include("download.jl")
include("summarysize.jl")
include("errorshow.jl")
include("util.jl")

include("initdefs.jl")

# worker threads
include("threadcall.jl")

# code loading
include("uuid.jl")
include("pkgid.jl")
include("toml/toml.jl")
include("linking.jl")
include("loading.jl")

# BinaryPlatforms, used by Artifacts.  Needs `Sort`.
include("binaryplatforms.jl")

# misc useful functions & macros
include("timing.jl")
include("client.jl")
include("asyncmap.jl")

# deprecated functions
include("deprecated.jl")
#
# Some additional basic documentation
include("docs/basedocs.jl")

# Documentation -- should always be included last in sysimg.
include("docs/Docs.jl")
using .Docs
Docs.loaddocs(CoreDocs.DOCS)
@eval CoreDocs DOCS = DocLinkedList()

include("precompilation.jl")

# finally, now make `include` point to the full version
for m in methods(include)
    delete_method(m)
end
for m in methods(IncludeInto(Base))
    delete_method(m)
end

# This method is here only to be overwritten during the test suite to test
# various sysimg related invalidation scenarios.
a_method_to_overwrite_in_test() = inferencebarrier(1)

# These functions are duplicated in client.jl/include(::String) for
# nicer stacktraces. Modifications here have to be backported there
@noinline include(mod::Module, _path::AbstractString) = _include(identity, mod, _path)
@noinline include(mapexpr::Function, mod::Module, _path::AbstractString) = _include(mapexpr, mod, _path)
(this::IncludeInto)(fname::AbstractString) = include(identity, this.m, fname)
(this::IncludeInto)(mapexpr::Function, fname::AbstractString) = include(mapexpr, this.m, fname)

# Compatibility with when Compiler was in Core
@eval Core const Compiler = $Base.Compiler
@eval Compiler const fl_parse = $Base.fl_parse

# Compiler frontend
Core.println("JuliaSyntax/src/JuliaSyntax.jl")
include(@__MODULE__, string(DATAROOT, "julia/JuliaSyntax/src/JuliaSyntax.jl"))
# May be replaced in incremental sysimage build after-the-fact
const JuliaLowering = nothing

# Now that JuliaSyntax is bootstrapped and ready to use, set Base's syntax version.
set_syntax_version(Base, VERSION)

end_base_include = time_ns()

Filesystem.__postinit__()
const _sysimage_modules = PkgId[]
in_sysimage(pkgid::PkgId) = pkgid in _sysimage_modules

if is_primary_base_module

# Profiling helper
# triggers printing the report and (optionally) saving a heap snapshot after a SIGINFO/SIGUSR1 profile request
# Needs to be in Base because Profile is no longer loaded on boot
function profile_printing_listener(cond::AsyncCondition)
    profile = nothing
    try
        while _trywait(cond)
            profile = @something(profile, require_stdlib(PkgId(UUID("9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"), "Profile")))::Module
            invokelatest(profile.peek_report[])
            if get_bool_env("JULIA_PROFILE_PEEK_HEAP_SNAPSHOT", false) === true
                println(stderr, "Saving heap snapshot...")
                fname = invokelatest(profile.take_heap_snapshot)
                println(stderr, "Heap snapshot saved to `$(fname)`")
            end
        end
    catch ex
        if !isa(ex, InterruptException)
            @error "Profile printing listener crashed" exception=ex,catch_backtrace()
        end
    end
    nothing
end

function start_profile_listener()
    cond = AsyncCondition()
    uv_unref(cond.handle)
    t = errormonitor(Threads.@spawn(profile_printing_listener(cond)))
    atexit() do
        # destroy this callback when exiting
        ccall(:jl_set_peek_cond, Cvoid, (Ptr{Cvoid},), C_NULL)
        # this will prompt any ongoing or pending event to flush also
        close(cond)
        # error-propagation is not needed, since the errormonitor will handle printing that better
        t === current_task() || _wait(t)
    end
    finalizer(cond) do c
        # if something goes south, still make sure we aren't keeping a reference in C to this
        ccall(:jl_set_peek_cond, Cvoid, (Ptr{Cvoid},), C_NULL)
    end
    ccall(:jl_set_peek_cond, Cvoid, (Ptr{Cvoid},), cond.handle)
end

# The ^C episode source: the cancellation token source governing the current
# interactive foreground evaluation. A fresh source is installed per
# evaluation (severities are monotonic, so a cancelled source is never
# reused); this Ref roots it while the C side mirrors a raw pointer for the
# signal thread's lock-free reads.
# The current ^C episode: the governing source (rooted here; `nothing`
# between episodes) paired with the C-side episode generation the mirror
# swap returned - the dispatch pass consumes a pending press only when it
# targeted this very generation, so a press for a previous episode can
# never cancel a newly installed one.
const _sigint_episode = Ref{Tuple{Union{Nothing, CancellationTokenSource}, UInt64}}((nothing, 0))
# The task driving the current foreground evaluation (the caller of
# sigint_new_episode!). Nothing in Base reads it yet: the ^C escalation
# machinery (follow-up PR) targets it directly when the running computation
# has no published token binding, and Distributed points it at the handler
# of the most recent remotely-submitted request.
const _sigint_foreground_task = Ref{Union{Nothing, Task}}(nothing)

"""
    Base.sigint_new_episode!([src::CancellationTokenSource]) -> CancellationToken

Install `src` (a fresh standalone source by default) as the ^C episode
source and return its token. The owner of an interactive session (e.g. the
REPL backend, or the script driver) calls this before each foreground
evaluation and runs the evaluation in a dynamic scope carrying the returned
token (`@with Base.CANCEL_TOKEN => tok ...`), so that ^C cancels exactly
that evaluation.

The caller chooses the source's place in the cancellation graph. The REPL
passes a fresh *evaluation* source - a child of the session source (see the
session tree in base/client.jl) - so work an evaluation leaves behind stays
sweepable via [`cancel_session_work!`](@ref) after its episode closes. The
session-covering episode installed by `_start` is deliberately a standalone
root: everything outside per-evaluation scopes (including the REPL's own
machinery) runs under it, and a session sweep must never cancel that.
"""
function sigint_new_episode!(src::CancellationTokenSource=CancellationTokenSource())
    _sigint_foreground_task[] = current_task()
    # Publish the C-side mirror. The episode Ref roots the new source; the
    # signal thread's use of the mirror is GC-excluded rather than rooted,
    # so nothing needs to keep the outgoing source alive (see
    # jl_sigint_request_cancellation).
    gen = ccall(:jl_set_sigint_source, UInt64, (Any,), src)
    _sigint_episode[] = (src, gen)
    return CancellationToken(src)
end

# Close the ^C episode without installing a new source (e.g. when the work
# item completes; the session installs a fresh source before its next
# evaluation).
function sigint_close_episode!()
    gen = ccall(:jl_set_sigint_source, UInt64, (Any,), nothing)
    _sigint_episode[] = (nothing, gen)
    _sigint_foreground_task[] = nothing
    nothing
end

# The active severity of the episode source, or `nothing` if it has not
# been cancelled (an alias kept for the ^C machinery's consumers).
sigint_active_severity(src::CancellationTokenSource) = cancel_severity(src)

# One listener task runs per nonempty threadpool (a pool's threads cannot run
# another pool's tasks, and any pool may be monopolized by a busy victim).
# The listeners race to claim each notification; the pass itself is
# level-based (it re-reads the episode state), so serializing claims through
# the lock is enough.
const sigint_pass_lock = ReentrantLock()

# One dispatch pass for a claimed ^C notification. Runs with
# `sigint_pass_lock` held; level-based (it re-reads the episode state), so
# serializing passes through the lock is enough.
function _sigint_dispatch_pass()
    src, gen = _sigint_episode[]
    # A press is consumed only if it targeted this episode's generation; a
    # press for a previous episode self-invalidates (its source was already
    # marked by the C fast path), so a late pass can never cancel a newly
    # installed episode (issues #58689, #42072).
    pending = ccall(:jl_consume_sigint_pending, Cint, (UInt64,), gen) != 0
    if src === nothing
        # No episode source is installed. In an interactive session this
        # is a between-evaluations window (a fresh source arrives with the
        # next prompt) - ignore the press. Without a live interactive
        # evaluator nothing can be cancelled or resumed - exit as an
        # unhandled ^C would.
        pending || return
        backend = active_repl_backend
        if backend !== nothing && !istaskdone(backend.backend_task::Task)
            return
        end
        istaskdone(roottask) || return
        exit(128 + 2) # 128 + SIGINT
    end
    src = src::CancellationTokenSource
    if sigint_active_severity(src) === nothing
        # The source is unmarked: only a press that targeted exactly this
        # episode warrants delivering to it.
        pending || return
        cancel!(src, CANCEL_REQUEST_SAFE)
    elseif pending
        # The press was delivered entirely by the C-side fast path with
        # no wake-up walk ever run - or this is a repeat press. Parked
        # waiters under the episode source still need their wake;
        # redeliver! is level-triggered and idempotent (a no-op for
        # tasks already unwinding).
        redeliver!(src)
    end
    # else: a wakeup without a press for this episode does nothing
    nothing
end

# Arbitrate and run pending ^C dispatch. Called from the sigint listener
# tasks on their async notification, and inline from any idle thread's
# scheduler loop (see jl_dispatch_sigint_inline in src/scheduler.c) - the
# pass only needs an ordinary task context, which keeps the event loop out
# of the delivery path even when its owning thread is stuck in a foreign
# call. `trylock` (never a parking `lock`): if another pass is running, it
# drains any claim posted meanwhile, and an unclaimed flag is retried by
# the next idle iteration or listener wakeup.
function maybe_dispatch_sigint()
    ccall(:jl_peek_sigint_dispatch, Cint, ()) != 0 || return
    trylock(sigint_pass_lock) || return
    try
        while ccall(:jl_claim_sigint_dispatch, Cint, ()) != 0
            try # an error in one pass must not disable the ^C machinery
                _sigint_dispatch_pass()
            catch ex
                try
                    @invokelatest showerror(stderr, ex, catch_backtrace())
                    println(stderr)
                catch
                end
            end
        end
    finally
        unlock(sigint_pass_lock)
    end
    nothing
end

function sigint_listener(cond::AsyncCondition)
    while _trywait(cond)
        maybe_dispatch_sigint()
    end
    nothing
end

# The dispatch path must not hit the JIT on the first ^C: it runs from the
# scheduler's idle loop, and a press on a cold or loaded session would
# stall interactive response behind the compile - bake it into the image.
precompile(Tuple{typeof(maybe_dispatch_sigint)})
precompile(Tuple{typeof(_sigint_dispatch_pass)})
precompile(Tuple{typeof(cancel!), CancellationTokenSource, CancellationRequest})
precompile(Tuple{typeof(redeliver!), CancellationTokenSource})

function start_sigint_listener()
    cond = AsyncCondition()
    # N.B.: The condition is deliberately kept ref'd: pending async events on
    # unreferenced handles are not dispatched once the loop has no live
    # handles left (as in a headless script), which would make the ^C
    # notification undeliverable exactly when it matters. The atexit hook
    # below closes the handle before the event loop is drained for exit.
    listeners = Task[]
    Threads.threadpoolsize(:interactive) > 0 &&
        push!(listeners, errormonitor(Threads.@spawn :interactive sigint_listener(cond)))
    push!(listeners, errormonitor(Threads.@spawn :default sigint_listener(cond)))
    atexit() do
        # destroy this callback when exiting
        ccall(:jl_set_sigint_cond, Cvoid, (Ptr{Cvoid},), C_NULL)
        # this will prompt any ongoing or pending event to flush also
        close(cond)
        # error-propagation is not needed, since the errormonitor will handle printing that better
        for t in listeners
            t === current_task() || _wait(t)
        end
    end
    finalizer(cond) do c
        # if something goes south, still make sure we aren't keeping a reference in C to this
        ccall(:jl_set_sigint_cond, Cvoid, (Ptr{Cvoid},), C_NULL)
    end
    ccall(:jl_set_sigint_cond, Cvoid, (Ptr{Cvoid},), cond.handle)
end

function __init__()
    # Base library init
    global _atexit_hooks_finished = false
    Filesystem.__postinit__()
    reinit_stdio()
    Multimedia.reinit_displays() # since Multimedia.displays uses stdout as fallback
    # initialize loading
    init_depot_path()
    init_load_path()
    init_active_project()
    append!(empty!(_sysimage_modules), keys(loaded_modules))
    empty!(loaded_precompiles) # If we load a packageimage when building the image this might not be empty
    for mod in loaded_modules_order
        push!(get!(Vector{Module}, loaded_precompiles, PkgId(mod)), mod)
    end
    if haskey(ENV, "JULIA_MAX_NUM_PRECOMPILE_FILES")
        MAX_NUM_PRECOMPILE_FILES[] = parse(Int, ENV["JULIA_MAX_NUM_PRECOMPILE_FILES"])
    end
    # Profiling helper
    @static if !Sys.iswindows()
        # triggering a profile via signals is not implemented on windows
        start_profile_listener()
    end
    start_sigint_listener()
    _require_world_age[] = get_world_counter()
    # Prevent spawned Julia process from getting stuck waiting on Tracy to connect.
    delete!(ENV, "JULIA_WAIT_FOR_TRACY")
    if get_bool_env("JULIA_USE_FLISP_PARSER", false) === false
        JuliaSyntax.enable_in_core!()
    end
    if JuliaLowering !== nothing && get_bool_env("JULIA_USE_FLISP_LOWERING", true) === false
        # This is not available by default, but JuliaLowering can be added to
        # Base after-the-fact via an incremental sysimage build.
        JuliaLowering.activate!()
    end

    CoreLogging.global_logger(CoreLogging.ConsoleLogger())
    nothing
end

# enable threads support
@eval PCRE PCRE_COMPILE_LOCK = Threads.SpinLock()

# Record dependency information for files belonging to the Compiler, so that
# we know whether the .ji can just give the Base copy or not.
# TODO: We may want to do this earlier to avoid TOCTOU issues.
const _compiler_require_dependencies = Any[]
@Core.latestworld
for i = 1:length(_included_files)
    (mod, file) = _included_files[i]
    if mod === Compiler || parentmodule(mod) === Compiler || endswith(file, "/Compiler.jl")
        _include_dependency!(_compiler_require_dependencies, true, mod, file, true, false)
    end
end
# Make relative to DATAROOTDIR to allow relocation
let basedir = joinpath(Sys.BINDIR, DATAROOTDIR)
for i = 1:length(_compiler_require_dependencies)
    tup = _compiler_require_dependencies[i]
    _compiler_require_dependencies[i] = (tup[1], relpath(tup[2], basedir), tup[3:end]...)
end
end
@assert length(_compiler_require_dependencies) >= 15

end
