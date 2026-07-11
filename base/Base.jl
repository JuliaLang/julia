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
include("weakkeydict.jl")

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
# reused); this Ref keeps it rooted while the C side mirrors a raw pointer to
# it for async-signal-safe reads.
const _sigint_source = Ref{Union{Nothing, CancellationTokenSource}}(nothing)
# The task driving the current foreground evaluation (the caller of
# sigint_new_episode!). The escalation ladder falls back to targeting it
# directly when the running computation has no published token binding (e.g.
# an interpreted top-level loop, which executes no compiled cancellation
# points).
const _sigint_foreground_task = Ref{Union{Nothing, Task}}(nothing)

"""
    Base.sigint_new_episode!() -> CancellationToken

Install a fresh ^C episode source and return its token. The owner of an
interactive session (e.g. the REPL backend, or the script driver) calls this
before each foreground evaluation and runs the evaluation in a dynamic scope
carrying the returned token (`@with Base.CANCEL_TOKEN => tok ...`), so that
^C cancels exactly that evaluation. Also stands down the ^C escalation
machinery of the previous episode.
"""
function sigint_new_episode!()
    src = CancellationTokenSource()
    _sigint_source[] = src
    _sigint_foreground_task[] = current_task()
    # also disarms the rescue timer and clears a stale pending marker
    ccall(:jl_set_sigint_source, Cvoid, (Any,), src)
    return CancellationToken(src)
end

# Close the ^C episode without installing a new source (e.g. after an
# abandoned target was cleaned up; the resumed session installs a fresh
# source before its next evaluation).
function sigint_close_episode!()
    _sigint_source[] = nothing
    _sigint_foreground_task[] = nothing
    ccall(:jl_set_sigint_source, Cvoid, (Any,), nothing)
    nothing
end

# The active severity of the episode source, or `nothing` if it has not been
# cancelled.
function sigint_active_severity(src::CancellationTokenSource)
    st = @atomic :acquire src.state
    st == 0x00 && return nothing
    r = st & SEVERITY_MASK
    r == 0x03 && return CANCEL_REQUEST_ABANDON_EXTERNAL
    r == 0x04 && return CANCEL_REQUEST_ABANDON_ALL
    return CANCEL_REQUEST_SAFE
end

# One listener task runs per nonempty threadpool (a pool's threads cannot run
# another pool's tasks, and any pool may be monopolized by the stuck victim -
# e.g. a rescued backend on the only default thread). The listeners race to
# claim each notification; the pass itself is level-based (it re-reads the
# episode state), so serializing claims through the lock is enough.
const sigint_pass_lock = ReentrantLock()

function sigint_listener(cond::AsyncCondition)
    while _trywait(cond)
      # Claim the notification - the winner also tells idle threads they no
      # longer need to help dispatch it (see jl_sigint_dispatch_pending in
      # the scheduler); the losing pool's listener just re-parks.
      ccall(:jl_claim_sigint_dispatch, Cint, ()) != 0 || continue
      lock(sigint_pass_lock)
      try # an error while processing one ^C must not disable the ^C machinery
        pending = ccall(:jl_consume_sigint_pending, Cint, ()) != 0
        # If the current episode's stuck task was already forcibly abandoned
        # (by our ABANDON_ALL escalation below, or by the C-side fallback when
        # no thread was available to run this listener), finish its cleanup
        # instead of delivering anything new.
        backend = active_repl_backend
        abandoned = nothing
        if backend !== nothing && (backend.backend_task::Task).state === :abandoned
            abandoned = backend.backend_task::Task
        elseif roottask.state === :abandoned && (backend === nothing || istaskdone(backend.backend_task::Task))
            # N.B.: an abandoned root task is the *permanent* state of a
            # rescued session (with the default REPL, evaluation initially
            # runs on the root task); it only needs cleanup while there is no
            # live backend to hand the session to.
            abandoned = roottask
        end
        if abandoned !== nothing
            cleanup_abandoned_sigint_target(abandoned)
            continue
        end
        src = _sigint_source[]
        if src === nothing
            # No episode source is installed. In an interactive session this
            # is a between-evaluations window (a fresh source arrives with the
            # next prompt) - ignore the press. Without a live interactive
            # evaluator nothing can be cancelled or resumed - exit as an
            # unhandled ^C would.
            pending || continue
            if backend !== nothing && !istaskdone(backend.backend_task::Task)
                continue
            end
            roottask.state === :abandoned || istaskdone(roottask) || continue
            exit(128 + 2) # 128 + SIGINT
        end
        active = sigint_active_severity(src)
        # has any task acknowledged (started handling) the active severity?
        delivered_bits = @atomic :monotonic src.delivered
        acked = active !== nothing && (delivered_bits & (0x01 << active.request)) != 0x00
        if active === nothing
            sev = CANCEL_REQUEST_SAFE
        elseif ccall(:jl_sigint_rescue_timer_expired_peek, Cint, ()) != 0
            # The grace period for the active severity has passed (the rescue
            # timer announced the escalation option to the user) - climb one
            # rung, and start a fresh grace period for the next one.
            sev = active === CANCEL_REQUEST_SAFE ? CANCEL_REQUEST_ABANDON_EXTERNAL :
                  CANCEL_REQUEST_ABANDON_ALL
            ccall(:jl_sigint_escalation_delivered, Cvoid, ())
        elseif !acked
            # The active severity was never observed by any task - a repeat
            # ^C within the grace period retries its delivery.
            sev = active
        elseif !pending
            # The cancellation was acknowledged and is being handled; a
            # spurious wakeup without a fresh press does nothing (once the
            # grace period lapses, the rescue timer offers escalation).
            continue
        else
            # Acknowledged, but this wakeup carries a real press. The press
            # may also have been the FIRST one, delivered entirely by the
            # C-side fast path (the episode source marked and the signal
            # dispatch acknowledged by a polling task) with no walk ever
            # run - parked waiters under the episode source still need
            # their wake. Redeliver: level-triggered and idempotent, it
            # wakes whoever is still parked and is a no-op for tasks
            # already unwinding.
            sev = active
        end
        # N.B.: these warnings must not park this task (the victim may be
        # hogging our only thread) - Core.print writes directly.
        if sev === CANCEL_REQUEST_ABANDON_ALL && active !== CANCEL_REQUEST_ABANDON_ALL
            Core.print(Core.stderr, "\nWARNING: Abandoning the current task.\n",
                            "         This may leave the process in an inconsistent state.\n")
        elseif sev === CANCEL_REQUEST_ABANDON_EXTERNAL && active === CANCEL_REQUEST_SAFE
            Core.print(Core.stderr, "\nWARNING: No longer waiting for external resources to complete cancellation.\n")
        end
        if sev === active
            redeliver!(src)
        else
            cancel!(src, sev)
        end
        # Fallback for a foreground task running code without a published
        # token binding (e.g. an interpreted top-level loop): target it
        # directly.
        fg = _sigint_foreground_task[]
        if fg isa Task && !istaskdone(fg)
            if sev === CANCEL_REQUEST_ABANDON_ALL
                freeze_task!(fg, sev, src)
            else
                # best-effort acceleration of asynchronous delivery
                tid = ccall(:jl_get_task_tid, Int16, (Any,), fg)
                tid >= 0 && ccall(:jl_send_cancellation_signal, Cvoid, (Int16,), tid)
            end
        end
        # The escalation timer keeps running until the episode closes (the
        # session resumes normal operation and installs a fresh episode
        # source, or an abandoned target is cleaned up): if the cancellation
        # stalls at any stage - including cleanup after a successful
        # delivery - the timer offers the next rung.
        if (REPL = REPL_MODULE_REF[]) !== Base
            invokelatest(REPL.maybe_rescue_REPL_after_sigint)
        end
        # Our own ABANDON_ALL rung may just have frozen the foreground task.
        backend = active_repl_backend
        if backend !== nothing && (backend.backend_task::Task).state === :abandoned
            cleanup_abandoned_sigint_target(backend.backend_task::Task)
        elseif roottask.state === :abandoned && (backend === nothing || istaskdone(backend.backend_task::Task))
            cleanup_abandoned_sigint_target(roottask)
        end
      catch ex
        try
            @invokelatest showerror(stderr, ex, catch_backtrace())
            println(stderr)
        catch
        end
      finally
        unlock(sigint_pass_lock)
      end
    end
    nothing
end

# Post-abandonment cleanup for a ^C target that was forcibly frozen (by the
# ABANDON_ALL escalation rung, or by the C-side fallback).
function cleanup_abandoned_sigint_target(t::Task)
    # A forcibly abandoned task never goes through the regular task completion
    # path, so wake up anyone waiting on it explicitly. (The root task's
    # donenotify may be `nothing`.)
    donenotify = t.donenotify
    if donenotify isa ThreadSynchronizer
        lock(donenotify)
        notify(donenotify)
        unlock(donenotify)
    end
    if (REPL = REPL_MODULE_REF[]) !== Base
        invokelatest(REPL.maybe_rescue_REPL_after_sigint)
    end
    # The episode is resolved: stand down the escalation timer and close the
    # episode so that the next ^C starts fresh (the resumed session installs
    # a new episode source before its next evaluation).
    sigint_close_episode!()
    # If the abandonment left the process without an interactive evaluator
    # (script mode, or the REPL could not be rescued), nothing can resume
    # normal operation - exit as ^C would.
    backend = active_repl_backend
    if t === roottask && (backend === nothing || istaskdone(backend.backend_task::Task))
        exit(128 + 2) # 128 + SIGINT
    end
    nothing
end

# The rescue task exists to give a thread whose current task got forcibly
# abandoned (because it failed to acknowledge ^C) something runnable to switch
# to; it simply enters the scheduler.
function sigint_rescue_loop()
    while true
        wait()
    end
end

# Keeps the rescue task rooted while the C runtime holds a reference to it
const _sigint_rescue_task = Ref{Union{Task, Nothing}}(nothing)
# The listener task itself, for diagnostics.
const _sigint_listener_task = Ref{Union{Task, Nothing}}(nothing)

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
    _sigint_listener_task[] = listeners[end]
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
    rescue = Task(sigint_rescue_loop)
    rescue.sticky = false
    _sigint_rescue_task[] = rescue
    ccall(:jl_set_sigint_rescue_task, Cvoid, (Any,), rescue)
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
