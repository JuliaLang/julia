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
# reused); this Ref roots it while the C side mirrors a raw pointer for the
# signal thread's lock-free reads.
# The current ^C episode: the governing source (rooted here; `nothing`
# between episodes) paired with the C-side episode generation the mirror
# swap returned - the dispatch pass consumes a pending press only when it
# targeted this very generation, so a press for a previous episode can
# never cancel a newly installed one.
const _sigint_episode = Ref{Tuple{Union{Nothing, CancellationTokenSource}, UInt64}}((nothing, 0))
# The task driving the current foreground evaluation (the caller of
# sigint_new_episode!). The escalation ladder falls back to targeting it
# directly when the running computation has no published token binding (e.g.
# an interpreted top-level loop, which executes no compiled cancellation
# points), and Distributed points it at the handler of the most recent
# remotely-submitted request.
const _sigint_foreground_task = Ref{Union{Nothing, Task}}(nothing)

"""
    Base.sigint_new_episode!([src::CancellationTokenSource]) -> CancellationToken

Install `src` (a fresh standalone source by default) as the ^C episode
source and return its token. The owner of an interactive session (e.g. the
REPL backend, or the script driver) calls this before each foreground
evaluation and runs the evaluation in a dynamic scope carrying the returned
token (`@with Base.CANCEL_TOKEN => tok ...`), so that ^C cancels exactly
that evaluation. Also stands down the ^C escalation machinery of the
previous episode.

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
    # Publish the C-side mirrors (rooted by the Refs above; the signal
    # thread's use of the source mirror is GC-excluded rather than rooted,
    # see jl_sigint_request_cancellation). The foreground task must be
    # published before the source: the source mirror is what arms the ^C
    # machinery on this episode. The source swap also disarms the previous
    # episode's rescue timer.
    ccall(:jl_set_sigint_foreground_task, Cvoid, (Any,), current_task())
    gen = ccall(:jl_set_sigint_source, UInt64, (Any,), src)
    _sigint_episode[] = (src, gen)
    return CancellationToken(src)
end

# Close the ^C episode without installing a new source (e.g. after an
# abandoned target was cleaned up; the resumed session installs a fresh
# source before its next evaluation).
function sigint_close_episode!()
    # Clear the source mirror first: it is the arming word - the fg-task
    # mirror must never outlive it into a window where it could be consulted
    # for a closed episode.
    gen = ccall(:jl_set_sigint_source, UInt64, (Any,), nothing)
    _sigint_episode[] = (nothing, gen)
    ccall(:jl_set_sigint_foreground_task, Cvoid, (Any,), nothing)
    _sigint_foreground_task[] = nothing
    nothing
end

# The active severity of the episode source, or `nothing` if it has not
# been cancelled (an alias kept for the ^C machinery's consumers).
sigint_active_severity(src::CancellationTokenSource) = cancel_severity(src)

# Serializes ^C dispatch passes (all run on the adopted signal thread; the
# lock is an invariant guard, not an arbitration point).
const sigint_pass_lock = ReentrantLock()

# One dispatch pass for a claimed ^C notification, run as ordinary Julia
# code directly on the adopted signal thread (see jl_run_sigint_dispatch in
# src/signal-handling.c). Runs with `sigint_pass_lock` held, in a full task
# context - the abandonment protocol and session cleanup may block; signals
# arriving meanwhile stay pending and coalesce. Level-based (it re-reads
# the episode state), so serializing passes through the lock is enough.
function _sigint_dispatch_pass()
    src, gen = _sigint_episode[]
    # A press is consumed only if it targeted this episode's generation; a
    # press for a previous episode self-invalidates (its source was already
    # marked by the C fast path), so a late pass can never cancel a newly
    # installed episode (issues #58689, #42072).
    pending = ccall(:jl_consume_sigint_pending, Cint, (UInt64,), gen) != 0
    # If the current episode's stuck task was already forcibly abandoned (by
    # the ABANDON_ALL escalation below, or by the C-side fallback when no
    # thread was available to run a dispatch pass), finish its cleanup
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
        return
    end
    if src === nothing
        # No episode source is installed. In an interactive session this
        # is a between-evaluations window (a fresh source arrives with the
        # next prompt) - ignore the press. Without a live interactive
        # evaluator nothing can be cancelled or resumed - exit as an
        # unhandled ^C would.
        pending || return
        if backend !== nothing && !istaskdone(backend.backend_task::Task)
            return
        end
        roottask.state === :abandoned || istaskdone(roottask) || return
        Core.print(Core.stderr, "\n[DEBUG sigint] dispatch-pass exit: no evaluator left\n")
        exit(128 + 2) # 128 + SIGINT
    end
    src = src::CancellationTokenSource
    active = sigint_active_severity(src)
    # has any task acknowledged (started handling) the active severity?
    delivered_bits = @atomic :monotonic src.delivered
    acked = active !== nothing && (delivered_bits & (0x01 << active.request)) != 0x00
    if active === nothing
        # The source is unmarked: only a press that targeted exactly this
        # episode warrants delivering to it.
        pending || return
        sev = CANCEL_REQUEST_SAFE
    elseif pending && ccall(:jl_sigint_rescue_timer_expired_peek, Cint, ()) != 0
        # A press arrived after the active severity's grace period passed
        # (the rescue timer announced the escalation option to the user):
        # climb one rung, and start a fresh grace period for the next one.
        # The press requirement is load-bearing - a bare expiry (or a stale
        # one straggling in from the previous episode's timer) must offer,
        # never climb.
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
        return
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
    #
    # Our own ABANDON_ALL rung may just have frozen the foreground task;
    # clean it up (which also rescues an abandoned REPL backend). The
    # pass reaches into the REPL *only* on this path, once the target is
    # actually frozen: invoking (and possibly compiling) REPL code while
    # a cancelled-but-unresponsive victim is still running would let the
    # compile's GC deadlock against a victim spinning without
    # safepoints - the very situation the remaining rungs must cut
    # through.
    backend = active_repl_backend
    if backend !== nothing && (backend.backend_task::Task).state === :abandoned
        cleanup_abandoned_sigint_target(backend.backend_task::Task)
    elseif roottask.state === :abandoned && (backend === nothing || istaskdone(backend.backend_task::Task))
        cleanup_abandoned_sigint_target(roottask)
    end
    nothing
end

# Arbitrate and run pending ^C dispatch, called on the adopted signal
# thread (press, rescue-timer tick, or abandonment-verdict settle).
# `trylock` for reentrancy safety; the claim loop drains anything posted
# while a pass was running.
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
        Core.print(Core.stderr, "\n[DEBUG sigint] abandoned-cleanup exit: no evaluator left\n")
        exit(128 + 2) # 128 + SIGINT
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

# Arm the C side's ^C dispatch (the signal thread adopts itself as a Julia
# thread and runs `maybe_dispatch_sigint` directly; see
# jl_run_sigint_dispatch in src/signal-handling.c). Trimmed images stub
# this out, leaving the dispatch disarmed.
function start_sigint_listener()
    atexit() do
        ccall(:jl_set_sigint_dispatch_ready, Cvoid, (Cint,), 0)
    end
    rescue = Task(sigint_rescue_loop)
    rescue.sticky = false
    _sigint_rescue_task[] = rescue
    ccall(:jl_set_sigint_rescue_task, Cvoid, (Any,), rescue)
    ccall(:jl_set_sigint_dispatch_ready, Cvoid, (Cint,), 1)
    nothing
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
