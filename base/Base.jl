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

# Tiered compilation worker: a dedicated OS thread, created and run
# entirely in C (jl_tier_start_worker / tier_worker_threadfun in
# src/tiered.c) and adopted into the runtime, that drains the promotion
# queue and re-emits hot CodeInstances at T1. It is woken directly by
# jl_tier_enqueue via a uv condition variable — no Julia task, scheduler,
# or libuv event loop involvement — and the kernel schedules it
# preemptively (like the compiler threads of HotSpot or .NET), so
# promotion makes progress even with --threads=1 and against a main
# thread that never yields.
# On-stack replacement for interpreted loops (Truffle-style; the C side is
# the back-edge counter in src/interpreter.c and the hook plumbing in
# src/tiered.c). When an interpreted method frame crosses the back-edge
# threshold, the interpreter hands us its lowered source, MethodInstance,
# the back-edge target statement index, and a snapshot of the frame state
# (slots, then ssavalues — #undef entries unset). We synthesize a
# continuation CodeInfo that re-enters the body at the target with the
# live values passed as OpaqueClosure arguments — inference then
# specializes the continuation (loop included) on their concrete runtime
# types — call it, and return Some(result) for the interpreter to return
# directly. The continuation is cached per (mi, target, world,
# slot-definedness).
const _tier_osr_cache = IdDict{Any,Any}()
const _tier_osr_lock = ReentrantLock()

function _tier_osr_rewrite(@nospecialize(x), slotmap::Vector{Int}, ssamap::Dict{Int,Int}, shift::Int,
                           sparams::Core.SimpleVector)
    if x isa Core.SlotNumber
        return Core.SlotNumber(slotmap[x.id])
    elseif x isa Core.SSAValue
        ns = get(ssamap, x.id, 0)
        return ns == 0 ? Core.SSAValue(x.id + shift) : Core.SlotNumber(ns)
    elseif x isa Core.GotoNode
        return Core.GotoNode(x.label + shift)
    elseif x isa Core.GotoIfNot
        return Core.GotoIfNot(_tier_osr_rewrite(x.cond, slotmap, ssamap, shift, sparams), x.dest + shift)
    elseif x isa Core.ReturnNode
        return isdefined(x, :val) ?
            Core.ReturnNode(_tier_osr_rewrite(x.val, slotmap, ssamap, shift, sparams)) : x
    elseif x isa Core.NewvarNode
        return Core.NewvarNode(Core.SlotNumber(slotmap[x.slot.id]))
    elseif x isa Expr
        if x.head === :static_parameter
            # The continuation OpaqueClosure method has no static parameters
            # (a stray :static_parameter would crash inference of the
            # continuation), so substitute the MethodInstance's concrete
            # value. The plan declines when any used sparam is still a
            # TypeVar (incompletely determined).
            return QuoteNode(sparams[x.args[1]::Int])
        end
        return Expr(x.head, Any[_tier_osr_rewrite(a, slotmap, ssamap, shift, sparams) for a in x.args]...)
    else
        return x # literals, GlobalRef, QuoteNode, Symbol, LineNumberNode, ...
    end
end

# Does `x` reference a static parameter that cannot be substituted with a
# concrete value (out of range, or still an unbound TypeVar)?
function _tier_osr_bad_sparam(@nospecialize(x), sparams::Core.SimpleVector)
    if x isa Expr
        if x.head === :static_parameter
            n = x.args[1]
            n isa Int || return true
            (1 <= n <= length(sparams)) || return true
            return sparams[n] isa Core.TypeVar
        end
        for a in x.args
            _tier_osr_bad_sparam(a, sparams) && return true
        end
    elseif x isa Core.GotoIfNot
        return _tier_osr_bad_sparam(x.cond, sparams)
    elseif x isa Core.ReturnNode
        return isdefined(x, :val) && _tier_osr_bad_sparam(x.val, sparams)
    end
    return false
end

# Statement indices reachable from `ip` in lowered control flow.
function _tier_osr_reachable(code::Vector{Any}, ip::Int)
    n = length(code)
    seen = falses(n)
    work = Int[ip]
    while !isempty(work)
        i = pop!(work)
        (i < 1 || i > n || seen[i]) && continue
        seen[i] = true
        st = code[i]
        if st isa Core.GotoNode
            push!(work, st.label)
        elseif st isa Core.GotoIfNot
            push!(work, st.dest); push!(work, i + 1)
        elseif st isa Core.ReturnNode
        else
            push!(work, i + 1)
        end
    end
    return seen
end

_tier_osr_collect_ssas!(out::Dict{Int,Int}, @nospecialize(x), k::Int, reachable) = begin
    if x isa Core.SSAValue
        # An SSA value must be carried through a slot (saved entry value +
        # def redirected to the slot) unless its only uses are the lowered
        # idiom chain at the immediately following statement — anything
        # farther can be reached on a path (notably the OSR entry path for
        # loop-carried values) where the definition has not (re)executed.
        if k > x.id + 1 || (x.id <= length(reachable) && reachable[k] && !reachable[x.id])
            out[x.id] = 0
        end
    elseif x isa Core.GotoIfNot
        _tier_osr_collect_ssas!(out, x.cond, k, reachable)
    elseif x isa Core.ReturnNode
        isdefined(x, :val) && _tier_osr_collect_ssas!(out, x.val, k, reachable)
    elseif x isa Expr
        for a in x.args
            _tier_osr_collect_ssas!(out, a, k, reachable)
        end
    end
    nothing
end

# Decide whether `src` is OSR-able at `ip` and, if so, which saved
# ssavalues the continuation needs as arguments (definitions that cannot
# re-execute from ip but are referenced by reachable code). Returns the
# sorted ssa id list, or nothing to decline.
function _tier_osr_build_plan(src::Core.CodeInfo, ip::Int, sparams::Core.SimpleVector)
    code = src.code::Vector{Any}
    # v1: no exception regions anywhere in the body (the continuation would
    # have to re-establish active handlers), and lowered source only (the
    # interpreter can also run inferred IR, whose phi nodes this transform
    # does not handle). Static parameters are substituted with their
    # concrete values during the rewrite; decline if any used one is still
    # an unbound TypeVar.
    for st in code
        if st isa Core.EnterNode || st isa Core.PhiNode ||
           st isa Core.PhiCNode || st isa Core.UpsilonNode ||
           (st isa Expr && (st.head === :enter || st.head === :leave || st.head === :pop_exception))
            return nothing
        end
        _tier_osr_bad_sparam(st, sparams) && return nothing
    end
    reachable = _tier_osr_reachable(code, ip)
    ssamap = Dict{Int,Int}()
    for i in 1:length(code)
        _tier_osr_collect_ssas!(ssamap, code[i], i, reachable)
    end
    # The definition of a slotified SSA value is redirected to assign the
    # slot; that is only expressible when the defining statement has a
    # value position (not itself a slot assignment or control flow).
    for j in keys(ssamap)
        st = code[j]
        if (st isa Expr && (st.head === :(=) || st.head === :meta || st.head === :loopinfo)) ||
           st isa Core.GotoNode || st isa Core.GotoIfNot || st isa Core.ReturnNode ||
           st isa Core.NewvarNode
            return nothing
        end
    end
    return sort!(collect(keys(ssamap)))
end

# Build (oc, ssalist) for re-entering `src` at statement `ip` with arguments
# of the concrete types `argtypes` (OpaqueClosures compile once for their
# declared signature, so the signature must be the live values' actual types
# for the continuation's loop to be well-typed).
function _tier_osr_build(src::Core.CodeInfo, mi::Core.MethodInstance, ip::Int, defined::Vector{Bool},
                         argtypes::Vector{Any}, ssalist::Vector{Int})
    code = src.code::Vector{Any}
    nslots = length(src.slotnames)
    ssamap = Dict{Int,Int}()

    # New slot layout: 1 = #self#, then defined original slots (arguments),
    # then saved ssavalues (arguments), then undef original slots (locals).
    slotmap = zeros(Int, nslots)
    slotnames = Symbol[Symbol("#osr#")]
    next = 2
    for k in 1:nslots
        if defined[k]
            slotmap[k] = next; next += 1
            push!(slotnames, src.slotnames[k])
        end
    end
    for j in ssalist
        ssamap[j] = next; next += 1
        push!(slotnames, Symbol(:osr_ssa, j))
    end
    nargs_real = next - 2
    undef_slots = Int[]
    for k in 1:nslots
        if !defined[k]
            slotmap[k] = next; next += 1
            push!(slotnames, src.slotnames[k])
            push!(undef_slots, slotmap[k])
        end
    end
    ntotal = next - 1

    # Prefix: force_compile (the continuation must not be parked back into
    # the interpreter by the tier gate), re-undef the undefined slots, then
    # jump to the target.
    shift = 2 + length(undef_slots)
    newcode = Vector{Any}(undef, shift + length(code))
    newcode[1] = Expr(:meta, :force_compile)
    for (i, sl) in enumerate(undef_slots)
        newcode[1 + i] = Core.NewvarNode(Core.SlotNumber(sl))
    end
    newcode[shift] = Core.GotoNode(ip + shift)
    sparams = mi.sparam_vals
    for i in 1:length(code)
        st = _tier_osr_rewrite(code[i], slotmap, ssamap, shift, sparams)
        # Slotified SSA definition: also store the value into its slot, so
        # later iterations observe the fresh definition while the OSR entry
        # path sees the saved one. (References were rewritten to the slot.)
        sl = get(ssamap, i, 0)
        if sl != 0
            st = Expr(:(=), Core.SlotNumber(sl), st)
        end
        newcode[shift + i] = st
    end

    cont = copy(src)
    cont.code = newcode
    cont.ssavaluetypes = length(newcode)
    cont.ssaflags = vcat(fill(zero(eltype(src.ssaflags)), shift), src.ssaflags)
    cont.slotnames = slotnames
    # Carry the original slot flags through the renumbering — inference
    # depends on them (notably the used-undef bit for conditionally
    # assigned slots; wrong flags miscompile the continuation). Saved-ssa
    # argument slots are plain used+assigned.
    slotflags = Vector{UInt8}(undef, ntotal)
    slotflags[1] = 0x00
    for k in 1:nslots
        slotflags[slotmap[k]] = src.slotflags[k]
    end
    for j in ssalist
        slotflags[ssamap[j]] = 0x08
    end
    cont.slotflags = slotflags
    cont.nargs = 1 + nargs_real
    cont.isva = false
    cont.debuginfo = Core.DebugInfo(:none)
    @assert length(argtypes) == nargs_real
    sig = Tuple{argtypes...}
    oc = Experimental.generate_opaque_closure(sig, Union{}, Any, cont, nargs_real, false;
                                               mod=mi.def.module, do_compile=true, isinferred=false)
    return (oc, ssalist)
end

function _tier_osr_impl(src::Core.CodeInfo, mi::Core.MethodInstance, ip::Int, state::Vector{Any})
    nslots = length(src.slotnames)
    defined = Bool[isassigned(state, k) for k in 1:nslots]
    # Assemble the live values first: the continuation is specialized on
    # their concrete types, which therefore belong in the cache key. The
    # ssavalue argument list cannot be known before building, so resolve it
    # in two steps: probe the cache with a slots-only key to find ssalist,
    # falling back to a build that records it.
    world = ccall(:jl_get_tls_world_age, UInt, ())
    slotargs = Vector{Any}()
    for k in 1:nslots
        defined[k] && push!(slotargs, state[k])
    end
    slottypes = Any[Core.Typeof(a) for a in slotargs]
    key = (mi, ip, world, defined, slottypes)
    entry = lock(_tier_osr_lock) do
        get!(_tier_osr_cache, key) do
            # Determine the ssavalue argument list and its current types,
            # then build with the full concrete signature. (Named `plan`,
            # not `ssalist`: reusing the outer destructuring name from
            # inside this closure would box it.)
            plan = _tier_osr_build_plan(src, ip, mi.sparam_vals)
            plan === nothing && return nothing
            argtypes = copy(slottypes)
            for j in plan
                push!(argtypes, isassigned(state, nslots + j) ?
                      Core.Typeof(state[nslots + j]) : Nothing)
            end
            length(argtypes) <= 64 || return nothing
            _tier_osr_build(src, mi, ip, defined, argtypes, plan)
        end
    end
    entry === nothing && return nothing
    oc, ssalist = entry
    args = slotargs
    for j in ssalist
        # An unset saved ssavalue can only belong to a dynamically-dead use;
        # pass a placeholder.
        push!(args, isassigned(state, nslots + j) ? state[nslots + j] : nothing)
    end
    return (oc, args)
end

function _tier_osr(@nospecialize(src), @nospecialize(mi), @nospecialize(ip), @nospecialize(state))
    # Called (via @cfunction) from the interpreter's back-edge check.
    # Build/lookup failures decline by returning nothing. The continuation
    # CALL is deliberately OUTSIDE the try: it runs user code with the
    # original frame's semantics, so an exception it throws must propagate
    # (through the cfunction, exactly like any throwing interpreted
    # statement). Catching it here would silently discard the error and
    # resume interpreting from the pre-OSR snapshot, re-running iterations
    # the continuation already executed (duplicated side effects).
    local oc, args
    try
        entry = _tier_osr_impl(src::Core.CodeInfo, mi::Core.MethodInstance,
                               ip::Int, state::Vector{Any})
        entry === nothing && return nothing
        oc, args = entry
    catch
        return nothing
    end
    return Some{Any}(oc(args...))
end

function start_tier_worker()
    # Master gate: tiering (and thus the worker) is on by default only
    # when a spare CPU exists for the worker thread to run on; see
    # jl_tier_enabled in src/tiered.c. Force with JULIA_TIER_ENABLE=1/0.
    ccall(:jl_tier_enabled, Cint, ()) != 0 || return
    get_bool_env("JULIA_TIER_WORKER", true) === true || return
    # The worker re-emits at T1 through the normal jl_compile_method_internal
    # path, re-inferring when the inferred IR isn't cached. Concurrent cache
    # mutation is safe: jl_mi_cache_insert holds the method writelock and
    # inference is serialized per-MethodInstance by the engine; the serializer
    # parks and drains the worker (jl_tier_quiesce/jl_tier_drain) before it
    # snapshots CodeInstances, so a promotion can never race serialization.
    ccall(:jl_tier_start_worker, Cvoid, ())
    # The interpreter's OSR back-edge path resolves `_tier_osr` dynamically
    # by name (jl_get_global in eval_try_osr); we deliberately do NOT hold a
    # @cfunction reference to it here, so juliac `--trim` never pulls its
    # runtime IR-building machinery into the static call graph.
    atexit() do
        # Parks the worker and waits out any in-flight promotion, so
        # runtime teardown never races a compile.
        ccall(:jl_tier_stop_worker, Cvoid, ())
    end
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
    start_tier_worker()
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
