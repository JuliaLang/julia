module EAUtils

export code_escapes, @code_escapes, __clear_cache!

const CC = Core.Compiler
using ..EscapeAnalysis
const EA = EscapeAnalysis

# entries
# -------

using Base: IdSet, unwrap_unionall, rewrap_unionall
using InteractiveUtils: gen_call_with_extracted_types_and_kwargs

"""
    @code_escapes [options...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`code_escapes`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of `code_escapes` keyword arguments can be given
as the optional arguments like `@code_escapes optimize=false myfunc(myargs...)`.
"""
macro code_escapes(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :code_escapes, ex0)
end

"""
    code_escapes(f, argtypes=Tuple{}; [debuginfo::Symbol = :none], [optimize::Bool = true]) -> result::EscapeResult

Runs the escape analysis on optimized IR of a generic function call with the given type signature.

# Keyword Arguments

- `optimize::Bool = true`:
  if `true` returns escape information of post-inlining IR (used for local optimization),
  otherwise returns escape information of pre-inlining IR (used for interprocedural escape information generation)
- `debuginfo::Symbol = :none`:
  controls the amount of code metadata present in the output, possible options are `:none` or `:source`.
"""
function code_escapes(@nospecialize(f), @nospecialize(types=Base.default_tt(f));
                      world::UInt = get_world_counter(),
                      debuginfo::Symbol = :none)
    tt = Base.signature_type(f, types)
    match = Base._which(tt; world, raise=true)
    mi = Core.Compiler.specialize_method(match)::MethodInstance
    interp = EscapeAnalyzer(world, mi)
    Core.Compiler.typeinf_ext(interp, mi)
    return EscapeResult(interp.ir, interp.estate, interp.mi, debuginfo === :source, interp)
end

# in order to run a whole analysis from ground zero (e.g. for benchmarking, etc.)
__clear_cache!() = empty!(GLOBAL_EA_CODE_CACHE)

# AbstractInterpreter
# -------------------

# imports
import .CC:
    AbstractInterpreter, NativeInterpreter, WorldView, WorldRange,
    InferenceParams, OptimizationParams, get_world_counter, get_inference_cache, code_cache
# usings
using Core:
    CodeInstance, MethodInstance, CodeInfo
using .CC:
    InferenceResult, OptimizationState, IRCode, copy as cccopy,
    @timeit, convert_to_ircode, slot2reg, compact!, ssa_inlining_pass!, sroa_pass!,
    adce_pass!, JLOptions, verify_ir, verify_linetable
using .EA: analyze_escapes, ArgEscapeCache, EscapeInfo, EscapeState, is_ipo_profitable

struct CodeCache
    cache::IdDict{MethodInstance,CodeInstance}
end
CodeCache() = CodeCache(IdDict{MethodInstance,CodeInstance}())
const GLOBAL_CODE_CACHE = CodeCache()

# when working outside of Core.Compiler,
# cache entire escape state for later inspection and debugging
struct EscapeCacheInfo
    argescapes::ArgEscapeCache
    state::EscapeState # preserved just for debugging purpose
    ir::IRCode         # preserved just for debugging purpose
end

struct EscapeCache
    cache::IdDict{MethodInstance,EscapeCacheInfo}
end
EscapeCache() = EscapeCache(IdDict{MethodInstance,EscapeCacheInfo}())
const GLOBAL_ESCAPE_CACHE = EscapeCache()

mutable struct EscapeAnalyzer <: AbstractInterpreter
    const world::UInt
    const inf_params::InferenceParams
    const opt_params::OptimizationParams
    const inf_cache::Vector{InferenceResult}
    const code_cache::CodeCache
    const escape_cache::EscapeCache
    const entry_mi::MethodInstance
    ir::IRCode
    estate::EscapeState
    mi::MethodInstance
    function EscapeAnalyzer(world::UInt, entry_mi::MethodInstance,
                            code_cache::CodeCache=GLOBAL_CODE_CACHE,
                            escape_cache::EscapeCache=GLOBAL_ESCAPE_CACHE)
        inf_params = InferenceParams()
        opt_params = OptimizationParams()
        inf_cache = InferenceResult[]
        return new(world, inf_params, opt_params, inf_cache, code_cache, escape_cache, entry_mi)
    end
end

CC.InferenceParams(interp::EscapeAnalyzer) = interp.inf_params
CC.OptimizationParams(interp::EscapeAnalyzer) = interp.opt_params
CC.get_world_counter(interp::EscapeAnalyzer) = interp.world
CC.get_inference_cache(interp::EscapeAnalyzer) = interp.inf_cache

struct EscapeAnalyzerCacheView
    code_cache::CodeCache
    escape_cache::EscapeCache
end

function CC.code_cache(interp::EscapeAnalyzer)
    worlds = WorldRange(get_world_counter(interp))
    return WorldView(EscapeAnalyzerCacheView(interp.code_cache, interp.escape_cache), worlds)
end
CC.haskey(wvc::WorldView{EscapeAnalyzerCacheView}, mi::MethodInstance) = haskey(wvc.cache.code_cache.cache, mi)
CC.get(wvc::WorldView{EscapeAnalyzerCacheView}, mi::MethodInstance, default) = get(wvc.cache.code_cache.cache, mi, default)
CC.getindex(wvc::WorldView{EscapeAnalyzerCacheView}, mi::MethodInstance) = getindex(wvc.cache.code_cache.cache, mi)
function CC.setindex!(wvc::WorldView{EscapeAnalyzerCacheView}, ci::CodeInstance, mi::MethodInstance)
    wvc.cache.code_cache.cache[mi] = ci
    add_invalidation_callback!(wvc.cache.code_cache, wvc.cache.escape_cache, mi) # register the callback on invalidation
    return nothing
end
function add_invalidation_callback!(code_cache::CodeCache, escape_cache::EscapeCache, mi)
    callback = InvalidationCallback(code_cache, escape_cache)
    if !isdefined(mi, :callbacks)
        mi.callbacks = Any[callback]
    else
        if !any(@nospecialize(cb)->cb===callback, mi.callbacks)
            push!(mi.callbacks, callback)
        end
    end
    return nothing
end
struct InvalidationCallback
    code_cache::CodeCache
    escape_cache::EscapeCache
end
function (callback::InvalidationCallback)(replaced::MethodInstance, max_world,
                                          seen::IdSet{MethodInstance}=IdSet{MethodInstance}())
    (; code_cache, escape_cache) = callback
    push!(seen, replaced)
    delete!(code_cache.cache, replaced)
    delete!(escape_cache.cache, replaced)
    if isdefined(replaced, :backedges)
        for mi in replaced.backedges
            isa(mi, MethodInstance) || continue # might be `Type` object representing an `invoke` signature
            mi in seen && continue # otherwise fall into infinite loop
            callback(mi, max_world, seen)
        end
    end
    return nothing
end

function CC.optimize(interp::EscapeAnalyzer, opt::OptimizationState, caller::InferenceResult)
    ir = run_passes_ipo_safe_with_ea(interp, opt.src, opt, caller)
    CC.ipo_dataflow_analysis!(interp, ir, caller)
    return CC.finish(interp, opt, ir, caller)
end

function record_escapes!(interp::EscapeAnalyzer,
    caller::InferenceResult, estate::EscapeState, cacheir::IRCode)
    cache = ArgEscapeCache(estate)
    ecache = EscapeCacheInfo(cache, estate, cacheir)
    return caller.argescapes = ecache
end

struct GetEscapeCache
    escape_cache::EscapeCache
    GetEscapeCache(interp::EscapeAnalyzer) = new(interp.escape_cache)
end
function ((; escape_cache)::GetEscapeCache)(mi::MethodInstance)
    cached = get(escape_cache.cache, mi, nothing)
    return cached === nothing ? nothing : cached.argescapes
end

struct FailedAnalysis
    ir::IRCode
    nargs::Int
    get_escape_cache::GetEscapeCache
end

function run_passes_ipo_safe_with_ea(interp::EscapeAnalyzer,
    ci::CodeInfo, sv::OptimizationState, caller::InferenceResult)
    @timeit "convert"   ir = convert_to_ircode(ci, sv)
    @timeit "slot2reg"  ir = slot2reg(ir, ci, sv)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    @timeit "compact 1" ir = compact!(ir)
    @timeit "Inlining"  ir = ssa_inlining_pass!(ir, sv.inlining, ci.propagate_inbounds)
    # @timeit "verify 2" verify_ir(ir)
    @timeit "compact 2" ir = compact!(ir)
    @timeit "SROA"      ir = sroa_pass!(ir, sv.inlining)
    @timeit "ADCE"      ir = adce_pass!(ir, sv.inlining)
    @timeit "compact 3" ir = compact!(ir, true)
    if JLOptions().debug_level == 2
        @timeit "verify 3" (verify_ir(ir); verify_linetable(ir.linetable))
    end
    nargs = let def = sv.linfo.def; isa(def, Method) ? Int(def.nargs) : 0; end
    get_escape_cache = GetEscapeCache(interp)
    local estate::EscapeState
    try
        @timeit "EA" estate = analyze_escapes(ir, nargs, get_escape_cache)
    catch err
        @error "error happened within EA, inspect `Main.failed_escapeanalysis`"
        Main.failed_escapeanalysis = FailedAnalysis(ir, nargs, get_escape_cache)
        rethrow(err)
    end
    if caller.linfo === interp.entry_mi
        # return back the result
        interp.ir = cccopy(ir)
        interp.estate = estate
        interp.mi = sv.linfo
    end
    record_escapes!(interp, caller, estate, ir)
    return ir
end

function CC.cache_result!(interp::EscapeAnalyzer, result::InferenceResult)
    argescapes = result.argescapes
    if argescapes isa EscapeCacheInfo
        interp.escape_cache.cache[result.linfo] = argescapes
    end
    return @invoke CC.cache_result!(interp::AbstractInterpreter, result::InferenceResult)
end

# printing
# --------

using Core: Argument, SSAValue
using .CC: widenconst, singleton_type

if EA._TOP_MOD === CC
    Base.getindex(estate::EscapeState, @nospecialize(x)) = CC.getindex(estate, x)
end

function get_name_color(x::EscapeInfo, symbol::Bool = false)
    getname(x) = string(nameof(x))
    if x === EA.⊥
        name, color = (getname(EA.NotAnalyzed), "◌"), :plain
    elseif EA.has_no_escape(EA.ignore_argescape(x))
        if EA.has_arg_escape(x)
            name, color = (getname(EA.ArgEscape), "✓"), :cyan
        else
            name, color = (getname(EA.NoEscape), "✓"), :green
        end
    elseif EA.has_all_escape(x)
        name, color = (getname(EA.AllEscape), "X"), :red
    elseif EA.has_return_escape(x)
        name = (getname(EA.ReturnEscape), "↑")
        color = EA.has_thrown_escape(x) ? :yellow : :blue
    else
        name = (nothing, "*")
        color = EA.has_thrown_escape(x) ? :yellow : :bold
    end
    name = symbol ? last(name) : first(name)
    if name !== nothing && !isa(x.AliasInfo, Bool)
        name = string(name, "′")
    end
    return name, color
end

# pcs = sprint(show, collect(x.EscapeSites); context=:limit=>true)
function Base.show(io::IO, x::EscapeInfo)
    name, color = get_name_color(x)
    if isnothing(name)
        @invoke show(io::IO, x::Any)
    else
        printstyled(io, name; color)
    end
end

struct EscapeResult
    ir::IRCode
    state::EscapeState
    mi::Union{Nothing,MethodInstance}
    source::Bool
    interp::Union{Nothing,EscapeAnalyzer}
    function EscapeResult(ir::IRCode, state::EscapeState,
                          mi::Union{Nothing,MethodInstance}=nothing,
                          source::Bool=false,
                          interp::Union{Nothing,EscapeAnalyzer}=nothing)
        return new(ir, state, mi, source, interp)
    end
end
Base.show(io::IO, result::EscapeResult) = print_with_info(io, result)
@eval Base.iterate(res::EscapeResult, state=1) =
    return state > $(fieldcount(EscapeResult)) ? nothing : (getfield(res, state), state+1)

Base.show(io::IO, cached::EscapeCacheInfo) = show(io, EscapeResult(cached.ir, cached.state))

# adapted from https://github.com/JuliaDebug/LoweredCodeUtils.jl/blob/4612349432447e868cf9285f647108f43bd0a11c/src/codeedges.jl#L881-L897
function print_with_info(io::IO, (; ir, state, mi, source)::EscapeResult)
    # print escape information on SSA values
    function preprint(io::IO)
        ft = ir.argtypes[1]
        f = singleton_type(ft)
        if f === nothing
            f = widenconst(ft)
        end
        print(io, f, '(')
        for i in 1:state.nargs
            arg = state[Argument(i)]
            i == 1 && continue
            c, color = get_name_color(arg, true)
            printstyled(io, c, ' ', '_', i, "::", ir.argtypes[i]; color)
            i ≠ state.nargs && print(io, ", ")
        end
        print(io, ')')
        if !isnothing(mi)
            def = mi.def
            printstyled(io, " in ", (isa(def, Module) ? (def,) : (def.module, " at ", def.file, ':', def.line))...; color=:bold)
        end
        println(io)
    end

    # print escape information on SSA values
    # nd = ndigits(length(ssavalues))
    function preprint(io::IO, idx::Int)
        c, color = get_name_color(state[SSAValue(idx)], true)
        # printstyled(io, lpad(idx, nd), ' ', c, ' '; color)
        printstyled(io, rpad(c, 2), ' '; color)
    end

    print_with_info(preprint, (args...)->nothing, io, ir, source)
end

function print_with_info(preprint, postprint, io::IO, ir::IRCode, source::Bool)
    io = IOContext(io, :displaysize=>displaysize(io))
    used = Base.IRShow.stmts_used(io, ir)
    if source
        line_info_preprinter = function (io::IO, indent::String, idx::Int)
            r = Base.IRShow.inline_linfo_printer(ir)(io, indent, idx)
            idx ≠ 0 && preprint(io, idx)
            return r
        end
    else
        line_info_preprinter = Base.IRShow.lineinfo_disabled
    end
    line_info_postprinter = Base.IRShow.default_expr_type_printer
    preprint(io)
    bb_idx_prev = bb_idx = 1
    for idx = 1:length(ir.stmts)
        preprint(io, idx)
        bb_idx = Base.IRShow.show_ir_stmt(io, ir, idx, line_info_preprinter, line_info_postprinter, used, ir.cfg, bb_idx)
        postprint(io, idx, bb_idx != bb_idx_prev)
        bb_idx_prev = bb_idx
    end
    max_bb_idx_size = ndigits(length(ir.cfg.blocks))
    line_info_preprinter(io, " "^(max_bb_idx_size + 2), 0)
    postprint(io)
    return nothing
end

end # module EAUtils
