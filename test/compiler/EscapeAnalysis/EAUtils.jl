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
    frame = Core.Compiler.typeinf_frame(interp, mi, #=run_optimizer=#true)
    isdefined(interp, :result) || error("optimization didn't happen: maybe everything has been constant folded?")
    slotnames = let src = frame.src
        src isa CodeInfo ? src.slotnames : nothing
    end
    return EscapeResult(interp.result.ir, interp.result.estate, interp.result.mi,
                        slotnames, debuginfo === :source, interp)
end

# in order to run a whole analysis from ground zero (e.g. for benchmarking, etc.)
__clear_cache!() = empty!(GLOBAL_EA_CODE_CACHE)

# AbstractInterpreter
# -------------------

# imports
import .CC:
    AbstractInterpreter, NativeInterpreter, WorldView, WorldRange,
    InferenceParams, OptimizationParams, get_world_counter, get_inference_cache,
    ipo_dataflow_analysis!, cache_result!
# usings
using Core:
    CodeInstance, MethodInstance, CodeInfo
using .CC:
    InferenceResult, InferenceState, OptimizationState, IRCode
using .EA: analyze_escapes, ArgEscapeCache, EscapeInfo, EscapeState

struct EAToken end

# when working outside of Core.Compiler,
# cache entire escape state for later inspection and debugging
struct EscapeCacheInfo
    argescapes::ArgEscapeCache
    state::EscapeState # preserved just for debugging purpose
    ir::IRCode         # preserved just for debugging purpose
end

struct EscapeCache
    cache::IdDict{MethodInstance,EscapeCacheInfo} # TODO(aviatesk) Should this be CodeInstance to EscapeCacheInfo?
end
EscapeCache() = EscapeCache(IdDict{MethodInstance,EscapeCacheInfo}())
const GLOBAL_ESCAPE_CACHE = EscapeCache()

struct EscapeResultForEntry
    ir::IRCode
    estate::EscapeState
    mi::MethodInstance
end

mutable struct EscapeAnalyzer <: AbstractInterpreter
    const world::UInt
    const inf_params::InferenceParams
    const opt_params::OptimizationParams
    const inf_cache::Vector{InferenceResult}
    const escape_cache::EscapeCache
    const entry_mi::MethodInstance
    result::EscapeResultForEntry
    function EscapeAnalyzer(world::UInt, entry_mi::MethodInstance,
                            escape_cache::EscapeCache=GLOBAL_ESCAPE_CACHE)
        inf_params = InferenceParams()
        opt_params = OptimizationParams()
        inf_cache = InferenceResult[]
        return new(world, inf_params, opt_params, inf_cache, escape_cache, entry_mi)
    end
end

CC.InferenceParams(interp::EscapeAnalyzer) = interp.inf_params
CC.OptimizationParams(interp::EscapeAnalyzer) = interp.opt_params
CC.get_inference_world(interp::EscapeAnalyzer) = interp.world
CC.get_inference_cache(interp::EscapeAnalyzer) = interp.inf_cache
CC.cache_owner(::EscapeAnalyzer) = EAToken()

function CC.ipo_dataflow_analysis!(interp::EscapeAnalyzer, ir::IRCode, caller::InferenceResult)
    # run EA on all frames that have been optimized
    nargs = let def = caller.linfo.def; isa(def, Method) ? Int(def.nargs) : 0; end
    get_escape_cache = GetEscapeCache(interp)
    estate = try
        analyze_escapes(ir, nargs, CC.optimizer_lattice(interp), get_escape_cache)
    catch err
        @error "error happened within EA, inspect `Main.failed_escapeanalysis`"
        Main.failed_escapeanalysis = FailedAnalysis(ir, nargs, get_escape_cache)
        rethrow(err)
    end
    if caller.linfo === interp.entry_mi
        # return back the result
        interp.result = EscapeResultForEntry(CC.copy(ir), estate, caller.linfo)
    end
    record_escapes!(interp, caller, estate, ir)

    @invoke CC.ipo_dataflow_analysis!(interp::AbstractInterpreter, ir::IRCode, caller::InferenceResult)
end

function record_escapes!(interp::EscapeAnalyzer,
    caller::InferenceResult, estate::EscapeState, ir::IRCode)
    argescapes = ArgEscapeCache(estate)
    ecacheinfo = EscapeCacheInfo(argescapes, estate, ir)
    return CC.stack_analysis_result!(caller, ecacheinfo)
end

struct GetEscapeCache
    escape_cache::EscapeCache
    GetEscapeCache(interp::EscapeAnalyzer) = new(interp.escape_cache)
end
function ((; escape_cache)::GetEscapeCache)(mi::MethodInstance)
    ecacheinfo = get(escape_cache.cache, mi, nothing)
    return ecacheinfo === nothing ? false : ecacheinfo.argescapes
end

struct FailedAnalysis
    ir::IRCode
    nargs::Int
    get_escape_cache::GetEscapeCache
end

function CC.finish!(interp::EscapeAnalyzer, state::InferenceState; can_discard_trees::Bool=CC.may_discard_trees(interp))
    ecacheinfo = CC.traverse_analysis_results(state.result) do @nospecialize result
        return result isa EscapeCacheInfo ? result : nothing
    end
    ecacheinfo isa EscapeCacheInfo && (interp.escape_cache.cache[state.linfo] = ecacheinfo)
    return @invoke CC.finish!(interp::AbstractInterpreter, state::InferenceState; can_discard_trees)
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
    slotnames::Union{Nothing,Vector{Symbol}}
    source::Bool
    interp::Union{Nothing,EscapeAnalyzer}
    function EscapeResult(ir::IRCode, state::EscapeState,
                          mi::Union{Nothing,MethodInstance}=nothing,
                          slotnames::Union{Nothing,Vector{Symbol}}=nothing,
                          source::Bool=false,
                          interp::Union{Nothing,EscapeAnalyzer}=nothing)
        return new(ir, state, mi, slotnames, source, interp)
    end
end
Base.show(io::IO, result::EscapeResult) = print_with_info(io, result)
@eval Base.iterate(res::EscapeResult, state=1) =
    return state > $(fieldcount(EscapeResult)) ? nothing : (getfield(res, state), state+1)

Base.show(io::IO, ecacheinfo::EscapeCacheInfo) = show(io, EscapeResult(ecacheinfo.ir, ecacheinfo.state))

# adapted from https://github.com/JuliaDebug/LoweredCodeUtils.jl/blob/4612349432447e868cf9285f647108f43bd0a11c/src/codeedges.jl#L881-L897
function print_with_info(io::IO, result::EscapeResult)
    (; ir, state, mi, slotnames, source) = result
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
            slot = isnothing(slotnames) ? "_$i" : slotnames[i]
            printstyled(io, c, ' ', slot, "::", ir.argtypes[i]; color)
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
