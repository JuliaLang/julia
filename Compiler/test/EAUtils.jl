module EAUtils

export code_escapes, @code_escapes, __clear_cache!

include("setup_Compiler.jl")

using .Compiler: EscapeAnalysis as EA

# AbstractInterpreter
# -------------------

# imports
import .Compiler:
    AbstractInterpreter, NativeInterpreter, WorldRange, InferenceParams,
    OptimizationParams, get_world_counter, get_inference_cache, ipo_dataflow_analysis!
# usings
using Core.IR
using .Compiler: InferenceResult, InferenceState, OptimizationState, IRCode
using .EA: analyze_escapes, ArgEscapeCache, ArgEscapeInfo, EscapeInfo, EscapeState

mutable struct EscapeAnalyzerCacheToken end
global GLOBAL_EA_CACHE_TOKEN::EscapeAnalyzerCacheToken = EscapeAnalyzerCacheToken()

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
    const token::EscapeAnalyzerCacheToken
    const entry_mi::Union{Nothing,MethodInstance}
    result::EscapeResultForEntry
    function EscapeAnalyzer(world::UInt, cache_token::EscapeAnalyzerCacheToken;
                            entry_mi::Union{Nothing,MethodInstance}=nothing)
        inf_params = InferenceParams()
        opt_params = OptimizationParams()
        inf_cache = InferenceResult[]
        return new(world, inf_params, opt_params, inf_cache, cache_token, entry_mi)
    end
end

Compiler.InferenceParams(interp::EscapeAnalyzer) = interp.inf_params
Compiler.OptimizationParams(interp::EscapeAnalyzer) = interp.opt_params
Compiler.get_inference_world(interp::EscapeAnalyzer) = interp.world
Compiler.get_inference_cache(interp::EscapeAnalyzer) = interp.inf_cache
Compiler.cache_owner(interp::EscapeAnalyzer) = interp.token
Compiler.get_escape_cache(::EscapeAnalyzer) = GetEscapeCache()

function Compiler.ipo_dataflow_analysis!(interp::EscapeAnalyzer, opt::OptimizationState,
                                         ir::IRCode, caller::InferenceResult)
    # run EA on all frames that have been optimized
    nargs = Int(opt.src.nargs)
    𝕃ₒ = Compiler.optimizer_lattice(interp)
    estate = try
        analyze_escapes(ir, nargs, 𝕃ₒ, GetEscapeCache())
    catch err
        @error "error happened within EA, inspect `Main.failedanalysis`"
        failedanalysis = FailedAnalysis(caller, ir, nargs)
        Core.eval(Main, :(failedanalysis = $failedanalysis))
        rethrow(err)
    end
    if caller.linfo === interp.entry_mi
        # return back the result
        interp.result = EscapeResultForEntry(Compiler.copy(ir), estate, caller.linfo)
    end
    record_escapes!(caller, estate, ir)

    @invoke Compiler.ipo_dataflow_analysis!(interp::AbstractInterpreter, opt::OptimizationState,
                                            ir::IRCode, caller::InferenceResult)
end

# cache entire escape state for inspection and debugging
struct EscapeCacheInfo
    argescapes::ArgEscapeCache
    state::EscapeState # preserved just for debugging purpose
    ir::IRCode         # preserved just for debugging purpose
end

function record_escapes!(caller::InferenceResult, estate::EscapeState, ir::IRCode)
    argescapes = ArgEscapeCache(estate)
    ecacheinfo = EscapeCacheInfo(argescapes, estate, ir)
    return Compiler.stack_analysis_result!(caller, ecacheinfo)
end

struct GetEscapeCache end
function (::GetEscapeCache)(codeinst::Union{CodeInstance,MethodInstance})
    codeinst isa CodeInstance || return false
    ecacheinfo = Compiler.traverse_analysis_results(codeinst) do @nospecialize result
        return result isa EscapeCacheInfo ? result : nothing
    end
    return ecacheinfo === nothing ? false : ecacheinfo.argescapes
end

struct FailedAnalysis
    caller::InferenceResult
    ir::IRCode
    nargs::Int
end

# printing
# --------

using Core: Argument, SSAValue
using .Compiler: widenconst, singleton_type

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

function get_sym_color(x::ArgEscapeInfo)
    escape_bits = x.escape_bits
    if escape_bits == EA.ARG_ALL_ESCAPE
        color, sym = :red, "X"
    elseif escape_bits == 0x00
        color, sym = :green, "✓"
    else
        color, sym = :bold, "*"
        if !iszero(escape_bits & EA.ARG_RETURN_ESCAPE)
            color, sym = :blue, "↑"
        end
        if !iszero(escape_bits & EA.ARG_THROWN_ESCAPE)
            color = :yellow
        end
    end
    return sym, color
end

function Base.show(io::IO, x::ArgEscapeInfo)
    escape_bits = x.escape_bits
    if escape_bits == EA.ARG_ALL_ESCAPE
        color, sym = :red, "X"
    elseif escape_bits == 0x00
        color, sym = :green, "✓"
    else
        color, sym = :bold, "*"
        if !iszero(escape_bits & EA.ARG_RETURN_ESCAPE)
            color, sym = :blue, "↑"
        end
        if !iszero(escape_bits & EA.ARG_THROWN_ESCAPE)
            color = :yellow
        end
    end
    printstyled(io, "ArgEscapeInfo(", sym, ")"; color)
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
    used = Compiler.IRShow.stmts_used(io, ir)
    if source
        line_info_preprinter = function (io::IO, indent::String, idx::Int)
            r = Compiler.IRShow.inline_linfo_printer(ir)(io, indent, idx)
            idx ≠ 0 && preprint(io, idx)
            return r
        end
    else
        line_info_preprinter = Compiler.IRShow.lineinfo_disabled
    end
    line_info_postprinter = Compiler.IRShow.default_expr_type_printer
    preprint(io)
    bb_idx_prev = bb_idx = 1
    for idx = 1:length(ir.stmts)
        preprint(io, idx)
        bb_idx = Compiler.IRShow.show_ir_stmt(io, ir, idx, line_info_preprinter, line_info_postprinter, ir.sptypes, used, ir.cfg, bb_idx)
        postprint(io, idx, bb_idx != bb_idx_prev)
        bb_idx_prev = bb_idx
    end
    max_bb_idx_size = ndigits(length(ir.cfg.blocks))
    line_info_preprinter(io, " "^(max_bb_idx_size + 2), 0)
    postprint(io)
    return nothing
end

# entries
# -------

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
    code_escapes(f, argtypes=Tuple{}; [world::UInt], [debuginfo::Symbol]) -> result::EscapeResult
    code_escapes(mi::MethodInstance; [world::UInt], [interp::EscapeAnalyzer], [debuginfo::Symbol]) -> result::EscapeResult

Runs the escape analysis on optimized IR of a generic function call with the given type signature,
while caching the analysis results.

# Keyword Arguments

- `world::UInt = Base.get_world_counter()`:
  controls the world age to use when looking up methods, use current world age if not specified.
- `cache_token::EscapeAnalyzerCacheToken = GLOBAL_EA_CACHE_TOKEN`:
  specifies the cache token to use, by default a global token is used so that the analysis
  can use the caches from previous invocations. If you with to use a fresh cache and perform
  a new analysis, specify a new `EscapeAnalyzerCacheToken` instance.
- `interp::EscapeAnalyzer = EscapeAnalyzer(world, cache_token)`:
  specifies the escape analyzer to use.
- `debuginfo::Symbol = :none`:
  controls the amount of code metadata present in the output, possible options are `:none` or `:source`.
"""
function code_escapes(@nospecialize(f), @nospecialize(types=Base.default_tt(f));
                      world::UInt = get_world_counter(),
                      cache_token::EscapeAnalyzerCacheToken = GLOBAL_EA_CACHE_TOKEN,
                      debuginfo::Symbol = :none)
    tt = Base.signature_type(f, types)
    match = Base._which(tt; world, raise=true)
    mi = Compiler.specialize_method(match)
    return code_escapes(mi; world, cache_token, debuginfo)
end

function code_escapes(mi::MethodInstance;
                      world::UInt = get_world_counter(),
                      cache_token::EscapeAnalyzerCacheToken = GLOBAL_EA_CACHE_TOKEN,
                      interp::EscapeAnalyzer=EscapeAnalyzer(world, cache_token; entry_mi=mi),
                      debuginfo::Symbol = :none)
    frame = Compiler.typeinf_frame(interp, mi, #=run_optimizer=#true)
    isdefined(interp, :result) || error("optimization didn't happen: maybe everything has been constant folded?")
    slotnames = let src = frame.src
        src isa CodeInfo ? src.slotnames : nothing
    end
    return EscapeResult(interp.result.ir, interp.result.estate, interp.result.mi,
                        slotnames, debuginfo === :source, interp)
end

"""
    code_escapes(ir::IRCode, nargs::Int; [world::UInt], [interp::AbstractInterpreter]) -> result::EscapeResult

Runs the escape analysis on `ir::IRCode`.
`ir` is supposed to be optimized already, specifically after inlining has been applied.
Note that this version does not cache the analysis results.

# Keyword Arguments

- `world::UInt = Base.get_world_counter()`:
  controls the world age to use when looking up methods, use current world age if not specified.
- `cache_token::EscapeAnalyzerCacheToken = GLOBAL_EA_CACHE_TOKEN`:
  specifies the cache token to use, by default a global token is used so that the analysis
  can use the caches from previous invocations. If you with to use a fresh cache and perform
  a new analysis, specify a new `EscapeAnalyzerCacheToken` instance.
- `interp::AbstractInterpreter = EscapeAnalyzer(world, cache_token)`:
  specifies the abstract interpreter to use, by default a new `EscapeAnalyzer` with an empty cache is created.
"""
function code_escapes(ir::IRCode, nargs::Int;
                      world::UInt = get_world_counter(),
                      cache_token::EscapeAnalyzerCacheToken = GLOBAL_EA_CACHE_TOKEN,
                      interp::AbstractInterpreter=EscapeAnalyzer(world, cache_token))
    estate = analyze_escapes(ir, nargs, Compiler.optimizer_lattice(interp), Compiler.get_escape_cache(interp))
    return EscapeResult(ir, estate) # return back the result
end

# in order to run a whole analysis from ground zero (e.g. for benchmarking, etc.)
__clear_cache!() = empty!(GLOBAL_EA_CODE_CACHE)

end # module EAUtils
