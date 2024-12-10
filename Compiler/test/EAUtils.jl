module EAUtils

export code_escapes, @code_escapes, __clear_cache!

include("setup_Compiler.jl")

using .Compiler: EscapeAnalysis as EA

# AbstractInterpreter
# -------------------

# imports
import .Compiler:
    AbstractInterpreter, NativeInterpreter, WorldView, WorldRange, InferenceParams,
    OptimizationParams, get_world_counter, get_inference_cache, ipo_dataflow_analysis!
# usings
using Core.IR
using .Compiler: InferenceResult, InferenceState, OptimizationState, IRCode
using .EA: analyze_escapes, ArgEscapeCache, ArgEscapeInfo, EscapeInfo, EscapeResult

mutable struct EscapeAnalyzerCacheToken end
global GLOBAL_EA_CACHE_TOKEN::EscapeAnalyzerCacheToken = EscapeAnalyzerCacheToken()

struct EscapeAnalysisResultForEntry
    ir::IRCode
    eresult::EscapeResult
    mi::MethodInstance
end

mutable struct EscapeAnalyzer <: AbstractInterpreter
    const world::UInt
    const inf_params::InferenceParams
    const opt_params::OptimizationParams
    const inf_cache::Vector{InferenceResult}
    const token::EscapeAnalyzerCacheToken
    const entry_mi::Union{Nothing,MethodInstance}
    result::EscapeAnalysisResultForEntry
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
    eresult = try
        analyze_escapes(ir, nargs, GetEscapeCache())
    catch err
        @error "error happened within EA, inspect `Main.failedanalysis`"
        failedanalysis = FailedAnalysis(caller, ir, nargs)
        Core.eval(Main, :(failedanalysis = $failedanalysis))
        rethrow(err)
    end
    if caller.linfo === interp.entry_mi
        # return back the result
        interp.result = EscapeAnalysisResultForEntry(Compiler.copy(ir), eresult, caller.linfo)
    end
    record_escapes!(caller, eresult, ir)

    @invoke Compiler.ipo_dataflow_analysis!(interp::AbstractInterpreter, opt::OptimizationState,
                                            ir::IRCode, caller::InferenceResult)
end

# cache entire escape state for inspection and debugging
struct EscapeCacheInfo
    argescapes::ArgEscapeCache
    eresult::EscapeResult # preserved just for debugging purpose
    ir::IRCode            # preserved just for debugging purpose
end

function record_escapes!(caller::InferenceResult, eresult::EscapeResult, ir::IRCode)
    argescapes = ArgEscapeCache(eresult)
    ecacheinfo = EscapeCacheInfo(argescapes, eresult, ir)
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

using .Compiler: widenconst, singleton_type

function get_name_color(x::Union{Nothing,EscapeInfo}, symbol::Bool = false)
    getname(x) = string(nameof(x))
    if x === nothing
        name, color = ("NotAnalyzed", "◌"), :plain
    elseif EA.has_no_escape(EA.ignore_argescape(x))
        if EA.has_arg_escape(x)
            name, color = ("ArgEscape", "✓"), :blue
        else
            name, color = ("NoEscape", "✓"), :green
        end
    elseif EA.has_all_escape(x)
        name, color = ("AllEscape", "X"), :red
    elseif EA.has_return_escape(x)
        name = ("ReturnEscape", "↑")
        color = EA.has_thrown_escape(x) ? :yellow : :blue
    else
        name = (nothing, "*")
        color = EA.has_thrown_escape(x) ? :yellow : :bold
    end
    name = symbol ? last(name) : first(name)
    if name !== nothing && x !== nothing && isa(x.ObjectInfo, EA.HasIndexableFields)
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

struct EscapeAnalysisResult
    ir::IRCode
    eresult::EscapeResult
    mi::Union{Nothing,MethodInstance}
    slotnames::Union{Nothing,Vector{Symbol}}
    source::Bool
    interp::Union{Nothing,EscapeAnalyzer}
    function EscapeAnalysisResult(ir::IRCode, eresult::EscapeResult,
                                  mi::Union{Nothing,MethodInstance}=nothing,
                                  slotnames::Union{Nothing,Vector{Symbol}}=nothing,
                                  source::Bool=false,
                                  interp::Union{Nothing,EscapeAnalyzer}=nothing)
        return new(ir, eresult, mi, slotnames, source, interp)
    end
end
Base.getindex(res::EscapeAnalysisResult, @nospecialize(x)) = res.eresult[x]
EA.getaliases(res::EscapeAnalysisResult, args...) = EA.getaliases(res.eresult, args...)
EA.isaliased(res::EscapeAnalysisResult, args...) = EA.isaliased(res.eresult, args...)
EA.is_load_forwardable(res::EscapeAnalysisResult, pc::Int) = EA.is_load_forwardable(res.eresult, pc)
@eval Base.iterate(res::EscapeAnalysisResult, s=1) =
    return s > $(fieldcount(EscapeAnalysisResult)) ? nothing : (getfield(res, s), s+1)

Base.show(io::IO, ecacheinfo::EscapeCacheInfo) = show(io, EscapeAnalysisResult(ecacheinfo.ir, ecacheinfo.eresult))

using Compiler: IRShow
function Base.show(io::IO, result::EscapeAnalysisResult, bb::Int=0)
    (; ir, eresult, mi, slotnames, source) = result
    if bb ≠ 0
        bbstate = eresult.bbescapes[bb]
        ssamemoryinfo = nothing
    else
        bbstate = eresult.retescape
        ssamemoryinfo = eresult.ssamemoryinfo
    end

    io = IOContext(io, :displaysize=>displaysize(io))

    # print escape information on SSA values
    function print_header(io::IO)
        ft = ir.argtypes[1]
        f = singleton_type(ft)
        if f === nothing
            f = widenconst(ft)
        end
        print(io, f, '(')
        for i in 1:bbstate.nargs
            arginfo = bbstate[Argument(i)]
            i == 1 && continue
            c, color = get_name_color(arginfo, true)
            slot = isnothing(slotnames) ? "_$i" : slotnames[i]
            printstyled(io, c, ' ', slot, "::", ir.argtypes[i]; color)
            i ≠ bbstate.nargs && print(io, ", ")
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
    function print_header(io::IO, idx::Int)
        c, color = get_name_color(bbstate[SSAValue(idx)], true)
        # printstyled(io, lpad(idx, nd), ' ', c, ' '; color)
        printstyled(io, rpad(c, 2), ' '; color)
    end

    lineprinter = IRShow.inline_linfo_printer(ir)
    preprinter = function (@nospecialize(io::IO), linestart::String, idx::Int)
        str = lineprinter(io, linestart, idx)
        if idx ≠ 0
            c, color = get_name_color(bbstate[SSAValue(idx)], true)
            return str * sprint(;context=IOContext(io)) do @nospecialize io::IO
                print(io, " ")
                printstyled(io, rpad(c, 2); color)
            end
        end
        return str
    end

    _postprinter = IRShow.default_expr_type_printer
    postprinter = if ssamemoryinfo !== nothing
        function (io::IO; idx::Int, @nospecialize(kws...))
            _postprinter(io; idx, kws...)
            if haskey(ssamemoryinfo, idx)
                memoryinfo = ssamemoryinfo[idx]
                if memoryinfo === EA.ConflictedMemory()
                    c, color = "*", :yellow
                elseif memoryinfo === EA.UnknownMemory()
                    c, color = "X", :red
                elseif memoryinfo === EA.UninitializedMemory()
                    c, color = "◌", :yellow
                else
                    c = sprint(context=IOContext(io)) do @nospecialize io::IO
                        Base.show_unquoted(io, memoryinfo)
                    end
                    color = :cyan
                end
                printstyled(io, " (↦ "; color=:light_black)
                printstyled(io, c; color)
                printstyled(io, ")"; color=:light_black)
            end
        end
    else
        _postprinter
    end

    bb_color = :normal
    irshow_config = IRShow.IRShowConfig(preprinter, postprinter; bb_color)

    print_header(io)
    IRShow.show_ir(io, ir, irshow_config)
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
    code_escapes(f, argtypes=Tuple{}; [world::UInt], [debuginfo::Symbol]) -> result::EscapeAnalysisResult
    code_escapes(mi::MethodInstance; [world::UInt], [interp::EscapeAnalyzer], [debuginfo::Symbol]) -> result::EscapeAnalysisResult

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
    return EscapeAnalysisResult(interp.result.ir, interp.result.eresult, interp.result.mi,
                                slotnames, debuginfo === :source, interp)
end

"""
    code_escapes(ir::IRCode, nargs::Int; [world::UInt], [interp::AbstractInterpreter]) -> result::EscapeAnalysisResult

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
    eresult = analyze_escapes(ir, nargs, Compiler.get_escape_cache(interp))
    return EscapeAnalysisResult(ir, eresult) # return back the result
end

# in order to run a whole analysis from ground zero (e.g. for benchmarking, etc.)
__clear_cache!() = empty!(GLOBAL_EA_CODE_CACHE)

end # module EAUtils
