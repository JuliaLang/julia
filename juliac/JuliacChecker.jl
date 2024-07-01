module JuliacChecker

export report_dyncall, @report_dyncall, report_dyncall_file

using JET.JETInterface
const CC = Core.Compiler

using Core: MethodInstance, Builtin
using .CC:
    MethodCallResult, ArgInfo, InferenceState, AbstractInterpreter, ConstCallResults,
    IRInterpretationState, argextype, singleton_type, InferenceResult, OptimizationState
using Base.Meta: isexpr
using JET: JET
using InteractiveUtils: gen_call_with_extracted_types_and_kwargs

struct JuliacAnalyzer <: AbstractAnalyzer
    state::AnalyzerState
    analysis_cache::AnalysisCache
end
function JuliacAnalyzer(world::UInt=Base.get_world_counter();
                        analysis_cache::AnalysisCache=JULIAC_ANALYZER_CACHE)
    state = AnalyzerState(world)
    return JuliacAnalyzer(state, analysis_cache)
end

const JULIAC_ANALYZER_CACHE = AnalysisCache()

# AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::JuliacAnalyzer) = analyzer.state
JETInterface.AbstractAnalyzer(analyzer::JuliacAnalyzer, state::AnalyzerState) = JuliacAnalyzer(state, analyzer.analysis_cache)
JETInterface.AnalysisCache(analyzer::JuliacAnalyzer) = analyzer.analysis_cache
JETInterface.vscode_diagnostics_order(analyzer::JuliacAnalyzer) = false # COMBAK

# AbstractInterpreter overloads
CC.InferenceParams(::JuliacAnalyzer) = CC.InferenceParams(;
    # TODO max_methods = ...
    unoptimize_throw_blocks=false)

function CC.const_prop_call(analyzer::JuliacAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
    concrete_eval_result::Union{Nothing,ConstCallResults})
    ret = @invoke CC.const_prop_call(analyzer::AbstractAnalyzer,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
        concrete_eval_result::Union{Nothing,ConstCallResults})
    if concrete_eval_result !== nothing
        # HACK disable the whole `OptAnalyzer` analysis as far as the frame has been concretized
        # (otherwise we may end up with useless reports from recursive calls)
        filter_lineages!(analyzer, sv.result, result.edge::MethodInstance)
    end
    return ret
end
function CC.const_prop_call(analyzer::JuliacAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::IRInterpretationState,
    concrete_eval_result::Union{Nothing,ConstCallResults})
    if concrete_eval_result !== nothing
        # HACK disable the whole `OptAnalyzer` analysis as far as the frame has been concretized
        # (otherwise we may end up with useless reports from recursive calls)
        return concrete_eval_result
    end
    return @invoke CC.const_prop_call(analyzer::AbstractInterpreter,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::IRInterpretationState,
        nothing::Nothing)
end

function CC.finish(frame::InferenceState, analyzer::JuliacAnalyzer)
    ret = @invoke CC.finish(frame::InferenceState, analyzer::AbstractAnalyzer)
    if CC.is_result_constabi_eligible(frame.result)
        # turn off optimization for this frame in order to achieve a minor perf gain,
        # similar to the effect of setting `may_discard_trees(::OptAnalyzer) = true`
        frame.result.src = nothing
    end
    return ret
end

function CC.finish!(analyzer::JuliacAnalyzer, frame::InferenceState)
    caller = frame.result

    # get the source before running `finish!` to keep the reference to `OptimizationState`
    src = caller.src
    if src isa OptimizationState{typeof(analyzer)}
        # allow the following analysis passes to see the optimized `CodeInfo`
        caller.src = CC.ir_to_codeinf!(src)
    end
    if src isa OptimizationState{typeof(analyzer)}
        report_dyncalls(analyzer, caller, src)
    else
        if src === nothing # the optimization didn't happen
        else # and this pass should never happen
            # NOTE `src` never be `CodeInfo` since `CC.may_discard_trees(::OptAnalyzer) === false`
            Core.eval(@__MODULE__, :(src = $src))
            throw("unexpected state happened, inspect `$(@__MODULE__).src`")
        end
    end

    return @invoke CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)
end

@jetreport struct DynamicCallReport <: InferenceErrorReport end
function JETInterface.print_report_message(io::IO, ::DynamicCallReport)
    print(io, "dynamic call detected")
end
function report_dyncalls(analyzer::JuliacAnalyzer, caller::InferenceResult, opt::OptimizationState)
    (; src, sptypes, slottypes) = opt

    # TODO better to work on `opt.ir::IRCode` (with some updates on `handle_sig!`)
    for (pc, x) in enumerate(src.code)
        lin = JET.get_lin((opt, pc))
        lin === nothing && continue # dead statement, just ignore it
        if lin.inlined_at ≠ 0
            # this statement has been inlined, so ignore it as any problems within
            # the callee should already have been reported
            continue
        end
        if isexpr(x, :call)
            ft = argextype(first(x.args), src, sptypes, slottypes)
            f = singleton_type(ft)
            f !== nothing && f isa Builtin && continue # ignore `:call`s of the language intrinsics
            add_new_report!(analyzer, caller, DynamicCallReport((opt, pc)))
        end
    end
end

# entries
# =======

JETInterface.valid_configurations(::JuliacAnalyzer) = JET.GENERAL_CONFIGURATIONS

"""
    report_dyncall(f, [types]; jetconfigs...) -> JETCallResult
    report_dyncall(tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult
    report_dyncall(mi::Core.MethodInstance; jetconfigs...) -> JETCallResult

Analyzes a function call with the given type signature to detect optimization failures and
unresolved method dispatches.
"""
function report_dyncall(args...; jetconfigs...)
    return analyze_and_report_call!(JuliacAnalyzer(), args...; jetconfigs...)
end

"""
    @report_dyncall f(args...)

Evaluates the arguments to a function call, determines their types, and then calls
[`report_dyncall`](@ref) on the resulting expression.
"""
macro report_dyncall(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :report_dyncall, ex0)
end

report_dyncall_file(filename::AbstractString; jetconfigs...) =
    analyze_and_report_file!(JuliacAnalyzer(), filename;
        analyze_from_definitions = :main,
        jetconfigs...)

end # module JuliacChecker
