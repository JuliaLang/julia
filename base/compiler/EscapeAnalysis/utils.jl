module EAUtils

import ..EscapeAnalysis: EscapeAnalysis
const EA = EscapeAnalysis
const CC = Core.Compiler

let
    README = normpath(dirname(@__DIR__), "README.md")
    include_dependency(README)
    @doc read(README, String) EA
end

let __init_hooks__ = []
    global __init__() = foreach(f->f(), __init_hooks__)
    global register_init_hook!(@nospecialize(f)) = push!(__init_hooks__, f)
end

# entries
# -------

using InteractiveUtils

macro analyze_escapes(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :analyze_escapes, ex0)
end

function analyze_escapes(@nospecialize(f), @nospecialize(types=Tuple{});
                         world = get_world_counter(),
                         interp = Core.Compiler.NativeInterpreter(world))
    interp = EscapeAnalyzer(interp)
    results = code_typed(f, types; optimize=true, world, interp)
    isone(length(results)) || throw(ArgumentError("`analyze_escapes` only supports single analysis result"))
    return EscapeResult(interp.ir, interp.state, interp.linfo)
end

# AbstractInterpreter
# -------------------

# imports
import .CC:
    AbstractInterpreter,
    NativeInterpreter,
    WorldView,
    WorldRange,
    InferenceParams,
    OptimizationParams,
    get_world_counter,
    get_inference_cache,
    lock_mi_inference,
    unlock_mi_inference,
    add_remark!,
    may_optimize,
    may_compress,
    may_discard_trees,
    verbose_stmt_info,
    code_cache,
    get_inference_cache
# usings
import Core:
    CodeInstance, MethodInstance
import .CC:
    OptimizationState, IRCode
import .EA:
    find_escapes, GLOBAL_ESCAPE_CACHE, EscapeCache

mutable struct EscapeAnalyzer{State} <: AbstractInterpreter
    native::NativeInterpreter
    ir::IRCode
    state::State
    linfo::MethodInstance
    EscapeAnalyzer(native::NativeInterpreter) = new{EscapeState}(native)
end

CC.InferenceParams(interp::EscapeAnalyzer)    = InferenceParams(interp.native)
CC.OptimizationParams(interp::EscapeAnalyzer) = OptimizationParams(interp.native)
CC.get_world_counter(interp::EscapeAnalyzer)  = get_world_counter(interp.native)

CC.lock_mi_inference(::EscapeAnalyzer,   ::MethodInstance) = nothing
CC.unlock_mi_inference(::EscapeAnalyzer, ::MethodInstance) = nothing

CC.add_remark!(interp::EscapeAnalyzer, sv, s) = add_remark!(interp.native, sv, s)

CC.may_optimize(interp::EscapeAnalyzer)      = may_optimize(interp.native)
CC.may_compress(interp::EscapeAnalyzer)      = may_compress(interp.native)
CC.may_discard_trees(interp::EscapeAnalyzer) = may_discard_trees(interp.native)
CC.verbose_stmt_info(interp::EscapeAnalyzer) = verbose_stmt_info(interp.native)

CC.get_inference_cache(interp::EscapeAnalyzer) = get_inference_cache(interp.native)

const GLOBAL_CODE_CACHE = IdDict{MethodInstance,CodeInstance}()
__clear_code_cache!() = empty!(GLOBAL_CODE_CACHE)

function CC.code_cache(interp::EscapeAnalyzer)
    worlds = WorldRange(get_world_counter(interp))
    return WorldView(GlobalCache(), worlds)
end

struct GlobalCache end

CC.haskey(wvc::WorldView{GlobalCache}, mi::MethodInstance) = haskey(GLOBAL_CODE_CACHE, mi)

CC.get(wvc::WorldView{GlobalCache}, mi::MethodInstance, default) = get(GLOBAL_CODE_CACHE, mi, default)

CC.getindex(wvc::WorldView{GlobalCache}, mi::MethodInstance) = getindex(GLOBAL_CODE_CACHE, mi)

function CC.setindex!(wvc::WorldView{GlobalCache}, ci::CodeInstance, mi::MethodInstance)
    GLOBAL_CODE_CACHE[mi] = ci
    add_callback!(mi) # register the callback on invalidation
    return nothing
end

function add_callback!(linfo)
    if !isdefined(linfo, :callbacks)
        linfo.callbacks = Any[invalidate_cache!]
    else
        if !any(@nospecialize(cb)->cb===invalidate_cache!, linfo.callbacks)
            push!(linfo.callbacks, invalidate_cache!)
        end
    end
    return nothing
end

function invalidate_cache!(replaced, max_world, depth = 0)
    delete!(GLOBAL_CODE_CACHE, replaced)

    if isdefined(replaced, :backedges)
        for mi in replaced.backedges
            mi = mi::MethodInstance
            if !haskey(GLOBAL_CODE_CACHE, mi)
                continue # otherwise fall into infinite loop
            end
            invalidate_cache!(mi, max_world, depth+1)
        end
    end
    return nothing
end

function CC.optimize(interp::EscapeAnalyzer, opt::OptimizationState, params::OptimizationParams, @nospecialize(result))
    ir = run_passes_with_ea(interp, opt.src, opt)
    return CC.finish(interp, opt, params, ir, result)
end

# HACK enable copy and paste from Core.Compiler
function run_passes_with_ea end
register_init_hook!() do
@eval CC begin
    function $(@__MODULE__).run_passes_with_ea(interp::$EscapeAnalyzer, ci::CodeInfo, sv::OptimizationState)
        @timeit "convert"   ir = convert_to_ircode(ci, sv)
        @timeit "slot2reg"  ir = slot2reg(ir, ci, sv)
        # TODO: Domsorting can produce an updated domtree - no need to recompute here
        @timeit "compact 1" ir = compact!(ir)
        @timeit "Inlining"  ir = ssa_inlining_pass!(ir, ir.linetable, sv.inlining, ci.propagate_inbounds)
        # @timeit "verify 2" verify_ir(ir)
        @timeit "compact 2" ir = compact!(ir)
        nargs = let def = sv.linfo.def
            isa(def, Method) ? Int(def.nargs) : 0
        end
        @timeit "collect escape information" state = $find_escapes(ir, nargs)
        cacheir = copy(ir)
        # cache this result
        $setindex!($GLOBAL_ESCAPE_CACHE, $EscapeCache(state, cacheir), sv.linfo)
        # return back the result
        interp.ir = cacheir
        interp.state = state
        interp.linfo = sv.linfo
        @timeit "SROA"      ir = sroa_pass!(ir)
        @timeit "ADCE"      ir = adce_pass!(ir)
        @timeit "type lift" ir = type_lift_pass!(ir)
        @timeit "compact 3" ir = compact!(ir)
        if JLOptions().debug_level == 2
            @timeit "verify 3" (verify_ir(ir); verify_linetable(ir.linetable))
        end
        return ir
    end
end
end # register_init_hook!() do

# printing
# --------

import Core: Argument, SSAValue
import .CC: widenconst, singleton_type
import .EA:
    EscapeLattice, EscapeState, TOP_ESCAPE_SITES, BOT_FIELD_SETS, ⊑, ⊏, __clear_escape_cache!

# in order to run a whole analysis from ground zero (e.g. for benchmarking, etc.)
__clear_caches!() = (__clear_code_cache!(); __clear_escape_cache!())

function get_name_color(x::EscapeLattice, symbol::Bool = false)
    getname(x) = string(nameof(x))
    if x == EA.NotAnalyzed()
        name, color = (getname(EA.NotAnalyzed), "◌"), :plain
    elseif EA.has_no_escape(x)
        name, color = (getname(EA.NoEscape), "✓"), :green
    elseif EA.NoEscape() ⊏ EA.ignore_fieldsets(x) ⊑ AllReturnEscape()
        name, color = (getname(EA.ReturnEscape), "↑"), :cyan
    elseif EA.NoEscape() ⊏ EA.ignore_fieldsets(x) ⊑ AllThrownEscape()
        name, color = (getname(EA.ThrownEscape), "↓"), :yellow
    elseif EA.has_all_escape(x)
        name, color = (getname(EA.AllEscape), "X"), :red
    else
        name, color = (nothing, "*"), :red
    end
    name = symbol ? last(name) : first(name)
    if name !== nothing && EA.has_fieldsets(x)
        name = string(name, "′")
    end
    return name, color
end

AllReturnEscape() = EscapeLattice(true, true, false, TOP_ESCAPE_SITES, BOT_FIELD_SETS)
AllThrownEscape() = EscapeLattice(true, false, true, TOP_ESCAPE_SITES, BOT_FIELD_SETS)

# pcs = sprint(show, collect(x.EscapeSites); context=:limit=>true)
function Base.show(io::IO, x::EscapeLattice)
    name, color = get_name_color(x)
    if isnothing(name)
        Base.@invoke show(io::IO, x::Any)
    else
        printstyled(io, name; color)
    end
end
function Base.show(io::IO, ::MIME"application/prs.juno.inline", x::EscapeLattice)
    name, color = get_name_color(x)
    if isnothing(name)
        return x # use fancy tree-view
    else
        printstyled(io, name; color)
    end
end

struct EscapeResult
    ir::IRCode
    state::EscapeState
    linfo::Union{Nothing,MethodInstance}
    EscapeResult(ir::IRCode, state::EscapeState, linfo::Union{Nothing,MethodInstance} = nothing) =
        new(ir, state, linfo)
end
Base.show(io::IO, result::EscapeResult) = print_with_info(io, result.ir, result.state, result.linfo)
@eval Base.iterate(res::EscapeResult, state=1) =
    return state > $(fieldcount(EscapeResult)) ? nothing : (getfield(res, state), state+1)

# adapted from https://github.com/JuliaDebug/LoweredCodeUtils.jl/blob/4612349432447e868cf9285f647108f43bd0a11c/src/codeedges.jl#L881-L897
function print_with_info(io::IO,
    ir::IRCode, state::EscapeState, linfo::Union{Nothing,MethodInstance})
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
            printstyled(io, '_', i, "::", ir.argtypes[i], ' ', c; color)
            i ≠ state.nargs && print(io, ", ")
        end
        print(io, ')')
        if !isnothing(linfo)
            def = linfo.def
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

    print_with_info(preprint, (args...)->nothing, io, ir)
end

function print_with_info(preprint, postprint, io::IO, ir::IRCode)
    io = IOContext(io, :displaysize=>displaysize(io))
    used = Base.IRShow.stmts_used(io, ir)
    # line_info_preprinter = Base.IRShow.lineinfo_disabled
    line_info_preprinter = function (io::IO, indent::String, idx::Int)
        r = Base.IRShow.inline_linfo_printer(ir)(io, indent, idx)
        idx ≠ 0 && preprint(io, idx)
        return r
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

using .EAUtils:
    analyze_escapes,
    @analyze_escapes
export
    analyze_escapes,
    @analyze_escapes
