const EA_AS_PKG = Symbol(@__MODULE__) !== :Base # develop EA as an external package

module EAUtils

import ..EA_AS_PKG
if EA_AS_PKG
    import ..EscapeAnalysis
else
    import Core.Compiler.EscapeAnalysis: EscapeAnalysis
    Base.getindex(estate::EscapeAnalysis.EscapeState, @nospecialize(x)) =
        Core.Compiler.getindex(estate, x)
end
const EA = EscapeAnalysis
const CC = Core.Compiler

# entries
# -------

@static if EA_AS_PKG
import InteractiveUtils: gen_call_with_extracted_types_and_kwargs

@doc """
    @code_escapes [options...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`code_escapes`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of `code_escapes` keyword arguments can be given
as the optional arguments like `@code_escpase interp=myinterp myfunc(myargs...)`.
"""
macro code_escapes(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :code_escapes, ex0)
end
end # @static if EA_AS_PKG

"""
    code_escapes(f, argtypes=Tuple{}; [world], [interp]) -> result::EscapeResult
    code_escapes(tt::Type{<:Tuple}; [world], [interp]) -> result::EscapeResult

Runs the escape analysis on optimized IR of a genefic function call with the given type signature.
Note that the escape analysis runs after inlining, but before any other optimizations.

```julia
julia> mutable struct SafeRef{T}
           x::T
       end

julia> Base.getindex(x::SafeRef) = x.x;

julia> Base.isassigned(x::SafeRef) = true;

julia> get′(x) = isassigned(x) ? x[] : throw(x);

julia> result = code_escapes((String,String,String)) do s1, s2, s3
           r1 = Ref(s1)
           r2 = Ref(s2)
           r3 = SafeRef(s3)
           try
               s1 = get′(r1)
               ret = sizeof(s1)
           catch err
               global g = err # will definitely escape `r1`
           end
           s2 = get′(r2)      # still `r2` doesn't escape fully
           s3 = get′(r3)      # still `r2` doesn't escape fully
           return s2, s3
       end
#3(X _2::String, ↑ _3::String, ↑ _4::String) in Main at REPL[7]:2
2  X  1 ── %1  = %new(Base.RefValue{String}, _2)::Base.RefValue{String}   │╻╷╷ Ref
3  *′ │    %2  = %new(Base.RefValue{String}, _3)::Base.RefValue{String}   │╻╷╷ Ref
4  ✓′ └─── %3  = %new(SafeRef{String}, _4)::SafeRef{String}               │╻╷  SafeRef
5  ◌  2 ── %4  = \$(Expr(:enter, #8))                                      │
   ✓′ │    %5  = ϒ (%3)::SafeRef{String}                                  │
   *′ └─── %6  = ϒ (%2)::Base.RefValue{String}                            │
6  ◌  3 ── %7  = Base.isdefined(%1, :x)::Bool                             │╻╷  get′
   ◌  └───       goto #5 if not %7                                        ││
   X  4 ──       Base.getfield(%1, :x)::String                            ││╻   getindex
   ◌  └───       goto #6                                                  ││
   ◌  5 ──       Main.throw(%1)::Union{}                                  ││
   ◌  └───       unreachable                                              ││
7  ◌  6 ──       nothing::typeof(Core.sizeof)                             │╻   sizeof
   ◌  │          nothing::Int64                                           ││
   ◌  └───       \$(Expr(:leave, 1))                                       │
   ◌  7 ──       goto #10                                                 │
   ✓′ 8 ── %17 = φᶜ (%5)::SafeRef{String}                                 │
   *′ │    %18 = φᶜ (%6)::Base.RefValue{String}                           │
   ◌  └───       \$(Expr(:leave, 1))                                       │
   X  9 ── %20 = \$(Expr(:the_exception))::Any                             │
9  ◌  │          (Main.g = %20)::Any                                      │
   ◌  └───       \$(Expr(:pop_exception, :(%4)))::Any                      │
11 ✓′ 10 ┄ %23 = φ (#7 => %3, #9 => %17)::SafeRef{String}                 │
   *′ │    %24 = φ (#7 => %2, #9 => %18)::Base.RefValue{String}           │
   ◌  │    %25 = Base.isdefined(%24, :x)::Bool                            ││╻   isassigned
   ◌  └───       goto #12 if not %25                                      ││
   ↑  11 ─ %27 = Base.getfield(%24, :x)::String                           │││╻   getproperty
   ◌  └───       goto #13                                                 ││
   ◌  12 ─       Main.throw(%24)::Union{}                                 ││
   ◌  └───       unreachable                                              ││
12 ↑  13 ─ %31 = Base.getfield(%23, :x)::String                           │╻╷╷ get′
13 ↑  │    %32 = Core.tuple(%27, %31)::Tuple{String, String}              │
   ◌  └───       return %32                                               │
```

The symbols in the side of each call argument and SSA statements represents the following meaning:
- `◌`: this value is not analyzed because escape information of it won't be used anyway (when the object is `isbitstype` for example)
- `✓`: this value never escapes (`has_no_escape(result.state[x])` holds)
- `↑`: this value can escape to the caller via return (`has_return_escape(result.state[x])` holds)
- `X`: this value can escape to somewhere the escape analysis can't reason about like escapes to a global memory (`has_all_escape(result.state[x])` holds)
- `*`: this value's escape state is between the `ReturnEscape` and `AllEscape` in the `EscapeLattice`, e.g. it has unhandled `ThrownEscape`
and additional `′` indicates that field analysis has been done successfully on that value.

For testing, escape information of each call argument and SSA value can be inspected programmatically as like:
```julia
julia> result.state[Core.Argument(3)]
ReturnEscape

julia> result.state[Core.SSAValue(3)]
NoEscape′
```
"""
function code_escapes(@nospecialize(args...);
                      world = get_world_counter(),
                      interp = Core.Compiler.NativeInterpreter(world))
    interp = EscapeAnalyzer(interp)
    results = code_typed(args...; optimize=true, world, interp)
    isone(length(results)) || throw(ArgumentError("`code_escapes` only supports single analysis result"))
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
    @timeit,
    get_inference_cache,
    convert_to_ircode,
    slot2reg,
    compact!,
    ssa_inlining_pass!,
    sroa_pass!,
    adce_pass!,
    type_lift_pass!,
    JLOptions,
    verify_ir,
    verify_linetable
# usings
import Core:
    CodeInstance, MethodInstance, CodeInfo
import .CC:
    OptimizationState, IRCode
import .EA:
    analyze_escapes, cache_escapes!

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

function run_passes_with_ea(interp::EscapeAnalyzer, ci::CodeInfo, sv::OptimizationState)
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
    local state
    try
        @timeit "collect escape information" state = analyze_escapes(ir, nargs)
    catch err
        @info "error happened within `analyze_escapes`, insepct `Main.ir` and `Main.nargs`"
        @eval Main (ir = $ir; nargs = $nargs)
        rethrow(err)
    end
    cacheir = Core.Compiler.copy(ir)
    # cache this result
    cache_escapes!(sv.linfo, state, cacheir)
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

# printing
# --------

import Core: Argument, SSAValue
import .CC: widenconst, singleton_type
import .EA: EscapeLattice, EscapeState, ⊑, ⊏

# in order to run a whole analysis from ground zero (e.g. for benchmarking, etc.)
__clear_caches!() = (__clear_code_cache!(); EA.__clear_escape_cache!())

function get_name_color(x::EscapeLattice, symbol::Bool = false)
    getname(x) = string(nameof(x))
    if x === EA.⊥
        name, color = (getname(EA.NotAnalyzed), "◌"), :plain
    elseif EA.has_no_escape(x)
        name, color = (getname(EA.NoEscape), "✓"), :green
    elseif EA.has_all_escape(x)
        name, color = (getname(EA.AllEscape), "X"), :red
    elseif EA.NoEscape() ⊏ (EA.ignore_thrownescapes ∘ EA.ignore_aliasescapes)(x) ⊑ EA.AllReturnEscape()
        name = (getname(EA.ReturnEscape), "↑")
        color = EA.has_thrown_escape(x) ? :yellow : :cyan
    else
        name = (nothing, "*")
        color = EA.has_thrown_escape(x) ? :yellow : :bold
    end
    name = symbol ? last(name) : first(name)
    if name !== nothing && !isa(x.AliasEscapes, Bool)
        name = string(name, "′")
    end
    return name, color
end

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
            printstyled(io, c, ' ', '_', i, "::", ir.argtypes[i]; color)
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
