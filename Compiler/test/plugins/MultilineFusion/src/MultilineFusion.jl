module MultilineFusion

export multiline_fusion

function multiline_fusion(f, args...)
    Base.invoke_within(MLFCompiler(), f, args...)
end

const CC = Core.Compiler
import .CC: SSAValue, GlobalRef

const COMPILER_WORLD = Ref{UInt}(0)
function __init__()
    COMPILER_WORLD[] = Base.get_world_counter()
end

struct MLFCompiler <: CC.AbstractCompiler end
CC.compiler_world(::MLFCompiler) = COMPILER_WORLD[]
CC.abstract_interpreter(compiler::MLFCompiler, world::UInt) =
    MLFInterp(compiler; world)

struct MLFInterp <: CC.AbstractInterpreter
    compiler::MLFCompiler
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    function MLFInterp(compiler::MLFCompiler = MLFCompiler();
                world::UInt = Base.get_world_counter(),
                inf_params::CC.InferenceParams = CC.InferenceParams(),
                opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[])
        return new(compiler, world, inf_params, opt_params, inf_cache)
    end
end

CC.InferenceParams(interp::MLFInterp) = interp.inf_params
CC.OptimizationParams(interp::MLFInterp) = interp.opt_params
CC.get_inference_world(interp::MLFInterp) = interp.world
CC.get_inference_cache(interp::MLFInterp) = interp.inf_cache
CC.cache_owner(interp::MLFInterp) = interp.compiler

import Core.Compiler: retrieve_code_info, maybe_validate_code
# Replace usage sited of `retrieve_code_info`, OptimizationState is one such, but in all interesting use-cases
# it is derived from an InferenceState. There is a third one in `typeinf_ext` in case the module forbids inference.
function CC.InferenceState(result::CC.InferenceResult, cache_mode::UInt8, interp::MLFInterp)
    world = CC.get_inference_world(interp)
    src = retrieve_code_info(result.linfo, world)
    src === nothing && return nothing
    maybe_validate_code(result.linfo, src, "lowered")
    src = transform(interp, result.linfo, src)
    maybe_validate_code(result.linfo, src, "transformed")
    return CC.InferenceState(result, src, cache_mode, interp)
end

function transform(interp, mi, src)
    ci = copy(src)
    transform!(mi, ci)
    return ci
end

import .CC: userefs, UseRefIterator, UseRef

Base.iterate(useref::UseRefIterator, state...) = CC.iterate(useref, state...)
Base.getindex(useref::UseRef) = CC.getindex(useref)
Base.setindex!(useref::UseRef, x) = CC.setindex!(useref, x)

"""
    is_ir_element(x, y, code::Vector)

Return `true` if `x === y` or if `x` is an `SSAValue` such that
`is_ir_element(code[x.id], y, code)` is `true`.
See also: [`replace_match!`](@ref), [`insert_statements!`](@ref)
"""
function is_ir_element(x, y, code::Vector)
    result = false
    while true # break by default
        if x === y #
            result = true
            break
        elseif isa(x, Core.SSAValue)
            x = code[x.id]
        else
            break
        end
    end
    return result
end

mutable struct MaterializeAnalysis
    def::SSAValue
    forward::Any
    removable::Bool
    MaterializeAnalysis(def, forward) = new(def, forward, true)
end

"""
    transform!

The goal of this transform is to perform multi-line fusion of broadcast statements.
This needs to happen before inference since the broadcast machinery depends on inference
to perform the actual fusion.

```
C = A .* B
D = C .+ A
```

Is lowered to

```
1 ─ %1 = Base.broadcasted(Main.:*, A, B)
│        C = Base.materialize(%1)
│   %3 = C
│   %4 = Base.broadcasted(Main.:+, %3, A)
│   %5 = Base.materialize(%4)
│        D = %5
└──      return %5
```

We find all calls to `materialize` and find all their uses.
If the materialize statment is used inside a `broadcasted`
statment we forward the argument to the `broadcasted`.

If it is only used for broadcasted statements we delete
the call to `Base.materialize`.

```
1 ─ %1 = Base.broadcasted(Main.:*, A, B)
│        C = nothing
│   %3 = C
│   %4 = Base.broadcasted(Main.:+, %1, A)
│   %5 = Base.materialize(%4)
│        D = %5
└──      return %5
```
"""
function transform!(mi, src)
    materialize = Base.IdDict{Any, MaterializeAnalysis}()
    for (i, x) in enumerate(src.code)
        isassign = Base.Meta.isexpr(x, :(=))
        stmt = isassign ? x.args[2] : x
        if Base.Meta.isexpr(stmt, :call)
            if is_ir_element(stmt.args[1], GlobalRef(Base, :materialize), src.code)
                manalysis = MaterializeAnalysis(SSAValue(i), stmt.args[2])
                if isassign
                    materialize[x.args[1]] = manalysis
                end
                materialize[SSAValue(i)] = manalysis
                continue
            end
            if is_ir_element(stmt.args[1], GlobalRef(Base, :broadcasted), src.code)
                for op in userefs(stmt)
                    use = op[]
                    manalysis = get(materialize, use, nothing)
                    if manalysis !== nothing
                        op[] = manalysis.forward
                    end
                end
            end
        elseif isassign
            if haskey(materialize, x.args[2])
                materialize[x.args[1]] = materialize[x.args[2]]
            end
        elseif x isa CC.SlotNumber || x isa CC.SSAValue
            if haskey(materialize, x)
                materialize[SSAValue(i)] = materialize[x]
            end
        else
            for op in userefs(stmt)
                use = op[]
                manalysis = get(materialize, use, nothing)
                if manalysis !== nothing
                    manalysis.removable = false
                end
            end
        end
    end

    for manalysis in unique(values(materialize))
        if manalysis.removable
            x = src.code[manalysis.def.id]
            if Base.Meta.isexpr(x, :(=))
                x.args[2] = nothing
            else
                src.code[manalysis.def.id] = nothing
            end
        end
    end
    return nothing
end


# precompilation
precompile(CC.abstract_interpreter, (MLFCompiler, UInt))
precompile(CC.typeinf_ext_toplevel, (MLFInterp, CC.MethodInstance))

COMPILER_WORLD[] = Base.get_world_counter()
# Insert code execution statements here

# end precompile
COMPILER_WORLD[] = 0

end # module MultilineFusion
