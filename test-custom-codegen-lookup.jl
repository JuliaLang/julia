using InteractiveUtils, Test
using Core: MethodInstance, CodeInstance
using Base: CodegenParams
const CC = Core.Compiler
include("test/compiler/newinterp.jl")

@newinterp ConstInvokeInterp

function CC.concrete_eval_eligible(interp::ConstInvokeInterp,
    @nospecialize(f), result::CC.MethodCallResult, arginfo::CC.ArgInfo, sv::CC.AbsIntState)
    ret = @invoke CC.concrete_eval_eligible(interp::CC.AbstractInterpreter,
        f::Any, result::CC.MethodCallResult, arginfo::CC.ArgInfo, sv::CC.AbsIntState)
    if ret === :semi_concrete_eval
        return :none # disable semi-concrete interpretation
    end
    return ret
end

function CC.abstract_call_known(interp::ConstInvokeInterp, @nospecialize(f),
    arginfo::CC.ArgInfo, si::CC.StmtInfo, sv::CC.AbsIntState,
    max_methods::Int)
    if f === sin && arginfo.argtypes[2] == Float64
        Core.println("[absint] hit")
        return CC.CallMeta(Core.Const(0.0), CC.Effects(), CC.NoCallInfo())
    end
    return @invoke CC.abstract_call_known(interp::CC.AbstractInterpreter, f::Any,
        arginfo::CC.ArgInfo, si::CC.StmtInfo, sv::CC.AbsIntState,
        max_methods::Int)
end

global runtime_target::Int
Base.@constprop :aggressive @noinline function target(c::Bool, x::Int)
    if c
        println("[runtime] hit ", x)
        global runtime_target = x
        y = sin(x)
        z = nothing
    else
        y = cos(x)
        z = missing
    end
    return y, z
end
context(x::Int) = target(true, x)

world = Base.get_world_counter()
interp = ConstInvokeInterp(; world)
function custom_lookup(mi::MethodInstance, min_world::UInt, max_world::UInt)
    local matched_mi = nothing
    for inf_result in interp.inf_cache
        if inf_result.linfo === mi
            if CC.any(inf_result.overridden_by_const)
                Core.println("[custom_lookup] hit")
                return CodeInstance(interp, inf_result, inf_result.src, inf_result.valid_worlds)
            elseif matched_mi === nothing
                matched_mi = inf_result.linfo
            end
        end
    end
    matched_mi === nothing && return nothing
    return interp.code_cache.dict[matched_mi]
end

ir, = only(Base.code_ircode(context; world, interp))

raw = false
lookup = @cfunction(custom_lookup, Any, (Any,Csize_t,Csize_t))
params = CodegenParams(; lookup)

oc = Core.OpaqueClosure(ir; params)

runtime_target = 0
@test oc(42) == (0.0, nothing)
@test runtime_target == 42
