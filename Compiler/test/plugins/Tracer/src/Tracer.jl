# This file is a part of Julia. License is MIT: https://julialang.org/license

module Tracer

export trace

struct Call
    parent
    f
    args
    children
end

# TODO: Handle task-safety
const call_tree = ScopedValue{Ref{Call}}()

function prehook(f, args...)
    parent = call_tree[][]
    current = Call(parent, f, args, Call[])
    push!(parent.children, current)
    call_tree[][] = current
end

function posthook(_, f, args...)
    current = call_tree[][]
    call_tree[][] = current.parent
end

function trace(f, args...)
    top = Call(nothing, f, args, Call[])
    @with call_tree => Ref(top) begin
        Base.invoke_within(TracerCompiler(), f, args...)
    end
    return top
end

const CC = Core.Compiler
import .CC: SSAValue, GlobalRef

const COMPILER_WORLD = Ref{UInt}(0)
function __init__()
    COMPILER_WORLD[] = Base.get_world_counter()
end

struct TracerCompiler <: CC.AbstractCompiler end
CC.compiler_world(::TracerCompiler) = COMPILER_WORLD[]
CC.abstract_interpreter(compiler::TracerCompiler, world::UInt) =
    TracerInterp(compiler; world)

struct TracerInterp <: CC.AbstractInterpreter
    compiler::TracerCompiler
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    function TracerInterp(compiler::TracerCompiler;
                world::UInt = Base.get_world_counter(),
                inf_params::CC.InferenceParams = CC.InferenceParams(),
                opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[])
        return new(compiler, world, inf_params, opt_params, inf_cache)
    end
end

CC.InferenceParams(interp::TracerInterp) = interp.inf_params
CC.OptimizationParams(interp::TracerInterp) = interp.opt_params
CC.get_inference_world(interp::TracerInterp) = interp.world
CC.get_inference_cache(interp::TracerInterp) = interp.inf_cache
CC.cache_owner(interp::TracerInterp) = interp.compiler

import Core.Compiler: retrieve_code_info, maybe_validate_code
# Replace usage sited of `retrieve_code_info`, OptimizationState is one such, but in all interesting use-cases
# it is derived from an InferenceState. There is a third one in `typeinf_ext` in case the module forbids inference.
function CC.InferenceState(result::CC.InferenceResult, cache_mode::UInt8, interp::TracerInterp)
    world = CC.get_inference_world(interp)
    src = retrieve_code_info(result.linfo, world)
    src === nothing && return nothing
    maybe_validate_code(result.linfo, src, "lowered")
    src = transform(interp, result.linfo, src)
    maybe_validate_code(result.linfo, src, "transformed")
    return CC.InferenceState(result, src, cache_mode, interp)
end

function transform(src)
    return src
end

##
# Cassette style early transform
##

# Allows for Cassette Pass transforms

function static_eval(mod, name)
    if Base.isbindingresolved(mod, name) && Base.isdefined(mod, name)
        return getfield(mod, name)
    else
        return nothing
    end
end

function prehook end
function posthook end

function transform(interp, mi, src)
    method = mi.def
    f = static_eval(method.module, method.name)
    if f === Core._apply
        return src
    end
    if f isa Core.Builtin
        error("Transforming builtin")
    end
    ci = copy(src)
    transform!(mi, ci)
    return ci
end

function ir_element(x, code::Vector)
    while isa(x, Core.SSAValue)
        x = code[x.id]
    end
    return x
end

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

"""
    insert_statements!(code::Vector, codelocs::Vector, stmtcount, newstmts)


For every statement `stmt` at position `i` in `code` for which `stmtcount(stmt, i)` returns
an `Int`, remove `stmt`, and in its place, insert the statements returned by
`newstmts(stmt, i)`. If `stmtcount(stmt, i)` returns `nothing`, leave `stmt` alone.

For every insertion, all downstream `SSAValue`s, label indices, etc. are incremented
appropriately according to number of inserted statements.

Proper usage of this function dictates that following properties hold true:

- `code` is expected to be a valid value for the `code` field of a `CodeInfo` object.
- `codelocs` is expected to be a valid value for the `codelocs` field of a `CodeInfo` object.
- `newstmts(stmt, i)` should return a `Vector` of valid IR statements.
- `stmtcount` and `newstmts` must obey `stmtcount(stmt, i) == length(newstmts(stmt, i))` if
    `isa(stmtcount(stmt, i), Int)`.

To gain a mental model for this function's behavior, consider the following scenario. Let's
say our `code` object contains several statements:
code = Any[oldstmt1, oldstmt2, oldstmt3, oldstmt4, oldstmt5, oldstmt6]
codelocs = Int[1, 2, 3, 4, 5, 6]

Let's also say that for our `stmtcount` returns `2` for `stmtcount(oldstmt2, 2)`, returns `3`
for `stmtcount(oldstmt5, 5)`, and returns `nothing` for all other inputs. From this setup, we
can think of `code`/`codelocs` being modified in the following manner:
newstmts2 = newstmts(oldstmt2, 2)
newstmts5 = newstmts(oldstmt5, 5)
code = Any[oldstmt1,
           newstmts2[1], newstmts2[2],
           oldstmt3, oldstmt4,
           newstmts5[1], newstmts5[2], newstmts5[3],
           oldstmt6]
codelocs = Int[1, 2, 2, 3, 4, 5, 5, 5, 6]

See also: [`replace_match!`](@ref), [`is_ir_element`](@ref)
"""
function insert_statements!(src, stmtcount, newstmts)
    code = src.code
    codelocs = src.codelocs
    ssaflags = src.ssaflags
    ssachangemap = fill(0, length(code))
    labelchangemap = fill(0, length(code))
    worklist = Tuple{Int,Int}[]
    for i in 1:length(code)
        stmt = code[i]
        nstmts = stmtcount(stmt, i)
        if nstmts !== nothing
            addedstmts = nstmts - 1
            push!(worklist, (i, addedstmts))
            ssachangemap[i] = addedstmts
            if i < length(code)
                labelchangemap[i + 1] = addedstmts
            end
        end
    end
    Core.Compiler.renumber_ir_elements!(code, ssachangemap, labelchangemap)
    for (i, addedstmts) in worklist
        i += ssachangemap[i] - addedstmts # correct the index for accumulated offsets
        stmts = newstmts(code[i], i)
        @assert length(stmts) == (addedstmts + 1)
        code[i] = stmts[end]
        for j in 1:(length(stmts) - 1) # insert in reverse to maintain the provided ordering
            insert!(code, i, stmts[end - j])
            insert!(codelocs, i, codelocs[i])
            insert!(ssaflags, i, CC.IR_FLAG_NULL)
            src.ssavaluetypes += 1
        end
    end
end

function transform!(mi, src)
    stmtcount = (x, i) -> begin
        isassign = Base.Meta.isexpr(x, :(=))
        stmt = isassign ? x.args[2] : x
        if Base.Meta.isexpr(stmt, :call)
            return 4
        end
        return nothing
    end
    newstmts = (x, i) -> begin
        callstmt = Base.Meta.isexpr(x, :(=)) ? x.args[2] : x
        isapplycall = is_ir_element(callstmt.args[1], GlobalRef(Core, :_apply), src.code)
        isapplyiteratecall = is_ir_element(callstmt.args[1], GlobalRef(Core, :_apply_iterate), src.code)
        if isapplycall || isapplyiteratecall
            callf = callstmt.args[2]
            callargs = callstmt.args[3:end]
            stmts = Any[
                Expr(:call,
                     GlobalRef(Core, :_call_within), nothing,
                     prehook, callf, callargs...),
                callstmt,
                Expr(:call,
                     GlobalRef(Core, :_call_within), nothing,
                     posthook, SSAValue(i + 1), callf, callargs...),
                Base.Meta.isexpr(x, :(=)) ? Expr(:(=), x.args[1], SSAValue(i + 1)) : SSAValue(i + 1)
            ]
        else
            stmts = Any[
                Expr(:call, GlobalRef(Core, :_call_within), nothing, prehook, callstmt.args...),
                callstmt,
                Expr(:call, GlobalRef(Core, :_call_within), nothing, posthook, SSAValue(i + 1), callstmt.args...),
                Base.Meta.isexpr(x, :(=)) ? Expr(:(=), x.args[1], SSAValue(i + 1)) : SSAValue(i + 1)
            ]
        end
        return stmts
    end
    insert_statements!(src, stmtcount, newstmts)
    return nothing
end


# precompilation
precompile(CC.abstract_interpreter, (TracerCompiler, UInt))
precompile(CC.typeinf_ext_toplevel, (TracerInterp, CC.MethodInstance))

COMPILER_WORLD[] = Base.get_world_counter()
# Insert code execution statements here

# end precompile
COMPILER_WORLD[] = 0


end # module Tracer
