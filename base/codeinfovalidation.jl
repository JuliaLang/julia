# This file is a part of Julia. License is MIT: https://julialang.org/license

const VALID_EXPR_HEADS = Symbol[:call, :invoke, :static_parameter, :line, :gotoifnot, :(=),
                                :method, :const, :null, :new, :return, :the_exception,
                                :enter, :leave, :inbounds, :boundscheck, :copyast, :meta]

const ASSIGNED_FLAG = 0x02

struct CodeInfoError <: Exception
    msg::String
end

"""
    validate_code_info(c::CodeInfo)

Returns a `Vector{CodeInfoError}` containing any errors encountered while validating `c`.

If the returned vector is empty, then the following properties hold true for `c`:

- `in(h, Base.Core.Inference.VALID_EXPR_HEADS)` for each subexpression head `h`
- `Base.Core.Inference.is_valid_lhs(lhs)` for each encountered LHS value `lhs`
- `Base.Core.Inference.is_valid_call_arg(arg)` for each call argument `arg`
- `!isempty(c.slotnames)`
- `length(c.slotnames) == length(c.slotflags)`
- if `c.inferred`:
    - `length(c.slottypes) == length(c.slotnames)`
    - all SSAValues in AST have a type in `c.ssavaluetypes`
- if `!c.inferred`:
    - `c.slottypes == nothing`
    - `c.ssavaluetypes` should be set to the number of SSAValues in `c.code`
- all assigned-to slots have bit flag 2 set in their respective slotflags
"""
function validate_code_info(c::CodeInfo)
    errors = Vector{CodeInfoError}()
    ssavals = SSAValue[]
    walkast(c.code) do x
        if isa(x, Expr)
            if !in(x.head, VALID_EXPR_HEADS)
                push!(errors, CodeInfoError("encountered invalid expression head $(x.head)"))
            elseif x.head == :(=) && !is_valid_lhs(x.args[1])
                push!(errors, CodeInfoError("encountered invalid LHS value $(x.args[1])"))
            elseif x.head == :call
                for i in 2:length(x.args)
                    if !is_valid_call_arg(x.args[i])
                        push!(errors, CodeInfoError("encountered invalid call argument $(x.args[i])"))
                    end
                end
            end
        elseif isa(x, SSAValue) && !in(x, ssavals)
            push!(ssavals, x)
        end
    end
    if isempty(c.slotnames)
        push!(errors, CodeInfoError("slotnames field is empty"))
    end
    if length(c.slotnames) != length(c.slotflags)
        push!(errors, CodeInfoError("lengths(slotflags) != length(slotnames); $(length(c.slotflags)) != $(length(c.slotnames))"))
    end
    if c.inferred
        if length(c.slottypes) != length(c.slotnames)
            push!(errors, CodeInfoError("lengths(slottypes) != length(slotnames); $(length(c.slottypes)) != $(length(c.slotnames))"))
        end
        if length(c.ssavaluetypes) < length(ssavals)
            missing = length(ssavals) - length(c.ssavaluetypes)
            push!(errors, CodeInfoError("not all SSAValues in AST have a type in ssavaluetypes ($missing SSAValue types are missing)"))
        end
    else
        if c.slottypes != nothing
            push!(errors, CodeInfoError("uninferred CodeInfo slottypes field should be `nothing`, instead it is $(c.slottypes)"))
        end
        if c.ssavaluetypes != length(ssavals)
            push!(errors, CodeInfoError("uninferred CodeInfo ssavaluetypes field should be $(length(ssavals)), instead it is $(c.ssavaluetypes)"))
        end
    end
    walkast(c.code) do x
        if isa(x, Expr) && x.head == :(=)
            lhs = x.args[1]
            if isa(lhs, SlotNumber) && !is_flag_set(c.slotflags[lhs.id], ASSIGNED_FLAG)
                push!(errors, CodeInfoError("slot $(lhs.id) has wrong assignment slotflag setting (bit flag 2 is not set, but should be)"))
            end
        end
    end
    return errors
end

function walkast(f, stmts::Array)
    for stmt in stmts
        f(stmt)
        isa(stmt, Expr) && walkast(f, stmt.args)
    end
end

is_valid_lhs(lhs) = isa(lhs, SlotNumber) || isa(lhs, SSAValue) || isa(lhs, GlobalRef)

function is_valid_call_arg(arg)
    isa(arg, Expr) && return !in(arg.head, (:gotoifnot, :new, :line, :const, :meta))
    return !isa(arg, GotoNode) && !isa(arg, LabelNode) && !isa(arg, LineNumberNode)
end

is_flag_set(byte::UInt8, flag::UInt8) = (byte & flag) == flag
