# This file is a part of Julia. License is MIT: https://julialang.org/license

const VALID_EXPR_HEADS = Symbol[:call, :invoke, :static_parameter, :line, :gotoifnot, :(=),
                                :method, :const, :null, :new, :return, :the_exception,
                                :enter, :leave, :inbounds, :boundscheck, :copyast, :meta]

const ASSIGNED_FLAG = 0x02

"""
    validate_code_info(c::CodeInfo)

Returns `0` if `c` represents valid IR, or returns a positive integer otherwise.

Below are the possible non-`0` return values and their conditions. As soon as
`validate_code_info` encounters a case where one of the below conditions evaluates
to `true`, it will return the corresponding error code.

`nslots` = the number of unique `SlotNumber`s in `c.code` (as well as `SlotNumber(1)`)
`nssavals` = the number of unique `SSAValue`s in `c.code`

1:  `length(c.slotflags) != nslots`
2:  `length(c.slotnames) != nslots`
3:  `c.inferred && length(c.slottypes) != nslots`
4:  `c.inferred && length(c.ssavaluetypes) != nssavals`
5:  `!c.inferred && c.slottypes != nothing`
6:  `!c.inferred && c.ssavaluetypes != nssavals`
7:  `!in(x.head, Base.Core.VALID_EXPR_HEADS)` for any subexpression `x`
8:  `length(c.slotnames) < 1`
9:  `!(isa(x, SSAValue) || isa(x, SlotNumber) || isa(x, GlobalRef))` where `x` is an assignment LHS
10: `isa(x, Expr) || x.head == :gotoifnot` where `x` is a function call argument
11:  A slot has an invalid slotflag setting for bit flag 2 (assignment property)
"""
function validate_code_info(c::CodeInfo)
    !c.inferred && (c.slottypes != nothing) && return 5
    length(c.slotnames) < 1 && return 8
    slotnums = SlotNumber[]
    ssavals = SSAValue[]
    error_code = 0
    walkast(c.code) do x
        if isa(x, Expr)
            if !in(x.head, VALID_EXPR_HEADS)
                error_code = 7
                return true
            elseif x.head == :(=) && !is_valid_lhs(x.args[1])
                error_code = 9
                return true
            elseif x.head == :call && !all(is_valid_call_arg(i) for i in x.args[2:end])
                error_code = 10
                return true
            end
        elseif isa(x, SSAValue) && !in(x, ssavals)
            push!(ssavals, x)
        elseif isa(x, SlotNumber) && !in(x, slotnums)
            push!(slotnums, x)
        end
        return false
    end
    error_code != 0 && return error_code
    length(c.slotnames) != length(c.slotflags) && return 1
    if c.inferred
        length(c.slottypes) != length(c.slotnames)  && return 3
        length(c.ssavaluetypes) != length(ssavals) && return 4
    else
        c.ssavaluetypes != length(ssavals) && return 6
    end
    error_code = 0
    walkast(c.code) do x
        if isa(x, Expr) && x.head == :(=)
            lhs = x.args[1]
            if isa(lhs, SlotNumber) && !is_flag_set(c.slotflags[lhs.id], ASSIGNED_FLAG)
                error_code = 11
                return true
            end
        end
        return false
    end
    return error_code
end

function walkast(f, stmts::Array)
    for stmt in stmts
        f(stmt) && return true
        if isa(stmt, Expr)
            walkast(f, stmt.args) && return true
        end
    end
    return false
end

is_valid_lhs(lhs) = isa(lhs, SlotNumber) || isa(lhs, SSAValue) || isa(lhs, GlobalRef)

function is_valid_call_arg(arg)
    isa(arg, Expr) && return !in(arg.head, (:gotoifnot, :new, :line, :const, :meta))
    return !isa(arg, GotoNode) && !isa(arg, LabelNode) && !isa(arg, LineNumberNode)
end

is_flag_set(byte::UInt8, flag::UInt8) = (byte & flag) == flag
