# This file is a part of Julia. License is MIT: https://julialang.org/license

const VALID_EXPR_HEADS = Symbol[:call, :invoke, :static_parameter, :line, :gotoifnot, :(=),
                                :method, :const, :null, :new, :return, :the_exception,
                                :enter, :leave, :inbounds, :boundscheck, :copyast, :meta]
"""
    is_valid_code_info(code_info::CodeInfo)

Returns `true` if `code_info` is valid, and returns `false` otherwise.

`code_info` is considered valid if the following properties hold true (where `nslotnums` is
the number of `SlotNumber`s in `code_info.code` and `nssavals` is the number of `SSAValue`s
in `code_info.code`):

- `length(code_info.slotflags) == length(code_info.slotnames) == nslotnums`
- `code_info.slottypes === nothing || length(code_info.slottypes) == nslotnums`
- `code_info.ssatypes === nssavals || length(code_info.ssatypes) == nssavals`
- `Expr` heads are contained in `Base.Core.VALID_EXPR_HEADS`
- slotflags are valid w.r.t. assignments in `code_info.code`
- LHS objects in assignments are either `SSAValue`s, `SlotNumber`s, or `GlobalRef`s
- no call arguments are `:gotoifnot` nodes
"""
function is_valid_code_info(code_info::CodeInfo)
end
