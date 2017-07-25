# This file is a part of Julia. License is MIT: https://julialang.org/license

const VALID_EXPR_HEADS = Symbol[:call, :invoke, :static_parameter, :line, :gotoifnot, :(=),
                                :method, :const, :null, :new, :return, :the_exception,
                                :enter, :leave, :inbounds, :boundscheck, :copyast, :meta]

const ASSIGNED_FLAG = 0x02

struct InvalidCodeError <: Exception
    msg::String
end

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo)

Validates `c`, logging any violations in `errors`.

Any violation of the following properties will be logged by pushing a `InvalidCodeError`
into `errors`:

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
function validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo)
    ssavals = SSAValue[]
    walkast(c.code) do x
        if isa(x, Expr)
            if !in(x.head, VALID_EXPR_HEADS)
                push!(errors, InvalidCodeError("encountered invalid expression head $(x.head)"))
            elseif x.head == :(=) && !is_valid_lhs(x.args[1])
                push!(errors, InvalidCodeError("encountered invalid LHS value $(x.args[1])"))
            elseif x.head == :call
                for i in 2:length(x.args)
                    if !is_valid_call_arg(x.args[i])
                        push!(errors, InvalidCodeError("encountered invalid call argument $(x.args[i])"))
                    end
                end
            end
        elseif isa(x, SSAValue) && !in(x, ssavals)
            push!(ssavals, x)
        end
    end
    if isempty(c.slotnames)
        push!(errors, InvalidCodeError("slotnames field is empty"))
    end
    if length(c.slotnames) != length(c.slotflags)
        push!(errors, InvalidCodeError("length(slotflags) != length(slotnames); $(length(c.slotflags)) != $(length(c.slotnames))"))
    end
    if c.inferred
        if length(c.slottypes) != length(c.slotnames)
            push!(errors, InvalidCodeError("length(slottypes) != length(slotnames); $(length(c.slottypes)) != $(length(c.slotnames))"))
        end
        if length(c.ssavaluetypes) < length(ssavals)
            missing = length(ssavals) - length(c.ssavaluetypes)
            push!(errors, InvalidCodeError("not all SSAValues in AST have a type in ssavaluetypes ($missing SSAValue types are missing)"))
        end
    else
        if c.slottypes != nothing
            push!(errors, InvalidCodeError("uninferred CodeInfo slottypes field should be `nothing`, instead it is $(c.slottypes)"))
        end
        if c.ssavaluetypes != length(ssavals)
            push!(errors, InvalidCodeError("uninferred CodeInfo ssavaluetypes field should be $(length(ssavals)), instead it is $(c.ssavaluetypes)"))
        end
    end
    walkast(c.code) do x
        if isa(x, Expr) && x.head == :(=)
            lhs = x.args[1]
            if isa(lhs, SlotNumber) && !is_flag_set(c.slotflags[lhs.id], ASSIGNED_FLAG)
                push!(errors, InvalidCodeError("slot $(lhs.id) has wrong assignment slotflag setting (bit flag 2 is not set, but should be)"))
            end
        end
    end
    return errors
end

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, m::Method)

Validates `m`, logging any violations in `errors`.

After all `Method` checks are complete, `validate_code!(errors, uncompress_ast(m))` is called.

Any violation of the following properties will be logged by pushing a `InvalidCodeError`
into `errors` (where `c = uncompress_ast(m)`):

- `length(m.sig.parameters) == m.nargs`
- `m.isva == isa(last(m.sig.parameters), Vararg)`
- `m.nargs <= length(c.slotnames)`
- `h != :method` for any subexpression head `h` if `m.nargs > 0`
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, m::Method)
    if length(m.sig.parameters) != m.nargs
        push!(errors, InvalidCodeError("number of types in method signature ($(length(m.sig.parameters))) does not match number of arguments ($(m.nargs))"))
    end
    if m.isva != isa(last(m.sig.parameters), Vararg)
        push!(errors, InvalidCodeError("last type in method signature ($(last(m.sig.parameters))) does not match `isva` field setting ($(m.isva))"))
    end
    c = ccall(:jl_uncompress_ast, Any, (Any, Any), m, m.source)
    if m.nargs > 0
        walkast(c.code) do x
            if isa(x, Expr) && x.head == :method
                push!(errors, InvalidCodeError("encountered `Expr` head `:method` in non-top-level code (i.e. `nargs` > 0)"))
            end
        end
    end
    if m.nargs > length(c.slotnames)
        push!(errors, InvalidCodeError("CodeInfo for method does contains fewer slotnames than the number of method arguments"))
    end
    validate_code!(errors, c)
    return errors
end

validate_code(x) = validate_code!(Vector{InvalidCodeError}(), x)

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
