# This file is a part of Julia. License is MIT: https://julialang.org/license

# Expr head => argument count bounds
const VALID_EXPR_HEADS = ObjectIdDict(
    :call => 1:typemax(Int),
    :invoke => 2:typemax(Int),
    :static_parameter => 1:1,
    :gotoifnot => 2:2,
    :(&) => 1:1,
    :(=) => 2:2,
    :method => 1:4,
    :const => 1:1,
    :null => 0:0, # TODO from @vtjnash: remove this + any :null handling code in Base
    :new => 1:typemax(Int),
    :return => 1:1,
    :the_exception => 0:0,
    :enter => 1:1,
    :leave => 1:1,
    :inbounds => 1:1,
    :boundscheck => 0:0,
    :copyast => 1:1,
    :meta => 0:typemax(Int),
    :global => 1:1,
    :foreigncall => 3:typemax(Int),
    :isdefined => 1:1,
    :simdloop => 0:0,
    :gc_preserve_begin => 0:typemax(Int),
    :gc_preserve_end => 0:typemax(Int)
)

# @enum isn't defined yet, otherwise I'd use it for this
const INVALID_EXPR_HEAD = "invalid expression head"
const INVALID_EXPR_NARGS = "invalid number of expression args"
const INVALID_LVALUE = "invalid LHS value"
const INVALID_RVALUE = "invalid RHS value"
const INVALID_CALL_ARG = "invalid :call argument"
const EMPTY_SLOTNAMES = "slotnames field is empty"
const SLOTFLAGS_MISMATCH = "length(slotnames) != length(slotflags)"
const SLOTTYPES_MISMATCH = "length(slotnames) != length(slottypes)"
const SLOTTYPES_MISMATCH_UNINFERRED = "uninferred CodeInfo slottypes field is not `nothing`"
const SSAVALUETYPES_MISMATCH = "not all SSAValues in AST have a type in ssavaluetypes"
const SSAVALUETYPES_MISMATCH_UNINFERRED = "uninferred CodeInfo ssavaluetypes field does not equal the number of present SSAValues"
const NON_TOP_LEVEL_METHOD = "encountered `Expr` head `:method` in non-top-level code (i.e. `nargs` > 0)"
const SIGNATURE_NARGS_MISMATCH = "method signature does not match number of method arguments"
const SLOTNAMES_NARGS_MISMATCH = "CodeInfo for method contains fewer slotnames than the number of method arguments"

struct InvalidCodeError <: Exception
    kind::AbstractString
    meta::Any
end
InvalidCodeError(kind::AbstractString) = InvalidCodeError(kind, nothing)


"""
    validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo)

Validate `c`, logging any violation by pushing an `InvalidCodeError` into `errors`.
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo, is_top_level::Bool = false)
    ssavals = IntSet()
    lhs_slotnums = IntSet()
    walkast(c.code) do x
        if isa(x, Expr)
            !is_top_level && x.head == :method && push!(errors, InvalidCodeError(NON_TOP_LEVEL_METHOD))
            narg_bounds = get(VALID_EXPR_HEADS, x.head, -1:-1)
            nargs = length(x.args)
            if narg_bounds == -1:-1
                push!(errors, InvalidCodeError(INVALID_EXPR_HEAD, (x.head, x)))
            elseif !in(nargs, narg_bounds)
                push!(errors, InvalidCodeError(INVALID_EXPR_NARGS, (x.head, nargs, x)))
            elseif x.head == :(=)
                lhs, rhs = x.args
                if !is_valid_lvalue(lhs)
                    push!(errors, InvalidCodeError(INVALID_LVALUE, lhs))
                elseif isa(lhs, SlotNumber) && !in(lhs.id, lhs_slotnums)
                    n = lhs.id
                    push!(lhs_slotnums, n)
                end
                if !is_valid_rvalue(rhs)
                    push!(errors, InvalidCodeError(INVALID_RVALUE, rhs))
                end
            elseif x.head == :call || x.head == :invoke
                for arg in x.args
                    if !is_valid_rvalue(arg)
                        push!(errors, InvalidCodeError(INVALID_CALL_ARG, arg))
                    end
                end
            end
        elseif isa(x, SSAValue)
            id = x.id + 1 # ensures that id > 0 for use with IntSet
            !in(id, ssavals) && push!(ssavals, id)
        end
    end
    nslotnames = length(c.slotnames)
    nslotflags = length(c.slotflags)
    nssavals = length(ssavals)
    nslotnames == 0 && push!(errors, InvalidCodeError(EMPTY_SLOTNAMES))
    nslotnames != nslotflags && push!(errors, InvalidCodeError(SLOTFLAGS_MISMATCH, (nslotnames, nslotflags)))
    if c.inferred
        nslottypes = length(c.slottypes)
        nssavaluetypes = length(c.ssavaluetypes)
        nslottypes != nslotnames && push!(errors, InvalidCodeError(SLOTTYPES_MISMATCH, (nslotnames, nslottypes)))
        nssavaluetypes < nssavals && push!(errors, InvalidCodeError(SSAVALUETYPES_MISMATCH, (nssavals, nssavaluetypes)))
    else
        c.slottypes !== nothing && push!(errors, InvalidCodeError(SLOTTYPES_MISMATCH_UNINFERRED, c.slottypes))
        c.ssavaluetypes != nssavals && push!(errors, InvalidCodeError(SSAVALUETYPES_MISMATCH_UNINFERRED, (nssavals, c.ssavaluetypes)))
    end
    return errors
end

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, mi::MethodInstance,
                   c::Union{Void,CodeInfo} = Core.Inference.retrieve_code_info(mi))

Validate `mi`, logging any violation by pushing an `InvalidCodeError` into `errors`.

If `isa(c, CodeInfo)`, also call `validate_code!(errors, c)`. It is assumed that `c` is
the `CodeInfo` instance associated with `mi`.
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, mi::Core.MethodInstance,
                        c::Union{Void,CodeInfo} = Core.Inference.retrieve_code_info(mi))
    m = mi.def::Method
    n_sig_params = length(Core.Inference.unwrap_unionall(m.sig).parameters)
    if (m.isva ? (n_sig_params < (m.nargs - 1)) : (n_sig_params != m.nargs))
        push!(errors, InvalidCodeError(SIGNATURE_NARGS_MISMATCH, (m.isva, n_sig_params, m.nargs)))
    end
    if isa(c, CodeInfo)
        m.nargs > length(c.slotnames) && push!(errors, InvalidCodeError(SLOTNAMES_NARGS_MISMATCH))
        validate_code!(errors, c, m.nargs == 0)
    end
    return errors
end

validate_code(args...) = validate_code!(Vector{InvalidCodeError}(), args...)

function walkast(f, stmts::Array)
    for stmt in stmts
        f(stmt)
        isa(stmt, Expr) && walkast(f, stmt.args)
    end
end

is_valid_lvalue(x) = isa(x, SlotNumber) || isa(x, SSAValue) || isa(x, GlobalRef)

function is_valid_rvalue(x)
    isa(x, Expr) && return !in(x.head, (:gotoifnot, :line, :const, :meta))
    return !isa(x, GotoNode) && !isa(x, LabelNode) && !isa(x, LineNumberNode)
end

is_flag_set(byte::UInt8, flag::UInt8) = (byte & flag) == flag
