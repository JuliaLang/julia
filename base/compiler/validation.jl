# This file is a part of Julia. License is MIT: https://julialang.org/license

# Expr head => argument count bounds
const VALID_EXPR_HEADS = IdDict{Symbol,UnitRange}(
    :call => 1:typemax(Int),
    :invoke => 2:typemax(Int),
    :static_parameter => 1:1,
    :(&) => 1:1,
    :(=) => 2:2,
    :method => 1:4,
    :const => 1:1,
    :new => 1:typemax(Int),
    :splatnew => 2:2,
    :the_exception => 0:0,
    :enter => 1:1,
    :leave => 1:1,
    :pop_exception => 1:1,
    :inbounds => 1:1,
    :boundscheck => 0:0,
    :copyast => 1:1,
    :meta => 0:typemax(Int),
    :global => 1:1,
    :foreigncall => 5:typemax(Int), # name, RT, AT, nreq, cconv, args..., roots...
    :cfunction => 5:5,
    :isdefined => 1:1,
    :code_coverage_effect => 0:0,
    :loopinfo => 0:typemax(Int),
    :gc_preserve_begin => 0:typemax(Int),
    :gc_preserve_end => 0:typemax(Int),
    :thunk => 1:1,
    :throw_undef_if_not => 2:2,
    :aliasscope => 0:0,
    :popaliasscope => 0:0,
    :new_opaque_closure => 4:typemax(Int)
)

# @enum isn't defined yet, otherwise I'd use it for this
const INVALID_EXPR_HEAD = "invalid expression head"
const INVALID_EXPR_NARGS = "invalid number of expression args"
const INVALID_LVALUE = "invalid LHS value"
const INVALID_RVALUE = "invalid RHS value"
const INVALID_RETURN = "invalid argument to return"
const INVALID_CALL_ARG = "invalid :call argument"
const EMPTY_SLOTNAMES = "slotnames field is empty"
const SLOTFLAGS_MISMATCH = "length(slotnames) < length(slotflags)"
const SSAVALUETYPES_MISMATCH = "not all SSAValues in AST have a type in ssavaluetypes"
const SSAVALUETYPES_MISMATCH_UNINFERRED = "uninferred CodeInfo ssavaluetypes field does not equal the number of present SSAValues"
const NON_TOP_LEVEL_METHOD = "encountered `Expr` head `:method` in non-top-level code (i.e. `nargs` > 0)"
const NON_TOP_LEVEL_GLOBAL = "encountered `Expr` head `:global` in non-top-level code (i.e. `nargs` > 0)"
const SIGNATURE_NARGS_MISMATCH = "method signature does not match number of method arguments"
const SLOTNAMES_NARGS_MISMATCH = "CodeInfo for method contains fewer slotnames than the number of method arguments"

struct InvalidCodeError <: Exception
    kind::AbstractString
    meta::Any
end
InvalidCodeError(kind::AbstractString) = InvalidCodeError(kind, nothing)

function validate_code_in_debug_mode(linfo::MethodInstance, src::CodeInfo, kind::String)
    if JLOptions().debug_level == 2
        # this is a debug build of julia, so let's validate linfo
        errors = validate_code(linfo, src)
        if !isempty(errors)
            for e in errors
                if linfo.def isa Method
                    println(stderr, "WARNING: Encountered invalid ", kind, " code for method ",
                            linfo.def, ": ", e)
                else
                    println(stderr, "WARNING: Encountered invalid ", kind, " code for top level expression in ",
                            linfo.def, ": ", e)
                end
            end
        end
    end
end

function _validate_val!(@nospecialize(x), errors, ssavals::BitSet)
    if isa(x, Expr)
        if x.head === :call || x.head === :invoke
            f = x.args[1]
            if f isa GlobalRef && (f.name === :cglobal) && x.head === :call
                # TODO: these are not yet linearized
            else
                for arg in x.args
                    if !is_valid_argument(arg)
                        push!(errors, InvalidCodeError(INVALID_CALL_ARG, arg))
                    else
                        _validate_val!(arg, errors, ssavals)
                    end
                end
            end
        end
    elseif isa(x, SSAValue)
        id = x.id
        !in(id, ssavals) && push!(ssavals, id)
    end
    return
end

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo)

Validate `c`, logging any violation by pushing an `InvalidCodeError` into `errors`.
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo, is_top_level::Bool = false)
    ssavals = BitSet()
    lhs_slotnums = BitSet()

    # Do not define recursive function as closure to work around
    # boxing of the function itself as `Core.Box`.
    validate_val!(@nospecialize(x)) = _validate_val!(x, errors, ssavals)

    for x in c.code
        if isa(x, Expr)
            head = x.head
            if !is_top_level
                head === :method && push!(errors, InvalidCodeError(NON_TOP_LEVEL_METHOD))
                head === :global && push!(errors, InvalidCodeError(NON_TOP_LEVEL_GLOBAL))
            end
            narg_bounds = get(VALID_EXPR_HEADS, head, -1:-1)
            nargs = length(x.args)
            if narg_bounds == -1:-1
                push!(errors, InvalidCodeError(INVALID_EXPR_HEAD, (head, x)))
            elseif !in(nargs, narg_bounds)
                push!(errors, InvalidCodeError(INVALID_EXPR_NARGS, (head, nargs, x)))
            elseif head === :(=)
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
                validate_val!(lhs)
                validate_val!(rhs)
            elseif head === :call || head === :invoke || head === :gc_preserve_end || head === :meta ||
                head === :inbounds || head === :foreigncall || head === :cfunction ||
                head === :const || head === :enter || head === :leave || head === :pop_exception ||
                head === :method || head === :global || head === :static_parameter ||
                head === :new || head === :splatnew || head === :thunk || head === :loopinfo ||
                head === :throw_undef_if_not || head === :code_coverage_effect
                validate_val!(x)
            else
                # TODO: nothing is actually in statement position anymore
                #push!(errors, InvalidCodeError("invalid statement", x))
            end
        elseif isa(x, NewvarNode)
        elseif isa(x, GotoNode)
        elseif isa(x, GotoIfNot)
            if !is_valid_argument(x.cond)
                push!(errors, InvalidCodeError(INVALID_CALL_ARG, x.cond))
            end
            validate_val!(x.cond)
        elseif isa(x, ReturnNode)
            if isdefined(x, :val)
                if !is_valid_return(x.val)
                    push!(errors, InvalidCodeError(INVALID_RETURN, x.val))
                end
                validate_val!(x.val)
            end
        elseif x === nothing
        elseif isa(x, SlotNumber)
        elseif isa(x, Argument)
        elseif isa(x, GlobalRef)
        elseif isa(x, LineNumberNode)
        elseif isa(x, PiNode)
        elseif isa(x, PhiCNode)
        elseif isa(x, PhiNode)
        elseif isa(x, UpsilonNode)
        else
            #push!(errors, InvalidCodeError("invalid statement", x))
        end
    end
    nslotnames = length(c.slotnames)
    nslotflags = length(c.slotflags)
    nssavals = length(c.code)
    !is_top_level && nslotnames == 0 && push!(errors, InvalidCodeError(EMPTY_SLOTNAMES))
    nslotnames < nslotflags && push!(errors, InvalidCodeError(SLOTFLAGS_MISMATCH, (nslotnames, nslotflags)))
    if c.inferred
        nssavaluetypes = length(c.ssavaluetypes)
        nssavaluetypes < nssavals && push!(errors, InvalidCodeError(SSAVALUETYPES_MISMATCH, (nssavals, nssavaluetypes)))
    else
        c.ssavaluetypes != nssavals && push!(errors, InvalidCodeError(SSAVALUETYPES_MISMATCH_UNINFERRED, (nssavals, c.ssavaluetypes)))
    end
    return errors
end

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, mi::MethodInstance,
                   c::Union{Nothing,CodeInfo} = Core.Compiler.retrieve_code_info(mi))

Validate `mi`, logging any violation by pushing an `InvalidCodeError` into `errors`.

If `isa(c, CodeInfo)`, also call `validate_code!(errors, c)`. It is assumed that `c` is
the `CodeInfo` instance associated with `mi`.
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, mi::Core.MethodInstance,
                        c::Union{Nothing,CodeInfo} = Core.Compiler.retrieve_code_info(mi))
    is_top_level = mi.def isa Module
    if is_top_level
        mnargs = 0
    else
        m = mi.def::Method
        mnargs = m.nargs
        n_sig_params = length(Core.Compiler.unwrap_unionall(m.sig).parameters)
        if (m.isva ? (n_sig_params < (mnargs - 1)) : (n_sig_params != mnargs))
            push!(errors, InvalidCodeError(SIGNATURE_NARGS_MISMATCH, (m.isva, n_sig_params, mnargs)))
        end
    end
    if isa(c, CodeInfo)
        mnargs > length(c.slotnames) && push!(errors, InvalidCodeError(SLOTNAMES_NARGS_MISMATCH))
        validate_code!(errors, c, is_top_level)
    end
    return errors
end

validate_code(args...) = validate_code!(Vector{InvalidCodeError}(), args...)

is_valid_lvalue(@nospecialize(x)) = isa(x, Slot) || isa(x, GlobalRef)

function is_valid_argument(@nospecialize(x))
    if isa(x, Slot) || isa(x, Argument) || isa(x, SSAValue) || isa(x, GlobalRef) || isa(x, QuoteNode) ||
        (isa(x,Expr) && (x.head in (:static_parameter, :boundscheck))) ||
        isa(x, Number) || isa(x, AbstractString) || isa(x, AbstractChar) || isa(x, Tuple) ||
        isa(x, Type) || isa(x, Core.Box) || isa(x, Module) || x === nothing
        return true
    end
    # TODO: consider being stricter about what needs to be wrapped with QuoteNode
    return !(isa(x,Expr) || isa(x,Symbol) || isa(x,GotoNode) ||
             isa(x,LineNumberNode) || isa(x,NewvarNode))
end

function is_valid_rvalue(@nospecialize(x))
    is_valid_argument(x) && return true
    if isa(x, Expr) && x.head in (:new, :splatnew, :the_exception, :isdefined, :call, :invoke, :foreigncall, :cfunction, :gc_preserve_begin, :copyast)
        return true
    end
    return false
end

is_valid_return(@nospecialize(x)) = is_valid_argument(x) || (isa(x, Expr) && x.head === :lambda)

is_flag_set(byte::UInt8, flag::UInt8) = (byte & flag) == flag
