# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Determine whether a statement is side-effect-free, i.e. may be removed if it has no uses.
"""
function stmt_effect_free(@nospecialize(stmt), @nospecialize(rt), src, sptypes::Vector{Any})
    isa(stmt, PiNode) && return true
    isa(stmt, PhiNode) && return true
    isa(stmt, ReturnNode) && return false
    isa(stmt, GotoNode) && return false
    isa(stmt, GotoIfNot) && return false
    isa(stmt, Slot) && return false # Slots shouldn't occur in the IR at this point, but let's be defensive here
    isa(stmt, GlobalRef) && return isdefined(stmt.mod, stmt.name)
    if isa(stmt, Expr)
        e = stmt::Expr
        head = e.head
        if head === :static_parameter
            etyp = sptypes[e.args[1]]
            # if we aren't certain enough about the type, it might be an UndefVarError at runtime
            return isa(etyp, Const)
        end
        ea = e.args
        if head === :call
            f = argextype(ea[1], src, sptypes)
            f = singleton_type(f)
            f === nothing && return false
            is_return_type(f) && return true
            if isa(f, IntrinsicFunction)
                intrinsic_effect_free_if_nothrow(f) || return false
                return intrinsic_nothrow(f,
                        Any[argextype(ea[i], src, sptypes) for i = 2:length(ea)])
            end
            contains_is(_PURE_BUILTINS, f) && return true
            contains_is(_PURE_OR_ERROR_BUILTINS, f) || return false
            rt === Bottom && return false
            return _builtin_nothrow(f, Any[argextype(ea[i], src, sptypes) for i = 2:length(ea)], rt)
        elseif head === :new
            a = ea[1]
            typ = argextype(a, src, sptypes)
            # `Expr(:new)` of unknown type could raise arbitrary TypeError.
            typ, isexact = instanceof_tfunc(typ)
            isexact || return false
            isconcretedispatch(typ) || return false
            typ = typ::DataType
            fieldcount(typ) >= length(ea) - 1 || return false
            for fld_idx in 1:(length(ea) - 1)
                eT = argextype(ea[fld_idx + 1], src, sptypes)
                fT = fieldtype(typ, fld_idx)
                eT ⊑ fT || return false
            end
            return true
        elseif head === :isdefined || head === :the_exception || head === :copyast || head === :inbounds || head === :boundscheck
            return true
        else
            # e.g. :loopinfo
            return false
        end
    end
    return true
end

function abstract_eval_ssavalue(s::SSAValue, src::IRCode)
    return types(src)[s]
end

function abstract_eval_ssavalue(s::SSAValue, src::IncrementalCompact)
    return types(src)[s]
end

function compact_exprtype(compact::IncrementalCompact, @nospecialize(value))
    if isa(value, AnySSAValue)
        return types(compact)[value]
    elseif isa(value, Argument)
        return compact.ir.argtypes[value.n]
    end
    return argextype(value, compact.ir, compact.ir.sptypes)
end

is_tuple_call(ir::IRCode, @nospecialize(def)) = isa(def, Expr) && is_known_call(def, tuple, ir, ir.sptypes)
is_tuple_call(compact::IncrementalCompact, @nospecialize(def)) = isa(def, Expr) && is_known_call(def, tuple, compact)
function is_known_call(e::Expr, @nospecialize(func), src::IncrementalCompact)
    if e.head !== :call
        return false
    end
    f = compact_exprtype(src, e.args[1])
    return singleton_type(f) === func
end
