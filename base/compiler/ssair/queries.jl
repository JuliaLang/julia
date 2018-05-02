"""
Determine whether a statement is side-effect-free, i.e. may be removed if it has no uses.
"""
function stmt_effect_free(@nospecialize(stmt), src, mod::Module)
    isa(stmt, Union{PiNode, PhiNode}) && return true
    isa(stmt, Union{ReturnNode, GotoNode, GotoIfNot}) && return false
    isa(stmt, GlobalRef) && return isdefined(stmt.mod, stmt.name)
    (isa(stmt, Symbol) || isa(stmt, SSAValue) || isa(stmt, Argument)) && return true
    isa(stmt, Slot) && return false # Slots shouldn't occur in the IR at this point, but let's be defensive here
    if isa(stmt, Expr)
        e = stmt::Expr
        head = e.head
        is_meta_expr_head(head) && return true
        if head === :static_parameter
            # if we aren't certain enough about the type, it might be an UndefVarError at runtime
            return isa(e.typ, Const) || issingletontype(widenconst(e.typ))
        end
        (e.typ === Bottom) && return false
        ea = e.args
        if head === :call
            f = exprtype(ea[1], src, mod)
            if isa(f, Const)
                f = f.val
            elseif isType(f)
                f = f.parameters[1]
            else
                return false
            end
            f === return_type && return true
            # TODO: This needs significant refinement
            contains_is(_PURE_BUILTINS, f) || return false
            return builtin_nothrow(f, Any[exprtype(ea[i], src, mod) for i = 2:length(ea)])
        elseif head === :new
            a = ea[1]
            typ = exprtype(a, src, mod)
            # `Expr(:new)` of unknown type could raise arbitrary TypeError.
            typ, isexact = instanceof_tfunc(typ)
            isexact || return false
            isconcretedispatch(typ) || return false
            typ = typ::DataType
            fieldcount(typ) >= length(ea) - 1 || return false
            for fld_idx in 1:(length(ea) - 1)
                eT = exprtype(ea[fld_idx + 1], src, mod)
                fT = fieldtype(typ, fld_idx)
                eT ⊑ fT || return false
            end
            return true
        elseif head === :isdefined || head === :the_exception || head === :copyast
            return true
        else
            return false
        end
    end
    return true
end

function abstract_eval_ssavalue(s::SSAValue, src::IRCode)
    return src.types[s.id]
end

function abstract_eval_ssavalue(s::SSAValue, src::IncrementalCompact)
    return types(src)[s]
end

function compact_exprtype(compact::IncrementalCompact, @nospecialize(value))
    if isa(value, Union{SSAValue, OldSSAValue})
        return types(compact)[value]
    elseif isa(value, Argument)
        return compact.ir.argtypes[value.n]
    end
    return exprtype(value, compact.ir, compact.ir.mod)
end

is_tuple_call(ir::IRCode, @nospecialize(def)) = isa(def, Expr) && is_known_call(def, tuple, ir, ir.mod)
is_tuple_call(compact::IncrementalCompact, @nospecialize(def)) = isa(def, Expr) && is_known_call(def, tuple, compact)
function is_known_call(e::Expr, @nospecialize(func), src::IncrementalCompact)
    if e.head !== :call
        return false
    end
    f = compact_exprtype(src, e.args[1])
    return isa(f, Const) && f.val === func
end
