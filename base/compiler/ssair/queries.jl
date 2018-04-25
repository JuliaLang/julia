function stmt_effect_free(@nospecialize(stmt), src, mod::Module)
    isa(stmt, Union{PiNode, PhiNode}) && return true
    isa(stmt, Union{ReturnNode, GotoNode, GotoIfNot}) && return false
    return effect_free(stmt, src, mod, true)
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
