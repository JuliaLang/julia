function stmt_effect_free(@nospecialize(stmt), src::IRCode, mod::Module)
    isa(stmt, Union{PiNode, PhiNode}) && return true
    isa(stmt, Union{ReturnNode, GotoNode, GotoIfNot}) && return false
    return statement_effect_free(stmt, src, mod)
end

function abstract_eval_ssavalue(s::SSAValue, src::IRCode)
    return src.types[s.id]
end
