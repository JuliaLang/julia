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
