function stmt_effect_free(@nospecialize(stmt), src, mod::Module)
    isa(stmt, Union{PiNode, PhiNode}) && return true
    isa(stmt, Union{ReturnNode, GotoNode, GotoIfNot}) && return false
    return effect_free(stmt, src, mod, true) # TODO: the old optimizer (void_use_elim_pass) used false here, for correctness
end

function abstract_eval_ssavalue(s::SSAValue, src::IRCode)
    return src.types[s.id]
end

function abstract_eval_ssavalue(s::SSAValue, src::IncrementalCompact)
    return types(src)[s]
end
