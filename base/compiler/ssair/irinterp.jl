
function codeinst_to_ir(code::CodeInstance)
    return copy(code.inferred.ir)
end

struct IREvalState
    frame::InferenceState
    ir::IRCode
end

function tristate_merge!(ir::IRCode, e::Effects)
    nothing
end

function collect_const_args2(argtypes::Vector{Any})
    return Any[ let a = widenconditional(argtypes[i])
                    isa(a, Const) ? a.val :
                    isa(a, ConstType) ? a.val :
                    isconstType(a) ? (a::DataType).parameters[1] :
                    (a::DataType).instance
                end for i in 1:length(argtypes) ]
end

function ir_abstract_constant_propagation(interp::AbstractInterpreter, mi_cache, frame::InferenceState, ir, argtypes)
    argtypes_refined = Bool[!(ir.argtypes[i] ⊑ argtypes[i]) for i = 1:length(argtypes)]
    empty!(ir.argtypes)
    append!(ir.argtypes, argtypes)
    ssa_refined = BitSet()

    sv = IREvalState(frame, ir)

    rt = Union{}

    for idx = 1:length(ir.stmts)
        inst = ir.stmts[idx][:inst]
        typ = ir.stmts[idx][:type]
        any_refined = false
        for ur in userefs(inst)
            val = ur[]
            if isa(val, Argument)
                any_refined = argtypes_refined[val.n]
            elseif isa(val, SSAValue)
                any_refined = val.id in ssa_refined
            end
            any_refined && break
        end
        if !any_refined
            if isa(inst, ReturnNode)
                isdefined(inst, :val) || continue
                rt = tmerge(rt, argextype(inst.val, ir))
            end
            continue
        end
        if isa(inst, Expr) || isa(inst, PhiNode)
            if isa(inst, PhiNode) || inst.head === :call || inst.head === :new
                rr = abstract_eval_statement(interp, inst, nothing, ir)
                if !(typ ⊑ rr)
                    push!(ssa_refined, idx)
                    ir.stmts[idx][:type] = rr
                end
            elseif inst.head === :invoke
                mi′ = inst.args[1]
                argtypes = collect_argtypes(interp, inst.args[2:end], nothing, ir)
                code = get(mi_cache, mi′, nothing)
                if code !== nothing
                    effects = decode_effects(code.ipo_purity_bits)
                    if is_total_or_error(effects) && is_all_const_arg(argtypes)
                        args = collect_const_args2(argtypes)
                        try
                            value = Core._call_in_world_total(get_world_counter(interp), args...)
                            if is_inlineable_constant(value) || call_result_unused(sv)
                                # If the constant is not inlineable, still do the const-prop, since the
                                # code that led to the creation of the Const may be inlineable in the same
                                # circumstance and may be optimizable.
                                rr = Const(value)
                            end
                        catch
                            rr = Union{}
                        end
                    else
                        ir′ = codeinst_to_ir(code)
                        rr = ir_abstract_constant_propagation(interp, mi_cache, frame, ir′, argtypes)
                    end
                    if !(typ ⊑ rr)
                        push!(ssa_refined, idx)
                        ir.stmts[idx][:type] = rr
                    end
                end
            else
                error()
            end
        elseif isa(inst, ReturnNode)
            if isdefined(inst, :val)
                rt = tmerge(rt, argextype(inst.val, ir))
            end
        elseif isa(inst, GotoIfNot) || isa(inst, GotoNode)
        else
            error()
        end
    end
    if rt === Union{}
        error()
    end
    return rt
    error()
end
