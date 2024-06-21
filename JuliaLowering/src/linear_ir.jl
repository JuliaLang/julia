#-------------------------------------------------------------------------------
# Lowering pass 5: Flatten to linear IR

function is_simple_atom(ex)
    k = kind(ex)
    # FIXME
#   (or (number? x) (string? x) (char? x)
#       (and (pair? x) (memq (car x) '(ssavalue null true false thismodule)))
#       (eq? (typeof x) 'julia_value)))
    is_number(k) || k == K"String" || k == K"Char"
end

# N.B.: This assumes that resolve-scopes has run, so outerref is equivalent to
# a global in the current scope.
function is_valid_ir_argument(ex)
    k = kind(ex)
    return is_simple_atom(ex)
    # FIXME ||
           #(k == K"outerref" && nothrow_julia_global(ex[1]))  ||
           #(k == K"globalref" && nothrow_julia_global(ex))    ||
           #(k == K"quote" || k = K"inert" || k == K"top" ||
            #k == K"core" || k == K"slot" || k = K"static_parameter")
end

"""
Context for creating linear IR.

One of these is created per lambda expression to flatten the body down to
linear IR.
"""
struct LinearIRContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    code::SyntaxList{GraphType, Vector{NodeId}}
    next_var_id::Ref{Int}
    next_label_id::Ref{Int}
    is_toplevel_thunk::Bool
    lambda_locals::Set{VarId}
    return_type::Union{Nothing,NodeId}
    var_info::Dict{VarId,VarInfo}
    mod::Module
end

function LinearIRContext(ctx, is_toplevel_thunk, lambda_locals, return_type)
    LinearIRContext(ctx.graph, SyntaxList(ctx.graph), ctx.next_var_id, Ref(0),
                    is_toplevel_thunk, lambda_locals, return_type, ctx.var_info, ctx.mod)
end

function is_valid_body_ir_argument(ex)
    is_valid_ir_argument(ex) && return true
    return false
    # FIXME
    k = kind(ex)
    return k == K"Identifier" && # Arguments are always defined slots
        TODO("vinfo-table stuff")
end

function is_simple_arg(ex)
    k = kind(ex)
    return is_simple_atom(ex) || k == K"Identifier" || k == K"quote" || k == K"inert" ||
           k == K"top" || k == K"core" || k == K"globalref" || k == K"outerref"
end

function is_single_assign_var(ctx::LinearIRContext, ex)
    return false # FIXME
    id = ex.var_id
    # return id in ctx.lambda_args ||
end

function is_const_read_arg(ctx, ex)
    k = kind(ex)
    return is_simple_atom(ex) ||
           is_single_assign_var(ctx, ex) ||
           k == K"quote" || k == K"inert" || k == K"top" || k == K"core"
end

function is_valid_ir_rvalue(lhs, rhs)
    return kind(lhs) == K"SSAValue"  ||
           is_valid_ir_argument(rhs) ||
           (kind(lhs) == K"Identifier" &&
            # FIXME: add: splatnew isdefined invoke cfunction gc_preserve_begin copyast new_opaque_closure globalref outerref
            kind(rhs) in KSet"new the_exception call foreigncall")
end

# evaluate the arguments of a call, creating temporary locations as needed
function compile_args(ctx, args)
    # First check if all the arguments as simple (and therefore side-effect free).
    # Otherwise, we need to use ssa values for all arguments to ensure proper
    # left-to-right evaluation semantics.
    all_simple = all(is_simple_arg, args)
    args_out = SyntaxList(ctx)
    for arg in args
        arg_val = compile(ctx, arg, true, false)
        if (all_simple || is_const_read_arg(ctx, arg_val)) && is_valid_body_ir_argument(arg_val)
            push!(args_out, arg_val)
        else
            push!(args_out, emit_assign_tmp(ctx, arg_val))
        end
    end
    return args_out
end

function emit(ctx::LinearIRContext, ex)
    push!(ctx.code, ex)
    return ex
end

function emit(ctx::LinearIRContext, srcref, k, args...)
    emit(ctx, makenode(ctx, srcref, k, args...))
end

# Emit computation of ex, assigning the result to an ssavar and returning that
function emit_assign_tmp(ctx::LinearIRContext, ex)
    # TODO: We could replace this with an index into the code array right away?
    tmp = makeleaf(ctx, ex, K"SSAValue", var_id=ctx.next_var_id[])
    ctx.next_var_id[] += 1
    emit(ctx, ex, K"=", tmp, ex)
    return tmp
end

function emit_return(ctx, srcref, ex)
    if isnothing(ex)
        return
    end
    # TODO: return type handling
    # TODO: exception stack handling
    # returning lambda directly is needed for @generated
    if !(is_valid_ir_argument(ex) || head(ex) == K"lambda")
        ex = emit_assign_tmp(ctx, ex)
    end
    # TODO: if !isnothing(ctx.return_type) ...
    emit(ctx, srcref, K"return", ex)
end

function emit_assignment(ctx, srcref, lhs, rhs)
    if !isnothing(rhs)
        if is_valid_ir_rvalue(lhs, rhs)
            emit(ctx, srcref, K"=", lhs, rhs)
        else
            r = emit_assign_tmp(ctx, rhs)
            emit(ctx, srcref, K"=", lhs, r)
        end
    else
        # in unreachable code (such as after return); still emit the assignment
        # so that the structure of those uses is preserved
        emit(ctx, rhs, K"=", lhs, nothing_(ctx, srcref))
        nothing
    end
end

function make_label(ctx, srcref)
    id = ctx.next_label_id[]
    ctx.next_label_id[] += 1
    makeleaf(ctx, srcref, K"label", id)
end

# flisp: make&mark-label
function emit_label(ctx, srcref)
    if !isempty(ctx.code)
        # Use current label if available
        e = ctx.code[end]
        if kind(e) == K"label"
            return e
        end
    end
    l = make_label(ctx, srcref)
    emit(ctx, l)
    l
end

function compile_condition_term(ctx, ex)
    cond = compile(ctx, ex, true, false)
    if !is_valid_body_ir_argument(cond)
        cond = emit_assign_tmp(ctx, cond)
    end
    return cond
end

# flisp: emit-cond
function compile_conditional(ctx, ex, false_label)
    if kind(ex) == K"block"
        for i in 1:numchildren(ex)-1
            compile(ctx, ex[i], false, false)
        end
        test = ex[end]
    else
        test = ex
    end
    k = kind(test)
    if k == K"||"
        true_label = make_label(ctx, test)
        for (i,e) in enumerate(children(test))
            c = compile_condition_term(ctx, e)
            if i < numchildren(test)
                next_term_label = make_label(ctx, test)
                # Jump over short circuit
                emit(ctx, @ast ctx e [K"gotoifnot" c next_term_label])
                # Short circuit to true
                emit(ctx, @ast ctx e [K"goto" true_label])
                emit(ctx, next_term_label)
            else
                emit(ctx, @ast ctx e [K"gotoifnot" c false_label])
            end
        end
        emit(ctx, true_label)
    elseif k == K"&&"
        for e in children(test)
            c = compile_condition_term(ctx, e)
            emit(ctx, @ast ctx e [K"gotoifnot" c false_label])
        end
    else
        c = compile_condition_term(ctx, test)
        emit(ctx, @ast ctx test [K"gotoifnot" c false_label])
    end
end

function new_mutable_var(ctx, srcref, name)
    id = new_var_id(ctx)
    ctx.var_info[id] = VarInfo(name, nothing, :local, false, false)
    push!(ctx.lambda_locals, id)
    makeleaf(ctx, srcref, K"Identifier", name, var_id=id)
end

# This pass behaves like an interpreter on the given code.
# To perform stateful operations, it calls `emit` to record that something
# needs to be done. In value position, it returns an expression computing
# the needed value.
#
# TODO: Is it ok to return `nothing` if we have no value in some sense?
function compile(ctx::LinearIRContext, ex, needs_value, in_tail_pos)
    k = kind(ex)
    if k == K"Identifier" || is_literal(k) || k == K"SSAValue" || k == K"quote" || k == K"inert" ||
            k == K"top" || k == K"core" || k == K"Value" || k == K"Symbol" || k == K"Placeholder" ||
            k == K"Bool"
        # TODO: other kinds: copyast the_exception $ globalref outerref thismodule cdecl stdcall fastcall thiscall llvmcall
        if needs_value && k == K"Placeholder"
            # TODO: ensure outterref, globalref work here
            throw(LoweringError(ex, "all-underscore identifiers are write-only and their values cannot be used in expressions"))
        end
        if in_tail_pos
            emit_return(ctx, ex, ex)
        elseif needs_value
            ex
        else
            if k == K"Identifier"
                emit(ctx, ex) # keep identifiers for undefined-var checking
            end
            nothing
        end
    elseif k == K"call"
        # TODO k ∈ splatnew foreigncall cfunction new_opaque_closure cglobal
        args = compile_args(ctx, children(ex))
        callex = makenode(ctx, ex, k, args)
        if in_tail_pos
            emit_return(ctx, ex, callex)
        elseif needs_value
            callex
        else
            emit(ctx, callex)
            nothing
        end
    elseif k == K"="
        lhs = ex[1]
        if kind(lhs) == K"Placeholder"
            compile(ctx, ex[2], needs_value, in_tail_pos)
        else
            rhs = compile(ctx, ex[2], true, false)
            # TODO look up arg-map for renaming if lhs was reassigned
            if needs_value && !isnothing(rhs)
                r = emit_assign_tmp(ctx, rhs)
                emit(ctx, ex, K"=", lhs, r)
                if in_tail_pos
                    emit_return(ctx, ex, r)
                else
                    r
                end
            else
                emit_assignment(ctx, ex, lhs, rhs)
            end
        end
    elseif k == K"block" || k == K"scope_block"
        nc = numchildren(ex)
        res = nothing
        for i in 1:nc
            islast = i == nc
            res = compile(ctx, ex[i], islast && needs_value, islast && in_tail_pos)
        end
        res
    elseif k == K"return"
        compile(ctx, ex[1], true, true)
        nothing
    elseif k == K"unnecessary"
        # `unnecessary` marks expressions generated by lowering that
        # do not need to be evaluated if their value is unused.
        if needs_value
            compile(ctx, ex[1], needs_value, in_tail_pos)
        else
            nothing
        end
    elseif k == K"if" || k == K"elseif"
        @chk numchildren(ex) <= 3
        has_else = numchildren(ex) > 2
        else_label = make_label(ctx, ex)
        compile_conditional(ctx, ex[1], else_label)
        if in_tail_pos
            compile(ctx, ex[2], needs_value, in_tail_pos)
            emit(ctx, else_label)
            if has_else
                compile(ctx, ex[3], needs_value, in_tail_pos)
            else
                emit_return(ctx, ex, nothing_(ctx, ex))
            end
            nothing
        else
            val = needs_value && new_mutable_var(ctx, ex, "if_val")
            v1 = compile(ctx, ex[2], needs_value, in_tail_pos)
            if needs_value
                emit_assignment(ctx, ex, val, v1)
            end
            if has_else || needs_value
                end_label = make_label(ctx, ex)
                emit(ctx, @ast ctx ex [K"goto" end_label])
            else
                end_label = nothing
            end
            emit(ctx, else_label)
            v2 = if has_else
                compile(ctx, ex[3], needs_value, in_tail_pos)
            elseif needs_value
                nothing_(ctx, ex)
            end
            if needs_value
                emit_assignment(ctx, ex, val, v2)
            end
            if !isnothing(end_label)
                emit(ctx, end_label)
            end
            val
        end
    elseif k == K"method"
        # TODO
        # throw(LoweringError(ex,
        #     "Global method definition needs to be placed at the top level, or use `eval`"))
        if numchildren(ex) == 1
            if in_tail_pos
                emit_return(ctx, ex, ex)
            elseif needs_value
                ex
            else
                emit(ctx, ex)
            end
        else
            @chk numchildren(ex) == 3
            fname = ex[1]
            sig = compile(ctx, ex[2], true, false)
            if !is_valid_ir_argument(sig)
                sig = emit_assign_tmp(ctx, sig)
            end
            lam = ex[3]
            if kind(lam) == K"lambda"
                lam = compile_lambda(ctx, lam)
            else
                # lam = emit_assign_tmp(ctx, compile(ctx, lam, true, false))
                TODO(lam, "non-lambda method argument??")
            end
            emit(ctx, ex, K"method", fname, sig, lam)
            @assert !needs_value && !in_tail_pos
            nothing
        end
    elseif k == K"lambda"
        lam = compile_lambda(ctx, ex)
        if in_tail_pos
            emit_return(ctx, ex, lam)
        elseif needs_value
            lam
        else
            emit(ctx, lam)
        end
    elseif k == K"global"
        if needs_value
            throw(LoweringError(ex, "misplaced `global` declaration"))
        end
        emit(ctx, ex)
        nothing
    elseif k == K"local_def" || k == K"local"
        nothing
    else
        throw(LoweringError(ex, "Invalid syntax; $(repr(k))"))
    end
end


#-------------------------------------------------------------------------------

# Recursively renumber an expression within linear IR
# flisp: renumber-stuff
function _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, ex)
    k = kind(ex)
    if k == K"Identifier"
        id = ex.var_id
        slot_id = get(slot_rewrites, id, nothing)
        if !isnothing(slot_id)
            makeleaf(ctx, ex, K"slot"; var_id=slot_id)
        else
            # TODO: look up any static parameters
            info = ctx.var_info[id]
            if info.kind === :global
                makeleaf(ctx, ex, K"globalref", ex.name_val, mod=info.mod)
            else
                TODO(ex, "Identifier which is not a slot or global?")
            end
        end
    elseif k == K"outerref" || k == K"meta"
        TODO(ex, "_renumber $k")
    elseif is_literal(k) || is_quoted(k) || k == K"global"
        ex
    elseif k == K"SSAValue"
        makeleaf(ctx, ex, K"SSAValue"; var_id=ssa_rewrites[ex.var_id])
    elseif k == K"enter"
        TODO(ex, "_renumber $k")
    elseif k == K"goto"
        @ast ctx ex [K"goto"
            label_table[ex[1].var_id]::K"label"
        ]
    elseif k == K"gotoifnot"
        @ast ctx ex [K"gotoifnot"
            _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, ex[1])
            label_table[ex[2].var_id]::K"label"
        ]
    elseif k == K"lambda"
        ex
    else
        mapchildren(ctx, ex) do e
            _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, e)
        end
        # TODO: foreigncall error check:
        # "ccall function name and library expression cannot reference local variables"
    end
end

# flisp: renumber-lambda, compact-ir
function renumber_body(ctx, input_code, slot_rewrites)
    # Step 1: Remove any assignments to SSA variables, record the indices of labels
    ssa_rewrites = Dict{VarId,VarId}()
    label_table = Dict{Int,Int}()
    code = SyntaxList(ctx)
    for ex in input_code
        k = kind(ex)
        ex_out = nothing
        if k == K"=" && kind(ex[1]) == K"SSAValue"
            lhs_id = ex[1].var_id
            if kind(ex[2]) == K"SSAValue"
                # For SSA₁ = SSA₂, record that all uses of SSA₁ should be replaced by SSA₂
                ssa_rewrites[lhs_id] = ssa_rewrites[ex[2].var_id]
            else
                # Otherwise, record which `code` index this SSA value refers to
                ssa_rewrites[lhs_id] = length(code) + 1
                ex_out = ex[2]
            end
        elseif k == K"label"
            label_table[ex.var_id] = length(code) + 1
        else
            ex_out = ex
        end
        if !isnothing(ex_out)
            push!(code, ex_out)
        end
    end

    # Step 2:
    # * Translate any SSA uses and labels into indices in the code table
    # * Translate locals into slot indices
    for i in 1:length(code)
        code[i] = _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, code[i])
    end
    code
end

# flisp: compile-body
function compile_body(ctx, ex)
    compile(ctx, ex, true, true)
    # TODO: Fix any gotos
    # TODO: Filter out any newvar nodes where the arg is definitely initialized
end

function _add_slots!(slot_rewrites, var_info, var_ids)
    n = length(slot_rewrites) + 1
    for id in var_ids
        info = var_info[id]
        if info.kind == :local || info.kind == :argument
            slot_rewrites[id] = n
            n += 1
        end
    end
    slot_rewrites
end

function compile_lambda(outer_ctx, ex)
    lambda_info = ex.lambda_info
    return_type = nothing # FIXME
    # TODO: Add assignments for reassigned arguments to body using lambda_info.args
    ctx = LinearIRContext(outer_ctx, lambda_info.is_toplevel_thunk, ex.lambda_locals, return_type)
    compile_body(ctx, ex[1])
    slot_rewrites = Dict{VarId,Int}()
    _add_slots!(slot_rewrites, ctx.var_info, (arg.var_id for arg in lambda_info.args))
    _add_slots!(slot_rewrites, ctx.var_info, sort(collect(ex.lambda_locals)))
    code = renumber_body(ctx, ctx.code, slot_rewrites)
    makenode(ctx, ex, K"lambda",
             makenode(ctx, ex[1], K"block", code),
             lambda_info=lambda_info,
             slot_rewrites=slot_rewrites
            )
end

function linearize_ir(ctx, ex)
    graph = ensure_attributes(ctx.graph,
                              slot_rewrites=Dict{VarId,Int},
                              var_info=Dict{VarId,VarInfo},
                              mod=Module)
    # TODO: Cleanup needed - `_ctx` is just a dummy context here. But currently
    # required to call reparent() ...
    _ctx = LinearIRContext(graph, SyntaxList(graph), ctx.next_var_id,
                           Ref(0), false, Set{VarId}(), nothing, ctx.var_info, ctx.mod)
    res = compile_lambda(_ctx, reparent(_ctx, ex))
    setattr!(graph, res.id, var_info=ctx.var_info)
    _ctx, res
end

