#-------------------------------------------------------------------------------
# Lowering pass 5: Flatten to linear IR

function is_valid_ir_argument(ctx, ex)
    k = kind(ex)
    if is_simple_atom(ctx, ex) || k in KSet"inert top core quote static_eval"
        true
    elseif k == K"BindingId"
        binfo = lookup_binding(ctx, ex)
        bk = binfo.kind
        bk === :slot
        # TODO: We should theoretically be able to allow `bk ===
        # :static_parameter` for slightly more compact IR, but it's uncertain
        # what the compiler is built to tolerate.  Notably, flisp allows
        # static_parameter, but doesn't produce this form until a later pass, so
        # it doesn't end up in the IR.
    else
        false
    end
end

function is_ssa(ctx, ex)
    kind(ex) == K"BindingId" && lookup_binding(ctx, ex).is_ssa
end

# Target to jump to, including info on try handler nesting and catch block
# nesting
struct JumpTarget{GraphType}
    label::SyntaxTree{GraphType}
    handler_token_stack::SyntaxList{GraphType, Vector{NodeId}}
    catch_token_stack::SyntaxList{GraphType, Vector{NodeId}}
end

function JumpTarget(label::SyntaxTree{GraphType}, ctx) where {GraphType}
    JumpTarget{GraphType}(label, copy(ctx.handler_token_stack), copy(ctx.catch_token_stack))
end

struct JumpOrigin{GraphType}
    goto::SyntaxTree{GraphType}
    index::Int
    handler_token_stack::SyntaxList{GraphType, Vector{NodeId}}
    catch_token_stack::SyntaxList{GraphType, Vector{NodeId}}
end

function JumpOrigin(goto::SyntaxTree{GraphType}, index, ctx) where {GraphType}
    JumpOrigin{GraphType}(goto, index, copy(ctx.handler_token_stack), copy(ctx.catch_token_stack))
end

struct FinallyHandler{GraphType}
    tagvar::SyntaxTree{GraphType}
    target::JumpTarget{GraphType}
    exit_actions::Vector{Tuple{Symbol,Union{Nothing,SyntaxTree{GraphType}}}}
end

function FinallyHandler(tagvar::SyntaxTree{GraphType}, target::JumpTarget) where {GraphType}
    FinallyHandler{GraphType}(tagvar, target,
        Vector{Tuple{Symbol, Union{Nothing,SyntaxTree{GraphType}}}}())
end


"""
Context for creating linear IR.

One of these is created per lambda expression to flatten the body down to
a sequence of statements (linear IR), which eventually becomes one CodeInfo.
"""
struct LinearIRContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    code::SyntaxList{GraphType, Vector{NodeId}}
    bindings::Bindings
    next_label_id::Ref{Int}
    is_toplevel_thunk::Bool
    lambda_bindings::LambdaBindings
    return_type::Union{Nothing, SyntaxTree{GraphType}}
    break_targets::Dict{String, JumpTarget{GraphType}}
    handler_token_stack::SyntaxList{GraphType, Vector{NodeId}}
    catch_token_stack::SyntaxList{GraphType, Vector{NodeId}}
    finally_handlers::Vector{FinallyHandler{GraphType}}
    symbolic_jump_targets::Dict{String,JumpTarget{GraphType}}
    symbolic_jump_origins::Vector{JumpOrigin{GraphType}}
    mod::Module
end

function LinearIRContext(ctx, is_toplevel_thunk, lambda_bindings, return_type)
    graph = syntax_graph(ctx)
    rett = isnothing(return_type) ? nothing : reparent(graph, return_type)
    GraphType = typeof(graph)
    LinearIRContext(graph, SyntaxList(ctx), ctx.bindings, Ref(0),
                    is_toplevel_thunk, lambda_bindings, rett,
                    Dict{String,JumpTarget{GraphType}}(), SyntaxList(ctx), SyntaxList(ctx),
                    Vector{FinallyHandler{GraphType}}(), Dict{String,JumpTarget{GraphType}}(),
                    Vector{JumpOrigin{GraphType}}(), ctx.mod)
end

function current_lambda_bindings(ctx::LinearIRContext)
    ctx.lambda_bindings
end

function is_valid_body_ir_argument(ctx, ex)
    if is_valid_ir_argument(ctx, ex)
        true
    elseif kind(ex) == K"BindingId"
        binfo = lookup_binding(ctx, ex)
        # Arguments are always defined
        # TODO: use equiv of vinfo:never-undef when we have it
        binfo.kind == :argument
    else
        false
    end
end

function is_simple_arg(ctx, ex)
    k = kind(ex)
    return is_simple_atom(ctx, ex) || k == K"BindingId" || k == K"quote" || k == K"inert" ||
           k == K"top" || k == K"core" || k == K"globalref" || k == K"static_eval"
end

function is_single_assign_var(ctx::LinearIRContext, ex)
    kind(ex) == K"BindingId" || return false
    binfo = lookup_binding(ctx, ex)
    # Arguments are always single-assign
    # TODO: Use equiv of vinfo:sa when we have it
    return binfo.kind == :argument
end

function is_const_read_arg(ctx, ex)
    k = kind(ex)
    # Even if we have side effects, we know that singly-assigned
    # locals cannot be affected by them so we can inline them anyway.
    # TODO from flisp: "We could also allow const globals here"
    return k == K"inert" || k == K"top" || k == K"core" || k == K"static_eval" ||
        is_simple_atom(ctx, ex) || is_single_assign_var(ctx, ex)
end

function is_valid_ir_rvalue(ctx, lhs, rhs)
    return is_ssa(ctx, lhs) ||
           is_valid_ir_argument(ctx, rhs) ||
           (kind(lhs) == K"BindingId" &&
            # FIXME: add: invoke ?
            kind(rhs) in KSet"new splatnew cfunction isdefined call foreigncall gc_preserve_begin foreigncall new_opaque_closure")
end

function check_no_local_bindings(ctx, ex, msg)
    contains_nonglobal_binding = contains_unquoted(ex) do e
        kind(e) == K"BindingId" && lookup_binding(ctx, e).kind !== :global
    end
    if contains_nonglobal_binding
        throw(LoweringError(ex, msg))
    end
end

# evaluate the arguments of a call, creating temporary locations as needed
function compile_args(ctx, args)
    # First check if all the arguments are simple (and therefore side-effect free).
    # Otherwise, we need to use ssa values for all arguments to ensure proper
    # left-to-right evaluation semantics.
    all_simple = all(a->is_simple_arg(ctx, a), args)
    args_out = SyntaxList(ctx)
    for arg in args
        arg_val = compile(ctx, arg, true, false)
        if (all_simple || is_const_read_arg(ctx, arg_val)) && is_valid_body_ir_argument(ctx, arg_val)
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
function emit_assign_tmp(ctx::LinearIRContext, ex, name="tmp")
    tmp = ssavar(ctx, ex, name)
    emit(ctx, @ast ctx ex [K"=" tmp ex])
    return tmp
end

function compile_pop_exception(ctx, srcref, src_tokens, dest_tokens)
    # It's valid to leave the context of src_tokens for the context of
    # dest_tokens when src_tokens is the same or nested within dest_tokens.
    # It's enough to check the token on the top of the dest stack.
    n = length(dest_tokens)
    jump_ok = n == 0 || (n <= length(src_tokens) && dest_tokens[n].var_id == src_tokens[n].var_id)
    jump_ok || throw(LoweringError(srcref, "Attempt to jump into catch block"))
    if n < length(src_tokens)
        @ast ctx srcref [K"pop_exception" src_tokens[n+1]]
    else
        nothing
    end
end

function compile_leave_handler(ctx, srcref, src_tokens, dest_tokens)
    n = length(dest_tokens)
    jump_ok = n == 0 || (n <= length(src_tokens) && dest_tokens[n].var_id == src_tokens[n].var_id)
    jump_ok || throw(LoweringError(srcref, "Attempt to jump into try block"))
    if n < length(src_tokens)
        @ast ctx srcref [K"leave" src_tokens[n+1:end]...]
    else
        nothing
    end
end

function emit_pop_exception(ctx::LinearIRContext, srcref, dest_tokens)
    pexc = compile_pop_exception(ctx, srcref, ctx.catch_token_stack, dest_tokens)
    if !isnothing(pexc)
        emit(ctx, pexc)
    end
end

function emit_leave_handler(ctx::LinearIRContext, srcref, dest_tokens)
    ex = compile_leave_handler(ctx, srcref, ctx.handler_token_stack, dest_tokens)
    if !isnothing(ex)
        emit(ctx, ex)
    end
end

function emit_jump(ctx, srcref, target::JumpTarget)
    emit_pop_exception(ctx, srcref, target.catch_token_stack)
    emit_leave_handler(ctx, srcref, target.handler_token_stack)
    emit(ctx, @ast ctx srcref [K"goto" target.label])
end

# Enter the current finally block, either through the landing pad (on_exit ==
# :rethrow) or via a jump (on_exit ∈ (:return, :break)).
#
# An integer tag is created to identify the current code path and select the
# on_exit action to be taken at finally handler exit.
function enter_finally_block(ctx, srcref, on_exit, value)
    @assert on_exit ∈ (:rethrow, :break, :return)
    handler = last(ctx.finally_handlers)
    push!(handler.exit_actions, (on_exit, value))
    tag = length(handler.exit_actions)
    emit(ctx, @ast ctx srcref [K"=" handler.tagvar tag::K"Integer"])
    if on_exit != :rethrow
        emit_jump(ctx, srcref, handler.target)
    end
end

# Helper function for emit_return
function _actually_return(ctx, ex)
    # TODO: Handle the implicit return coverage hack for #53354 ?
    rett = ctx.return_type
    if !isnothing(rett)
        ex = compile(ctx, convert_for_type_decl(ctx, rett, ex, rett, true), true, false)
    end
    simple_ret_val = isempty(ctx.catch_token_stack) ?
        # returning lambda directly is needed for @generated
        (is_valid_ir_argument(ctx, ex) || kind(ex) == K"lambda") :
        is_simple_atom(ctx, ex)
    if !simple_ret_val
        ex = emit_assign_tmp(ctx, ex, "return_tmp")
    end
    emit_pop_exception(ctx, ex, ())
    emit(ctx, @ast ctx ex [K"return" ex])
    return nothing
end

function emit_return(ctx, srcref, ex)
    # todo: Mark implicit returns
    if isnothing(ex)
        return
    elseif isempty(ctx.handler_token_stack)
        _actually_return(ctx, ex)
        return
    end
    # TODO: What's this !is_ssa(ctx, ex) here about?
    x = if is_simple_atom(ctx, ex) && !(is_ssa(ctx, ex) && !isempty(ctx.finally_handlers))
        ex
    elseif !isempty(ctx.finally_handlers)
        # todo: Why does flisp lowering create a mutable variable here even
        # though we don't mutate it?
        # tmp = ssavar(ctx, srcref, "returnval_via_finally") # <- can we use this?
        tmp = new_local_binding(ctx, srcref, "returnval_via_finally")
        emit(ctx, @ast ctx srcref [K"=" tmp ex])
        tmp
    else
        emit_assign_tmp(ctx, ex, "returnval_via_finally")
    end
    if !isempty(ctx.finally_handlers)
        enter_finally_block(ctx, srcref, :return, x)
    else
        emit(ctx, @ast ctx srcref [K"leave" ctx.handler_token_stack...])
        _actually_return(ctx, x)
    end
    return nothing
end

function emit_return(ctx, ex)
    emit_return(ctx, ex, ex)
end

function emit_break(ctx, ex)
    name = ex[1].name_val
    target = get(ctx.break_targets, name, nothing)
    if isnothing(target)
        ty = name == "loop_exit" ? "break" : "continue"
        throw(LoweringError(ex, "$ty must be used inside a `while` or `for` loop"))
    end
    if !isempty(ctx.finally_handlers)
        handler = last(ctx.finally_handlers)
        if length(target.handler_token_stack) < length(handler.target.handler_token_stack)
            enter_finally_block(ctx, ex, :break, ex)
            return
        end
    end
    emit_jump(ctx, ex, target)
end

# `op` may be either K"=" (where global assignments are converted to setglobal!)
# or K"constdecl".  flisp: emit-assignment-or-setglobal
function emit_simple_assignment(ctx, srcref, lhs, rhs, op=K"=")
    binfo = lookup_binding(ctx, lhs.var_id)
    if binfo.kind == :global && op == K"="
        emit(ctx, @ast ctx srcref [
            K"call"
            "setglobal!"::K"core"
            binfo.mod::K"Value"
            binfo.name::K"Symbol"
            rhs
        ])
    else
        emit(ctx, srcref, op, lhs, rhs)
    end
end

function emit_assignment(ctx, srcref, lhs, rhs, op=K"=")
    if !isnothing(rhs)
        if is_valid_ir_rvalue(ctx, lhs, rhs)
            emit_simple_assignment(ctx, srcref, lhs, rhs, op)
        else
            r = emit_assign_tmp(ctx, rhs)
            emit_simple_assignment(ctx, srcref, lhs, r, op)
        end
    else
        # in unreachable code (such as after return); still emit the assignment
        # so that the structure of those uses is preserved
        emit_simple_assignment(ctx, srcref, lhs, nothing_(ctx, srcref), op)
        nothing
    end
end

function make_label(ctx, srcref)
    id = ctx.next_label_id[]
    ctx.next_label_id[] += 1
    makeleaf(ctx, srcref, K"label", id=id)
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

function emit_latestworld(ctx, srcref)
    (isempty(ctx.code) || kind(last(ctx.code)) != K"latestworld") &&
        emit(ctx, makeleaf(ctx, srcref, K"latestworld"))
end

function compile_condition_term(ctx, ex)
    cond = compile(ctx, ex, true, false)
    if !is_valid_body_ir_argument(ctx, cond)
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

# Lowering of exception handling must ensure that
#
# * Each `enter` is matched with a `leave` on every possible non-exceptional
#   program path (including implicit returns generated in tail position).
# * Each catch block which is entered and handles the exception - by exiting
#   via a non-exceptional program path - leaves the block with `pop_exception`.
# * Each `finally` block runs, regardless of any early `return` or jumps
#   via `break`/`continue`/`goto` etc.
#
# These invariants are upheld by tracking the nesting using
# `handler_token_stack` and `catch_token_stack` and using these when emitting
# any control flow (return / goto) which leaves the associated block.
#
# The following special forms are emitted into the IR:
#
#   (= tok (enter catch_label dynscope))
#     push exception handler with catch block at `catch_label` and dynamic
#     scope `dynscope`, yielding a token which is used by `leave` and
#     `pop_exception`. `dynscope` is only used in the special `tryfinally` form
#     without associated source level syntax (see the `@with` macro)
#
#   (leave tok)
#     pop exception handler back to the state of the `tok` from the associated
#     `enter`. Multiple tokens can be supplied to pop multiple handlers using
#     `(leave tok1 tok2 ...)`.
#
#   (pop_exception tok) - pop exception stack back to state of associated enter
#
# See the devdocs for further discussion.
function compile_try(ctx::LinearIRContext, ex, needs_value, in_tail_pos)
    @chk numchildren(ex) <= 3
    try_block = ex[1]
    if kind(ex) == K"trycatchelse"
        catch_block = ex[2]
        else_block = numchildren(ex) == 2 ? nothing : ex[3]
        finally_block = nothing
        catch_label = make_label(ctx, catch_block)
    else
        catch_block = nothing
        else_block = nothing
        finally_block = ex[2]
        catch_label = make_label(ctx, finally_block)
    end

    end_label = !in_tail_pos || !isnothing(finally_block) ? make_label(ctx, ex) : nothing
    try_result = needs_value && !in_tail_pos ? new_local_binding(ctx, ex, "try_result") : nothing

    # Exception handler block prefix
    handler_token = ssavar(ctx, ex, "handler_token")
    emit(ctx, @ast ctx ex [K"="
        handler_token
        [K"enter" catch_label]  # TODO: dynscope
    ])
    if !isnothing(finally_block)
        # TODO: Trivial finally block optimization from JuliaLang/julia#52593 (or
        # support a special form for @with)?
        finally_handler = FinallyHandler(new_local_binding(ctx, finally_block, "finally_tag"),
                                         JumpTarget(end_label, ctx))
        push!(ctx.finally_handlers, finally_handler)
        emit(ctx, @ast ctx finally_block [K"=" finally_handler.tagvar (-1)::K"Integer"])
    end
    push!(ctx.handler_token_stack, handler_token)

    # Try block code.
    try_val = compile(ctx, try_block, needs_value, false)
    # Exception handler block postfix
    if isnothing(else_block)
        if in_tail_pos
            if !isnothing(try_val)
                emit_return(ctx, try_val)
            end
        else
            if needs_value && !isnothing(try_val)
                emit_assignment(ctx, ex, try_result, try_val)
            end
            emit(ctx, @ast ctx ex [K"leave" handler_token])
        end
        pop!(ctx.handler_token_stack)
    else
        if !isnothing(try_val) && (in_tail_pos || needs_value)
            emit(ctx, try_val) # TODO: Only for any side effects ?
        end
        emit(ctx, @ast ctx ex [K"leave" handler_token])
        pop!(ctx.handler_token_stack)
        # Else block code
        else_val = compile(ctx, else_block, needs_value, in_tail_pos)
        if !in_tail_pos
            if needs_value && !isnothing(else_val)
                emit_assignment(ctx, ex, try_result, else_val)
            end
        end
    end
    if !in_tail_pos
        emit(ctx, @ast ctx ex [K"goto" end_label])
    end

    # Catch pad
    # Emit either catch or finally block. A combined try/catch/finally block
    # was split into separate trycatchelse and tryfinally blocks earlier.
    emit(ctx, catch_label) # <- Exceptional control flow enters here
    if !isnothing(finally_block)
        # Attribute the postfix and prefix to the finally block as a whole.
        srcref = finally_block
        enter_finally_block(ctx, srcref, :rethrow, nothing)
        emit(ctx, end_label) # <- Non-exceptional control flow enters here
        pop!(ctx.finally_handlers)
        compile(ctx, finally_block, false, false)
        # Finally block postfix: Emit a branch for every code path which enters
        # the block to dynamically decide which return/break/rethrow exit action to take
        for (tag, (on_exit, value)) in Iterators.reverse(enumerate(finally_handler.exit_actions))
            next_action_label = !in_tail_pos || tag != 1 || on_exit != :return ?
                make_label(ctx, srcref) : nothing
            if !isnothing(next_action_label)
                next_action_label = make_label(ctx, srcref)
                tmp = ssavar(ctx, srcref, "do_finally_action")
                emit(ctx, @ast ctx srcref [K"=" tmp
                    [K"call"
                        "==="::K"core"
                        finally_handler.tagvar
                        tag::K"Integer"
                    ]
                ])
                emit(ctx, @ast ctx srcref [K"gotoifnot" tmp next_action_label])
            end
            if on_exit === :return
                emit_return(ctx, value)
            elseif on_exit === :break
                emit_break(ctx, value)
            elseif on_exit === :rethrow
                emit(ctx, @ast ctx srcref [K"call" "rethrow"::K"top"])
            else
                @assert false
            end
            if !isnothing(next_action_label)
                emit(ctx, next_action_label)
            end
        end
    else
        push!(ctx.catch_token_stack, handler_token)
        catch_val = compile(ctx, catch_block, needs_value, in_tail_pos)
        if !isnothing(try_result) && !isnothing(catch_val)
            emit_assignment(ctx, ex, try_result, catch_val)
        end
        if !in_tail_pos
            emit(ctx, @ast ctx ex [K"pop_exception" handler_token])
            emit(ctx, end_label)
        else
            # (pop_exception done in emit_return)
        end
        pop!(ctx.catch_token_stack)
    end
    try_result
end

# This pass behaves like an interpreter on the given code.
# To perform stateful operations, it calls `emit` to record that something
# needs to be done. In value position, it returns an expression computing
# the needed value.
function compile(ctx::LinearIRContext, ex, needs_value, in_tail_pos)
    k = kind(ex)
    if k == K"BindingId" || is_literal(k) || k == K"quote" || k == K"inert" ||
            k == K"top" || k == K"core" || k == K"Value" || k == K"Symbol" ||
            k == K"SourceLocation" || k == K"static_eval"
        if in_tail_pos
            emit_return(ctx, ex)
        elseif needs_value
            ex
        else
            if k == K"BindingId" && !is_ssa(ctx, ex)
                emit(ctx, ex) # keep identifiers for undefined-var checking
            end
            nothing
        end
    elseif k == K"Placeholder"
        if needs_value
            throw(LoweringError(ex, "all-underscore identifiers are write-only and their values cannot be used in expressions"))
        end
        nothing
    elseif k == K"TOMBSTONE"
        @chk !needs_value (ex,"TOMBSTONE encountered in value position")
        nothing
    elseif k == K"call" || k == K"new" || k == K"splatnew" || k == K"foreigncall" ||
            k == K"new_opaque_closure" || k == K"cfunction"
        callex = makenode(ctx, ex, k, compile_args(ctx, children(ex)))
        if in_tail_pos
            emit_return(ctx, ex, callex)
        elseif needs_value
            callex
        else
            emit(ctx, callex)
            nothing
        end
    elseif k == K"=" || k == K"constdecl"
        lhs = ex[1]
        res = if kind(lhs) == K"Placeholder"
            compile(ctx, ex[2], needs_value, in_tail_pos)
        else
            rhs = compile(ctx, ex[2], true, false)
            # TODO look up arg-map for renaming if lhs was reassigned
            if needs_value && !isnothing(rhs)
                r = emit_assign_tmp(ctx, rhs)
                emit_simple_assignment(ctx, ex, lhs, r, k)
                if in_tail_pos
                    emit_return(ctx, ex, r)
                else
                    r
                end
            else
                emit_assignment(ctx, ex, lhs, rhs, k)
            end
        end
        k == K"constdecl" && emit_latestworld(ctx, ex)
        res
    elseif k == K"block" || k == K"scope_block"
        nc = numchildren(ex)
        if nc == 0
            if in_tail_pos
                emit_return(ctx, nothing_(ctx, ex))
            elseif needs_value
                nothing_(ctx, ex)
            else
                nothing
            end
        else
            res = nothing
            for i in 1:nc
                islast = i == nc
                res = compile(ctx, ex[i], islast && needs_value, islast && in_tail_pos)
            end
            res
        end
    elseif k == K"break_block"
        end_label = make_label(ctx, ex)
        name = ex[1].name_val
        outer_target = get(ctx.break_targets, name, nothing)
        ctx.break_targets[name] = JumpTarget(end_label, ctx)
        compile(ctx, ex[2], false, false)
        if isnothing(outer_target)
            delete!(ctx.break_targets, name)
        else
            ctx.break_targets[name] = outer_target
        end
        emit(ctx, end_label)
        if needs_value
            compile(ctx, nothing_(ctx, ex), needs_value, in_tail_pos)
        end
    elseif k == K"break"
        emit_break(ctx, ex)
    elseif k == K"symbolic_label"
        label = emit_label(ctx, ex)
        name = ex.name_val
        if haskey(ctx.symbolic_jump_targets, name)
            throw(LoweringError(ex, "Label `$name` defined multiple times"))
        end
        push!(ctx.symbolic_jump_targets, name=>JumpTarget(label, ctx))
        if in_tail_pos
            emit_return(ctx, ex, nothing_(ctx, ex))
        elseif needs_value
            throw(LoweringError(ex, "misplaced label in value position"))
        end
    elseif k == K"symbolic_goto"
        push!(ctx.symbolic_jump_origins, JumpOrigin(ex, length(ctx.code)+1, ctx))
        emit(ctx, makeleaf(ctx, ex, K"TOMBSTONE")) # ? pop_exception
        emit(ctx, makeleaf(ctx, ex, K"TOMBSTONE")) # ? leave
        emit(ctx, makeleaf(ctx, ex, K"TOMBSTONE")) # ? goto
        nothing
    elseif k == K"return"
        compile(ctx, ex[1], true, true)
        nothing
    elseif k == K"removable"
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
            val = needs_value && new_local_binding(ctx, ex, "if_val")
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
    elseif k == K"trycatchelse" || k == K"tryfinally"
        compile_try(ctx, ex, needs_value, in_tail_pos)
    elseif k == K"method"
        # TODO
        # throw(LoweringError(ex,
        #     "Global method definition needs to be placed at the top level, or use `eval`"))
        res = if numchildren(ex) == 1
            if in_tail_pos
                emit_return(ctx, ex)
            elseif needs_value
                ex
            else
                emit(ctx, ex)
            end
        else
            @chk numchildren(ex) == 3
            fname = ex[1]
            sig = compile(ctx, ex[2], true, false)
            if !is_valid_ir_argument(ctx, sig)
                sig = emit_assign_tmp(ctx, sig)
            end
            lam = ex[3]
            if kind(lam) == K"lambda"
                lam = compile_lambda(ctx, lam)
            else
                lam = emit_assign_tmp(ctx, compile(ctx, lam, true, false))
            end
            emit(ctx, ex, K"method", fname, sig, lam)
            @assert !needs_value && !in_tail_pos
            nothing
        end
        emit_latestworld(ctx, ex)
        res
    elseif k == K"opaque_closure_method"
        @ast ctx ex [K"opaque_closure_method"
            ex[1]
            ex[2]
            ex[3]
            ex[4]
            compile_lambda(ctx, ex[5])
        ]
    elseif k == K"lambda"
        lam = compile_lambda(ctx, ex)
        if in_tail_pos
            emit_return(ctx, lam)
        elseif needs_value
            lam
        else
            emit(ctx, lam)
        end
    elseif k == K"gc_preserve_begin"
        makenode(ctx, ex, k, compile_args(ctx, children(ex)))
    elseif k == K"gc_preserve_end"
        if needs_value
            throw(LoweringError(ex, "misplaced kind $k in value position"))
        end
        emit(ctx, ex)
        nothing
    elseif k == K"global"
        if needs_value
            throw(LoweringError(ex, "misplaced global declaration in value position"))
        end
        emit(ctx, ex)
        ctx.is_toplevel_thunk && emit_latestworld(ctx, ex)
        nothing
    elseif k == K"meta"
        emit(ctx, ex)
        if needs_value
            val = @ast ctx ex "nothing"::K"core"
            if in_tail_pos
                emit_return(ctx, val)
            else
                val
            end
        end
    elseif k == K"_while"
        end_label = make_label(ctx, ex)
        top_label = emit_label(ctx, ex)
        compile_conditional(ctx, ex[1], end_label)
        compile(ctx, ex[2], false, false)
        emit(ctx, @ast ctx ex [K"goto" top_label])
        emit(ctx, end_label)
        if needs_value
            compile(ctx, nothing_(ctx, ex), needs_value, in_tail_pos)
        end
    elseif k == K"_do_while"
        end_label = make_label(ctx, ex)
        top_label = emit_label(ctx, ex)
        compile(ctx, ex[1], false, false)
        compile_conditional(ctx, ex[2], end_label)
        emit(ctx, @ast ctx ex [K"goto" top_label])
        emit(ctx, end_label)
        if needs_value
            compile(ctx, nothing_(ctx, ex), needs_value, in_tail_pos)
        end
    elseif k == K"isdefined" || k == K"captured_local" || k == K"throw_undef_if_not" ||
            k == K"boundscheck"
        if in_tail_pos
            emit_return(ctx, ex)
        elseif needs_value
            ex
        end
    elseif k == K"newvar"
        @assert !needs_value
        is_duplicate = !isempty(ctx.code) &&
            (e = last(ctx.code); kind(e) == K"newvar" && e[1].var_id == ex[1].var_id)
        if !is_duplicate
            # TODO: also exclude deleted vars
            emit(ctx, ex)
        end
    elseif k == K"globaldecl"
        if needs_value
            throw(LoweringError(ex, "misplaced global declaration"))
        end
        if numchildren(ex) == 1 || is_identifier_like(ex[2])
            emit(ctx, ex)
        else
            rr = emit_assign_tmp(ctx, ex[2])
            emit(ctx, @ast ctx ex [K"globaldecl" ex[1] rr])
        end
        ctx.is_toplevel_thunk && emit_latestworld(ctx, ex)
    elseif k == K"latestworld"
        if needs_value
            throw(LoweringError(ex, "misplaced latestsworld"))
        end
        emit_latestworld(ctx, ex)
    elseif k == K"latestworld_if_toplevel"
        ctx.is_toplevel_thunk && emit_latestworld(ctx, ex)
    else
        throw(LoweringError(ex, "Invalid syntax; $(repr(k))"))
    end
end

function _remove_vars_with_isdefined_check!(vars, ex)
    if is_leaf(ex) || is_quoted(ex) || kind(ex) == K"static_eval"
        return
    elseif kind(ex) == K"isdefined"
        delete!(vars, ex[1].var_id)
    else
        for e in children(ex)
            _remove_vars_with_isdefined_check!(vars, e)
        end
    end
end

# Find newvar nodes that are unnecessary because
# 1. The variable is not captured and 
# 2. The variable is assigned before any branches.
#
# This is used to remove newvar nodes that are not needed for re-initializing
# variables to undefined (see Julia issue #11065). It doesn't look for variable
# *uses*, because any variables used-before-def that also pass this test are
# *always* used undefined, and therefore don't need to be reinitialized. The
# one exception to that is `@isdefined`, which can observe an undefined
# variable without throwing an error.
function unnecessary_newvar_ids(ctx, stmts)
    vars = Set{IdTag}()
    ids_assigned_before_branch = Set{IdTag}()
    for ex in stmts
        _remove_vars_with_isdefined_check!(vars, ex)
        k = kind(ex)
        if k == K"newvar"
            id = ex[1].var_id
            if !lookup_binding(ctx, id).is_captured
                push!(vars, id)
            end
        elseif k == K"goto" || k == K"gotoifnot" || (k == K"=" && kind(ex[2]) == K"enter")
            empty!(vars)
        elseif k == K"="
            id = ex[1].var_id
            if id in vars
                delete!(vars, id)
                push!(ids_assigned_before_branch, id)
            end
        end
    end
    ids_assigned_before_branch
end

# flisp: compile-body
function compile_body(ctx, ex)
    compile(ctx, ex, true, true)

    # Fix up any symbolic gotos. (We can't do this earlier because the goto
    # might precede the label definition in unstructured control flow.)
    for origin in ctx.symbolic_jump_origins
        name = origin.goto.name_val
        target = get(ctx.symbolic_jump_targets, name, nothing)
        if isnothing(target)
            throw(LoweringError(origin.goto, "label `$name` referenced but not defined"))
        end
        i = origin.index
        pop_ex = compile_pop_exception(ctx, origin.goto, origin.catch_token_stack,
                                     target.catch_token_stack)
        if !isnothing(pop_ex)
            @assert kind(ctx.code[i]) == K"TOMBSTONE"
            ctx.code[i] = pop_ex
            i += 1
        end
        leave_ex = compile_leave_handler(ctx, origin.goto, origin.handler_token_stack,
                                         target.handler_token_stack)
        if !isnothing(leave_ex)
            @assert kind(ctx.code[i]) == K"TOMBSTONE"
            ctx.code[i] = leave_ex
            i += 1
        end
        @assert kind(ctx.code[i]) == K"TOMBSTONE"
        ctx.code[i] = @ast ctx origin.goto [K"goto" target.label]
    end

    # Filter out unnecessary newvar nodes
    ids_assigned_before_branch = unnecessary_newvar_ids(ctx, ctx.code)
    filter!(ctx.code) do ex
        !(kind(ex) == K"newvar" && ex[1].var_id in ids_assigned_before_branch)
    end
end

#-------------------------------------------------------------------------------

# Recursively renumber an expression within linear IR
# flisp: renumber-stuff
function _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, ex)
    k = kind(ex)
    if k == K"BindingId"
        id = ex.var_id
        if haskey(ssa_rewrites, id)
            makeleaf(ctx, ex, K"SSAValue"; var_id=ssa_rewrites[id])
        else
            new_id = get(slot_rewrites, id, nothing)
            binfo = lookup_binding(ctx, id)
            if !isnothing(new_id)
                sk = binfo.kind == :local || binfo.kind == :argument ? K"slot"             :
                     binfo.kind == :static_parameter                 ? K"static_parameter" :
                     throw(LoweringError(ex, "Found unexpected binding of kind $(binfo.kind)"))
                makeleaf(ctx, ex, sk; var_id=new_id)
            else
                if binfo.kind !== :global
                    throw(LoweringError(ex, "Found unexpected binding of kind $(binfo.kind)"))
                end
                makeleaf(ctx, ex, K"globalref", binfo.name, mod=binfo.mod)
            end
        end
    elseif k == K"meta" || k == K"static_eval"
        # Somewhat-hack for Expr(:meta, :generated, gen) which has
        # weird top-level semantics for `gen`, but we still need to translate
        # the binding it contains to a globalref. (TODO: use
        # static_eval for this meta, somehow)
        mapchildren(ctx, ex) do e
            _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, e)
        end
    elseif is_literal(k) || is_quoted(k)
        ex
    elseif k == K"label"
        @ast ctx ex label_table[ex.id]::K"label"
    elseif k == K"code_info"
        ex
    else
        mapchildren(ctx, ex) do e
            _renumber(ctx, ssa_rewrites, slot_rewrites, label_table, e)
        end
    end
end

# flisp: renumber-lambda, compact-ir
function renumber_body(ctx, input_code, slot_rewrites)
    # Step 1: Remove any assignments to SSA variables, record the indices of labels
    ssa_rewrites = Dict{IdTag,IdTag}()
    label_table = Dict{Int,Int}()
    code = SyntaxList(ctx)
    for ex in input_code
        k = kind(ex)
        ex_out = nothing
        if k == K"=" && is_ssa(ctx, ex[1])
            lhs_id = ex[1].var_id
            if is_ssa(ctx, ex[2])
                # For SSA₁ = SSA₂, record that all uses of SSA₁ should be replaced by SSA₂
                ssa_rewrites[lhs_id] = ssa_rewrites[ex[2].var_id]
            else
                # Otherwise, record which `code` index this SSA value refers to
                ssa_rewrites[lhs_id] = length(code) + 1
                ex_out = ex[2]
            end
        elseif k == K"label"
            label_table[ex.id] = length(code) + 1
        elseif k == K"TOMBSTONE"
            # remove statement
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

struct Slot
    name::String
    kind::Symbol
    is_nospecialize::Bool
    is_read::Bool
    is_single_assign::Bool
    is_maybe_undef::Bool
    is_called::Bool
end

function compile_lambda(outer_ctx, ex)
    lambda_args = ex[1]
    static_parameters = ex[2]
    ret_var = numchildren(ex) == 4 ? ex[4] : nothing
    # TODO: Add assignments for reassigned arguments to body
    lambda_bindings = ex.lambda_bindings
    ctx = LinearIRContext(outer_ctx, ex.is_toplevel_thunk, lambda_bindings, ret_var)
    compile_body(ctx, ex[3])
    slots = Vector{Slot}()
    slot_rewrites = Dict{IdTag,Int}()
    for arg in children(lambda_args)
        if kind(arg) == K"Placeholder"
            # Unused functions arguments like: `_` or `::T`
            push!(slots, Slot(arg.name_val, :argument, false, false, false, false, false))
        else
            @assert kind(arg) == K"BindingId"
            id = arg.var_id
            binfo = lookup_binding(ctx, id)
            lbinfo = lookup_lambda_binding(ctx, id)
            @assert binfo.kind == :local || binfo.kind == :argument
            # FIXME: is_single_assign, is_maybe_undef
            push!(slots, Slot(binfo.name, :argument, binfo.is_nospecialize,
                              lbinfo.is_read, false, false, lbinfo.is_called))
            slot_rewrites[id] = length(slots)
        end
    end
    # Sorting the lambda locals is required to remove dependence on Dict iteration order.
    for (id, lbinfo) in sort(collect(pairs(lambda_bindings.bindings)), by=first)
        if !lbinfo.is_captured
            binfo = lookup_binding(ctx.bindings, id)
            if binfo.kind == :local
                # FIXME: is_single_assign, is_maybe_undef
                push!(slots, Slot(binfo.name, :local, false,
                                  lbinfo.is_read, false, false, lbinfo.is_called))
                slot_rewrites[id] = length(slots)
            end
        end
    end
    for (i,arg) in enumerate(children(static_parameters))
        @assert kind(arg) == K"BindingId"
        id = arg.var_id
        info = lookup_binding(ctx.bindings, id)
        @assert info.kind == :static_parameter
        slot_rewrites[id] = i
    end
    # @info "" @ast ctx ex [K"block" ctx.code...]
    code = renumber_body(ctx, ctx.code, slot_rewrites)
    @ast ctx ex [K"code_info"(is_toplevel_thunk=ex.is_toplevel_thunk,
                              slots=slots)
        [K"block"(ex[3])
            code...
        ]
    ]
end

"""
This pass converts nested ASTs in the body of a lambda into a list of
statements (ie, Julia's linear/untyped IR).

Most of the compliexty of this pass is in lowering structured control flow (if,
loops, etc) to gotos and exception handling to enter/leave. We also convert
`K"BindingId"` into K"slot", `K"globalref"` or `K"SSAValue` as appropriate.
"""
@fzone "JL: linearize" function linearize_ir(ctx, ex)
    graph = ensure_attributes(ctx.graph,
                              slots=Vector{Slot},
                              mod=Module,
                              id=Int)
    # TODO: Cleanup needed - `_ctx` is just a dummy context here. But currently
    # required to call reparent() ...
    GraphType = typeof(graph)
    _ctx = LinearIRContext(graph, SyntaxList(graph), ctx.bindings,
                           Ref(0), false, LambdaBindings(), nothing,
                           Dict{String,JumpTarget{typeof(graph)}}(),
                           SyntaxList(graph), SyntaxList(graph),
                           Vector{FinallyHandler{GraphType}}(),
                           Dict{String, JumpTarget{GraphType}}(),
                           Vector{JumpOrigin{GraphType}}(), ctx.mod)
    res = compile_lambda(_ctx, reparent(_ctx, ex))
    _ctx, res
end
