#-------------------------------------------------------------------------------
# Lowering pass 4: Flatten to linear IR

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
    return_type::Union{Nothing,NodeId}
    var_info::Dict{VarId,VarInfo}
    mod::Module
end

function LinearIRContext(ctx, return_type)
    LinearIRContext(ctx.graph, SyntaxList(ctx.graph), ctx.next_var_id,
                    return_type, ctx.var_info, ctx.mod)
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
    tmp = makenode(ctx, ex, K"SSAValue", var_id=ctx.next_var_id[])
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

# This pass behaves like an interpreter on the given code.
# To perform stateful operations, it calls `emit` to record that something
# needs to be done. In value position, it returns an expression computing
# the needed value.
#
# TODO: is it ok to return `nothing` if we have no value in some sense
function compile(ctx::LinearIRContext, ex, needs_value, in_tail_pos)
    k = kind(ex)
    if k == K"Identifier" || is_literal(k) || k == K"SSAValue" || k == K"quote" || k == K"inert" ||
            k == K"top" || k == K"core" || k == K"Value"
        # TODO: other kinds: copyast the_exception $ globalref outerref thismodule cdecl stdcall fastcall thiscall llvmcall
        if in_tail_pos
            emit_return(ctx, ex, ex)
        elseif needs_value
            if is_placeholder(ex)
                # TODO: ensure outterref, globalref work here
                throw(LoweringError(ex, "all-underscore identifiers are write-only and their values cannot be used in expressions"))
            end
            ex
        else
            if k == K"Identifier"
                emit(ctx, ex) # keep symbols for undefined-var checking
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
        # TODO: Handle underscore
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
    elseif k == K"block"
        nc = numchildren(ex)
        for i in 1:nc
            islast = i == nc
            compile(ctx, ex[i], islast && needs_value, islast && in_tail_pos)
        end
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
            makenode(ctx, ex, K"slot"; var_id=slot_id)
        else
            # TODO: look up any static parameters
            ex
        end
    elseif k == K"outerref" || k == K"meta"
        TODO(ex, "_renumber $k")
    elseif is_literal(k) || is_quoted(k) || k == K"global"
        ex
    elseif k == K"SSAValue"
        makenode(ctx, ex, K"SSAValue"; var_id=ssa_rewrites[ex.var_id])
    elseif k == K"goto" || k == K"enter" || k == K"gotoifnot"
        TODO(ex, "_renumber $k")
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
    label_table = Dict{String,Int}()
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
            label_table[ex.name_val] = length(code) + 1
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
    ctx = LinearIRContext(outer_ctx, return_type)
    compile_body(ctx, ex[1])
    slot_rewrites = Dict{VarId,Int}()
    _add_slots!(slot_rewrites, ctx.var_info, (arg.var_id for arg in lambda_info.args))
    _add_slots!(slot_rewrites, ctx.var_info, ex.lambda_locals)
    code = renumber_body(ctx, ctx.code, slot_rewrites)
    makenode(ctx, ex, K"lambda",
             makenode(ctx, ex[1], K"block", code),
             lambda_info=lambda_info,
             slot_rewrites=slot_rewrites
            )
end

function linearize_ir(ctx, ex)
    graph = ensure_attributes(ctx.graph, slot_rewrites=Dict{VarId,Int})
    _ctx = LinearIRContext(graph, SyntaxList(graph), ctx.next_var_id,
                           nothing, ctx.var_info, ctx.mod)
    res = compile_lambda(_ctx, reparent(_ctx, ex))
    _ctx, res
end

#-------------------------------------------------------------------------------
# Conversion to Expr + CodeInfo

# Convert our data structures to CodeInfo
function to_code_info(ex, mod, funcname, var_info, slot_rewrites)
    input_code = children(ex)
    # Convert code to Expr and record low res locations in table
    num_stmts = length(input_code)
    code = Vector{Any}(undef, num_stmts)
    codelocs = Vector{Int32}(undef, num_stmts)
    linetable_map = Dict{Tuple{Int,String}, Int32}()
    linetable = Any[]
    for i in 1:length(code)
        code[i] = to_expr(mod, var_info, input_code[i])
        fname = filename(input_code[i])
        lineno, _ = source_location(input_code[i])
        loc = (lineno, fname)
        codelocs[i] = get!(linetable_map, loc) do
            inlined_at = 0 # FIXME: nonzero for expanded macros
            full_loc = Core.LineInfoNode(mod, Symbol(funcname), Symbol(fname),
                                         Int32(lineno), Int32(inlined_at))
            push!(linetable, full_loc)
            length(linetable)
        end
    end

    # FIXME
    ssaflags = zeros(UInt32, length(code))

    nslots = length(slot_rewrites)
    slotnames = Vector{Symbol}(undef, nslots)
    slot_rename_inds = Dict{String,Int}()
    slotflags = Vector{UInt8}(undef, nslots)
    for (id,i) in slot_rewrites
        info = var_info[id]
        name = info.name
        ni = get(slot_rename_inds, name, 0)
        slot_rename_inds[name] = ni + 1
        if ni > 0
            name = "$name@$ni"
        end
        slotnames[i] = Symbol(name)
        slotflags[i] = 0x00  # FIXME!!
    end

    _CodeInfo(
        code,
        codelocs,
        num_stmts,         # ssavaluetypes (why put num_stmts in here??)
        ssaflags,
        nothing,           #  method_for_inference_limit_heuristics
        linetable,
        slotnames,
        slotflags,
        nothing,           #  slottypes
        Any,               #  rettype
        nothing,           #  parent
        nothing,           #  edges
        Csize_t(1),        #  min_world
        typemax(Csize_t),  #  max_world
        false,             #  inferred
        false,             #  propagate_inbounds
        false,             #  has_fcall
        false,             #  nospecializeinfer
        0x00,              #  inlining
        0x00,              #  constprop
        0x0000,            #  purity
        0xffff,            #  inlining_cost
    )
end

function to_expr(mod, var_info, ex)
    k = kind(ex)
    if is_literal(k)
        ex.value
    elseif k == K"core"
        GlobalRef(Core, Symbol(ex.name_val))
    elseif k == K"top"
        GlobalRef(Base, Symbol(ex.name_val))
    elseif k == K"Identifier"
        # Implicitly refers to name in parent module
        # TODO: Should we even have plain identifiers at this point or should
        # they all effectively be resolved into GlobalRef earlier?
        Symbol(ex.name_val)
    elseif k == K"slot"
        Core.SlotNumber(ex.var_id)
    elseif k == K"SSAValue"
        Core.SSAValue(ex.var_id)
    elseif k == K"return"
        Core.ReturnNode(to_expr(mod, var_info, ex[1]))
    elseif is_quoted(k)
        TODO(ex, "Convert SyntaxTree to Expr")
    elseif k == K"lambda"
        funcname = ex.lambda_info.is_toplevel_thunk ?
            "top-level scope" :
            "none"              # FIXME
        ir = to_code_info(ex[1], mod, funcname, var_info, ex.slot_rewrites)
        if ex.lambda_info.is_toplevel_thunk
            Expr(:thunk, ir)
        else
            ir
        end
    elseif k == K"Value"
        ex.value
    else
        # Allowed forms according to https://docs.julialang.org/en/v1/devdocs/ast/
        #
        # call invoke static_parameter `=` method struct_type abstract_type
        # primitive_type global const new splatnew isdefined the_exception
        # enter leave pop_exception inbounds boundscheck loopinfo copyast meta
        # foreigncall new_opaque_closure lambda
        head = k == K"call"   ? :call   :
               k == K"="      ? :(=)    :
               k == K"method" ? :method :
               k == K"global" ? :global :
               k == K"const"  ? :const  :
               nothing
        if isnothing(head)
            TODO(ex, "Unhandled form for kind $k")
        end
        Expr(head, map(e->to_expr(mod, var_info, e), children(ex))...)
    end
end

