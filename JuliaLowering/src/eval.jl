# Non-incremental lowering API for non-toplevel non-module expressions.
# May be removed?

function lower(mod::Module, ex0; expr_compat_mode=false, world=Base.get_world_counter())
    ctx1, ex1 = expand_forms_1(  mod,  ex0, expr_compat_mode, world)
    ctx2, ex2 = expand_forms_2(  ctx1, ex1)
    ctx3, ex3 = resolve_scopes(  ctx2, ex2)
    ctx4, ex4 = convert_closures(ctx3, ex3)
    ctx5, ex5 = linearize_ir(    ctx4, ex4)
    ex5
end

function macroexpand(mod::Module, ex; expr_compat_mode=false, world=Base.get_world_counter())
    ctx1, ex1 = expand_forms_1(mod, ex, expr_compat_mode, world)
    ex1
end

# Incremental lowering API which can manage toplevel and module expressions.
#
# This iteration API is oddly bespoke and arguably somewhat non-Julian for two
# reasons:
#
# * Lowering knows when new modules are required, and may request them with
#   `:begin_module`. However `eval()` generates those modules so they need to
#   be passed back into lowering. So we can't just use `Base.iterate()`. (Put a
#   different way, we have a situation which is suited to coroutines but we
#   don't want to use full Julia `Task`s for this.)
# * We might want to implement this `eval()` in Julia's C runtime code or early
#   in bootstrap. Hence using SimpleVector and Symbol as the return values of
#   `lower_step()`
#
# We might consider changing at least the second of these choices, depending on
# how we end up putting this into Base.

struct LoweringIterator{GraphType}
    ctx::MacroExpansionContext{GraphType}
    todo::Vector{Tuple{SyntaxTree{GraphType}, Bool, Int}}
end

function lower_init(ex::SyntaxTree, mod::Module, macro_world::UInt; expr_compat_mode::Bool=false)
    graph = ensure_macro_attributes(syntax_graph(ex))
    ctx = MacroExpansionContext(graph, mod, expr_compat_mode, macro_world)
    ex = reparent(ctx, ex)
    LoweringIterator{typeof(graph)}(ctx, [(ex, false, 0)])
end

function lower_step(iter, push_mod=nothing)
    if !isnothing(push_mod)
        push_layer!(iter.ctx, push_mod, false)
    end

    if isempty(iter.todo)
        return Core.svec(:done)
    end

    ex, is_module_body, child_idx = pop!(iter.todo)
    if child_idx > 0
        next_child = child_idx + 1
        if child_idx <= numchildren(ex)
            push!(iter.todo, (ex, is_module_body, next_child))
            ex = ex[child_idx]
        else
            if is_module_body
                pop_layer!(iter.ctx)
                return Core.svec(:end_module)
            else
                return lower_step(iter)
            end
        end
    end

    k = kind(ex)
    if !(k in KSet"toplevel module")
        ex = expand_forms_1(iter.ctx, ex)
        k = kind(ex)
    end
    if k == K"toplevel"
        push!(iter.todo, (ex, false, 1))
        return lower_step(iter)
    elseif k == K"module"
        name = ex[1]
        if kind(name) != K"Identifier"
            throw(LoweringError(name, "Expected module name"))
        end
        newmod_name = Symbol(name.name_val)
        body = ex[2]
        if kind(body) != K"block"
            throw(LoweringError(body, "Expected block in module body"))
        end
        std_defs = !has_flags(ex, JuliaSyntax.BARE_MODULE_FLAG)
        loc = source_location(LineNumberNode, ex)
        push!(iter.todo, (body, true, 1))
        return Core.svec(:begin_module, newmod_name, std_defs, loc)
    else
        # Non macro expansion parts of lowering
        ctx2, ex2 = expand_forms_2(iter.ctx, ex)
        ctx3, ex3 = resolve_scopes(ctx2, ex2)
        ctx4, ex4 = convert_closures(ctx3, ex3)
        ctx5, ex5 = linearize_ir(ctx4, ex4)
        thunk = to_lowered_expr(ex5)
        return Core.svec(:thunk, thunk)
    end
end


#-------------------------------------------------------------------------------

function codeinfo_has_image_globalref(@nospecialize(e))
    if e isa GlobalRef
        return 0x00 !== @ccall jl_object_in_image(e.mod::Any)::UInt8
    elseif e isa Core.CodeInfo
        return any(codeinfo_has_image_globalref, e.code)
    else
        return false
    end
end

_CodeInfo_need_ver = v"1.12.0-DEV.512"
if VERSION < _CodeInfo_need_ver
    function _CodeInfo(args...)
        error("Constructing a CodeInfo using JuliaLowering currently requires Julia version $_CodeInfo_need_ver or greater")
    end
else
    # debuginfo changed completely as of https://github.com/JuliaLang/julia/pull/52415
    # nargs / isva was added as of       https://github.com/JuliaLang/julia/pull/54341
    # field rettype added in             https://github.com/JuliaLang/julia/pull/54655
    # field has_image_globalref added in https://github.com/JuliaLang/julia/pull/57433
    # CodeInfo constructor. TODO: Should be in Core
    let
        fns = fieldnames(Core.CodeInfo)
        fts = fieldtypes(Core.CodeInfo)
        conversions = [:(convert($t, $n)) for (t,n) in zip(fts, fns)]

        expected_fns = (:code, :debuginfo, :ssavaluetypes, :ssaflags, :slotnames, :slotflags, :slottypes, :rettype, :parent, :edges, :min_world, :max_world, :method_for_inference_limit_heuristics, :nargs, :propagate_inbounds, :has_fcall, :has_image_globalref, :nospecializeinfer, :isva, :inlining, :constprop, :purity, :inlining_cost)
        expected_fts = (Vector{Any}, Core.DebugInfo, Any, Vector{UInt32}, Vector{Symbol}, Vector{UInt8}, Any, Any, Any, Any, UInt64, UInt64, Any, UInt64, Bool, Bool, Bool, Bool, Bool, UInt8, UInt8, UInt16, UInt16)

        code = if fns != expected_fns
            unexpected_fns = collect(setdiff(Set(fns), Set(expected_fns)))
            missing_fns = collect(setdiff(Set(expected_fns), Set(fns)))
            :(function _CodeInfo(args...)
                  error("Unrecognized CodeInfo fields: Maybe version $VERSION is too new for this version of JuliaLowering?"
                         * isempty(unexpected_fns) ? "" : "\nUnexpected fields found: $($unexpected_fns)"
                         * isempty(missing_fns)    ? "" : "\nMissing fields:          $($missing_fns)")
              end)
        elseif fts != expected_fts
            :(function _CodeInfo(args...)
                  error("Unrecognized CodeInfo field types: Maybe version $VERSION is too new for this version of JuliaLowering?")
              end)
        else
            :(function _CodeInfo($(fns...))
                $(Expr(:new, :(Core.CodeInfo), conversions...))
            end)
        end

        Core.eval(@__MODULE__, code)
    end
end

function _compress_debuginfo(info)
    filename, edges, codelocs = info
    edges = Core.svec(map(_compress_debuginfo, edges)...)
    codelocs = @ccall jl_compress_codelocs((-1)::Int32, codelocs::Any,
                                           div(length(codelocs),3)::Csize_t)::String
    Core.DebugInfo(Symbol(filename), nothing, edges, codelocs)
end

function ir_debug_info_state(ex)
    e1 = first(flattened_provenance(ex))
    topfile = filename(e1)
    [(topfile, [], Vector{Int32}())]
end

function add_ir_debug_info!(current_codelocs_stack, stmt)
    locstk = [(filename(e), source_location(e)[1]) for e in flattened_provenance(stmt)]
    for j in 1:length(locstk)
        if j === 1 && current_codelocs_stack[j][1] != locstk[j][1]
            # dilemma: the filename stack here shares no prefix with that of the
            # previous statement, where differing filenames usually (j > 1) mean
            # a different macro expansion has started at this statement.  guess
            # that both files are the same, and inherit the previous filename.
            locstk[j] = (current_codelocs_stack[j][1], locstk[j][2])
        end
        if j < length(current_codelocs_stack) && (j === length(locstk) ||
                current_codelocs_stack[j+1][1] != locstk[j+1][1])
            while j < length(current_codelocs_stack)
                info = pop!(current_codelocs_stack)
                push!(last(current_codelocs_stack)[2], info)
            end
        elseif j > length(current_codelocs_stack)
            push!(current_codelocs_stack, (locstk[j][1], [], Vector{Int32}()))
        end
    end
    @assert length(locstk) === length(current_codelocs_stack)
    for (j, (file,line)) in enumerate(locstk)
        fn, edges, codelocs = current_codelocs_stack[j]
        @assert fn == file
        if j < length(locstk)
            edge_index = length(edges) + 1
            edge_codeloc_index = fld1(length(current_codelocs_stack[j+1][3]) + 1, 3)
        else
            edge_index = 0
            edge_codeloc_index = 0
        end
        push!(codelocs, line)
        push!(codelocs, edge_index)
        push!(codelocs, edge_codeloc_index)
    end
end

function finish_ir_debug_info!(current_codelocs_stack)
    while length(current_codelocs_stack) > 1
        info = pop!(current_codelocs_stack)
        push!(last(current_codelocs_stack)[2], info)
    end

    _compress_debuginfo(only(current_codelocs_stack))
end

# Convert SyntaxTree to the CodeInfo+Expr data structures understood by the
# Julia runtime
function to_code_info(ex::SyntaxTree, slots::Vector{Slot}, meta::CompileHints)
    stmts = Any[]

    current_codelocs_stack = ir_debug_info_state(ex)

    nargs = sum((s.kind==:argument for s in slots), init=0)
    slotnames = Vector{Symbol}(undef, length(slots))
    slot_rename_inds = Dict{String,Int}()
    slotflags = Vector{UInt8}(undef, length(slots))
    for (i, slot) in enumerate(slots)
        name = slot.name
        # TODO: Do we actually want unique names here? The C code in
        # `jl_new_code_info_from_ir` has logic to simplify gensym'd names and
        # use the empty string for compiler-generated bindings.
        ni = get(slot_rename_inds, name, 0)
        slot_rename_inds[name] = ni + 1
        if ni > 0
            name = "$name@$ni"
        end
        sname = Symbol(name)
        slotnames[i] = sname
        slotflags[i] =                   # Inference          | Codegen
            slot.is_read          << 3 | # SLOT_USED          | jl_vinfo_sa
            slot.is_single_assign << 4 | # SLOT_ASSIGNEDONCE  | -
            slot.is_maybe_undef   << 5 | # SLOT_USEDUNDEF     | jl_vinfo_usedundef
            slot.is_called        << 6   # SLOT_CALLED        | -
        if slot.is_nospecialize
            # Ideally this should be a slot flag instead
            add_ir_debug_info!(current_codelocs_stack, ex)
            push!(stmts, Expr(:meta, :nospecialize, Core.SlotNumber(i)))
        end
    end

    stmt_offset = length(stmts)
    for stmt in children(ex)
        push!(stmts, _to_lowered_expr(stmt, stmt_offset))
        add_ir_debug_info!(current_codelocs_stack, stmt)
    end

    debuginfo = finish_ir_debug_info!(current_codelocs_stack)

    has_image_globalref = any(codeinfo_has_image_globalref, stmts)

    # TODO: Set ssaflags based on call site annotations:
    # - @inbounds annotations
    # - call site @inline / @noinline
    # - call site @assume_effects
    ssaflags = zeros(UInt32, length(stmts))

    propagate_inbounds =
        get(meta, :propagate_inbounds, false)
    # TODO: Set true if there's a foreigncall
    has_fcall = false
    nospecializeinfer =
        get(meta, :nospecializeinfer, false)
    inlining =
        get(meta, :inline, false) ? 0x01 :
        get(meta, :noinline, false) ? 0x02 : 0x00
    constprop =
        get(meta, :aggressive_constprop, false) ? 0x01 :
        get(meta, :no_constprop, false) ? 0x02 : 0x00
    purity =
        let eo = get(meta, :purity, nothing)
            isnothing(eo) ? 0x0000 : Base.encode_effects_override(eo)
        end

    # The following CodeInfo fields always get their default values for
    # uninferred code.
    ssavaluetypes      = length(stmts) # Why does the runtime code do this?
    slottypes          = nothing
    parent             = nothing
    method_for_inference_limit_heuristics = nothing
    edges               = nothing
    min_world           = Csize_t(1)
    max_world           = typemax(Csize_t)
    isva                = false
    inlining_cost       = 0xffff
    rettype             = Any

    _CodeInfo(
        stmts,
        debuginfo,
        ssavaluetypes,
        ssaflags,
        slotnames,
        slotflags,
        slottypes,
        rettype,
        parent,
        edges,
        min_world,
        max_world,
        method_for_inference_limit_heuristics,
        nargs,
        propagate_inbounds,
        has_fcall,
        has_image_globalref,
        nospecializeinfer,
        isva,
        inlining,
        constprop,
        purity,
        inlining_cost
    )
end

@fzone "JL: to_lowered_expr" function to_lowered_expr(ex::SyntaxTree)
    _to_lowered_expr(ex, 0)
end

function _to_lowered_expr(ex::SyntaxTree, stmt_offset::Int)
    k = kind(ex)
    if is_literal(k)
        ex.value
    elseif k == K"core"
        name = ex.name_val
        if name == "cglobal"
            # Inference expects cglobal as call argument to be `GlobalRef`,
            # so we resolve that name as a symbol of `Core.Intrinsics` here.
            # https://github.com/JuliaLang/julia/blob/7a8cd6e202f1d1216a6c0c0b928fb43a123cada8/Compiler/src/validation.jl#L87
            GlobalRef(Core.Intrinsics, :cglobal)
        elseif name == "nothing"
            # Translate Core.nothing into literal `nothing`s (flisp uses a
            # special form (null) for this during desugaring, etc)
            nothing
        else
            GlobalRef(Core, Symbol(name))
        end
    elseif k == K"top"
        GlobalRef(Base, Symbol(ex.name_val))
    elseif k == K"globalref"
        GlobalRef(ex.mod, Symbol(ex.name_val))
    elseif k == K"Identifier"
        # Implicitly refers to name in parent module
        # TODO: Should we even have plain identifiers at this point or should
        # they all effectively be resolved into GlobalRef earlier?
        Symbol(ex.name_val)
    elseif k == K"SourceLocation"
        QuoteNode(source_location(LineNumberNode, ex))
    elseif k == K"Symbol"
        QuoteNode(Symbol(ex.name_val))
    elseif k == K"slot"
        Core.SlotNumber(ex.var_id)
    elseif k == K"static_parameter"
        Expr(:static_parameter, ex.var_id)
    elseif k == K"SSAValue"
        Core.SSAValue(ex.var_id + stmt_offset)
    elseif k == K"return"
        Core.ReturnNode(_to_lowered_expr(ex[1], stmt_offset))
    elseif k == K"inert"
        e1 = ex[1]
        getmeta(ex, :as_Expr, false) ? QuoteNode(Expr(e1)) : e1
    elseif k == K"code_info"
        ir = to_code_info(ex[1], ex.slots, ex.meta)
        if ex.is_toplevel_thunk
            Expr(:thunk, ir) # TODO: Maybe nice to just return a CodeInfo here?
        else
            ir
        end
    elseif k == K"Value"
        ex.value
    elseif k == K"goto"
        Core.GotoNode(ex[1].id + stmt_offset)
    elseif k == K"gotoifnot"
        Core.GotoIfNot(_to_lowered_expr(ex[1], stmt_offset), ex[2].id + stmt_offset)
    elseif k == K"enter"
        catch_idx = ex[1].id
        numchildren(ex) == 1 ?
            Core.EnterNode(catch_idx) :
            Core.EnterNode(catch_idx, _to_lowered_expr(ex[2], stmt_offset))
    elseif k == K"method"
        cs = map(e->_to_lowered_expr(e, stmt_offset), children(ex))
        # Ad-hoc unwrapping to satisfy `Expr(:method)` expectations
        cs1 = cs[1]
        c1 = cs1 isa QuoteNode ? cs1.value : cs1
        Expr(:method, c1, cs[2:end]...)
    elseif k == K"newvar"
        Core.NewvarNode(_to_lowered_expr(ex[1], stmt_offset))
    elseif k == K"opaque_closure_method"
        args = map(e->_to_lowered_expr(e, stmt_offset), children(ex))
        # opaque_closure_method has special non-evaluated semantics for the
        # `functionloc` line number node so we need to undo a level of quoting
        arg4 = args[4]
        @assert arg4 isa QuoteNode
        args[4] = arg4.value
        Expr(:opaque_closure_method, args...)
    elseif k == K"meta"
        args = Any[_to_lowered_expr(e, stmt_offset) for e in children(ex)]
        # Unpack K"Symbol" QuoteNode as `Expr(:meta)` requires an identifier here.
        arg1 = args[1]
        @assert arg1 isa QuoteNode
        args[1] = arg1.value
        Expr(:meta, args...)
    elseif k == K"static_eval"
        @assert numchildren(ex) == 1
        _to_lowered_expr(ex[1], stmt_offset)
    elseif k == K"cfunction"
        args = Any[_to_lowered_expr(e, stmt_offset) for e in children(ex)]
        if kind(ex[2]) == K"static_eval"
            args[2] = QuoteNode(args[2])
        end
        Expr(:cfunction, args...)
    else
        # Allowed forms according to https://docs.julialang.org/en/v1/devdocs/ast/
        #
        # call invoke static_parameter `=` method struct_type abstract_type
        # primitive_type global const new splatnew isdefined
        # enter leave pop_exception inbounds boundscheck loopinfo copyast meta
        # lambda
        head = k == K"call"      ? :call       :
               k == K"new"       ? :new        :
               k == K"splatnew"  ? :splatnew   :
               k == K"="         ? :(=)        :
               k == K"leave"     ? :leave      :
               k == K"isdefined" ? :isdefined  :
               k == K"latestworld"       ? :latestworld       :
               k == K"pop_exception"     ? :pop_exception     :
               k == K"captured_local"    ? :captured_local    :
               k == K"gc_preserve_begin" ? :gc_preserve_begin :
               k == K"gc_preserve_end"   ? :gc_preserve_end   :
               k == K"foreigncall"       ? :foreigncall       :
               k == K"new_opaque_closure" ? :new_opaque_closure :
               nothing
        if isnothing(head)
            throw(LoweringError(ex, "Unhandled form for kind $k"))
        end
        ret = Expr(head)
        for e in children(ex)
            push!(ret.args, _to_lowered_expr(e, stmt_offset))
        end
        return ret
    end
end

#-------------------------------------------------------------------------------
# Our version of eval - should be upstreamed though?
@fzone "JL: eval" function eval(mod::Module, ex::SyntaxTree;
                                macro_world::UInt=Base.get_world_counter(),
                                opts...)
    iter = lower_init(ex, mod, macro_world; opts...)
    _eval(mod, iter)
end

# Version of eval() taking `Expr` (or Expr tree leaves of any type)
function eval(mod::Module, ex; opts...)
    eval(mod, expr_to_syntaxtree(ex); opts...)
end

if VERSION >= v"1.13.0-DEV.1199" # https://github.com/JuliaLang/julia/pull/59604

function _eval(mod, iter)
    modules = Module[]
    new_mod = nothing
    result = nothing
    while true
        thunk = lower_step(iter, new_mod)::Core.SimpleVector
        new_mod = nothing
        type = thunk[1]::Symbol
        if type == :done
            break
        elseif type == :begin_module
            push!(modules, mod)
            filename = something(thunk[4].file, :none)
            mod = @ccall jl_begin_new_module(mod::Any, thunk[2]::Symbol, thunk[3]::Cint,
                                             filename::Cstring, thunk[4].line::Cint)::Module
            new_mod = mod
        elseif type == :end_module
            @ccall jl_end_new_module(mod::Module)::Cvoid
            result = mod
            mod = pop!(modules)
        else
            @assert type == :thunk
            result = Core.eval(mod, thunk[2])
        end
    end
    @assert isempty(modules)
    return result
end

else

function _eval(mod, iter, new_mod=nothing)
    in_new_mod = !isnothing(new_mod)
    result = nothing
    while true
        thunk = lower_step(iter, new_mod)::Core.SimpleVector
        new_mod = nothing
        type = thunk[1]::Symbol
        if type == :done
            @assert !in_new_mod
            break
        elseif type == :begin_module
            name = thunk[2]::Symbol
            std_defs = thunk[3]
            result = Core.eval(mod,
                Expr(:module, std_defs, name,
                     Expr(:block, thunk[4], Expr(:call, m->_eval(m, iter, m), name)))
            )
        elseif type == :end_module
            @assert in_new_mod
            return mod
        else
            @assert type == :thunk
            result = Core.eval(mod, thunk[2])
        end
    end
    return result
end

end

"""
    include(mod::Module, path::AbstractString)

Evaluate the contents of the input source file in the global scope of module
`mod`. Every module (except those defined with baremodule) has its own
definition of `include()` omitting the `mod` argument, which evaluates the file
in that module. Returns the result of the last evaluated expression of the
input file. During including, a task-local include path is set to the directory
containing the file. Nested calls to include will search relative to that path.
This function is typically used to load source interactively, or to combine
files in packages that are broken into multiple source files.
"""
function include(mod::Module, path::AbstractString)
    path, prev = Base._include_dependency(mod, path)
    code = read(path, String)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    try
        return include_string(mod, code, path)
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
end

"""
    include_string(mod::Module, code::AbstractString, filename::AbstractString="string")

Like `include`, except reads code from the given string rather than from a file.
"""
function include_string(mod::Module, code::AbstractString, filename::AbstractString="string";
                        expr_compat_mode=false)
    eval(mod, parseall(SyntaxTree, code; filename=filename); expr_compat_mode)
end
