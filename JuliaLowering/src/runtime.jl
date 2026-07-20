# Runtime support for
# 1. Functions called by the code emitted from lowering
# 2. Introspecting Julia's state during lowering
#
# These should probably all move to `Core` at some point.

#-------------------------------------------------------------------------------
# Functions/types used by code emitted from lowering, but not called by it directly

@inline function _invoke_in_world(w::UInt, f::F, @nospecialize(args...)) where {F}
    if ccall(:jl_is_in_pure_context, Int8, ()) != 0
        # Similar to `Base.invoke_in_world` but also works inside generated-function
        # expansion (see `jl_code_for_staged`)
        return Core._call_in_world_total(w, f, args...)
    end
    return Base.invoke_in_world(w, f, args...)
end

# Re-dispatch `f(args...)` at the pinned lowering world (see `jl_lowering_world`)
@inline function invoke_in_lowering_world(f::F, @nospecialize(args...)) where {F}
    @static if VERSION >= v"1.14.0-DEV.2635"
        w = unsafe_load(cglobal(:jl_lowering_world, Csize_t))
        if w == 0
            # Fallback when the Base lowering hook is not set up
            w = Base.tls_world_age()
            # FIXME: as a side effect, enabling the Base lowering hook now affects
            #        JuliaLowering execution not passing through the hook
        end
        return _invoke_in_world(w, f, args...)
    else
        f(args...)
    end
end

# Return the current exception. In JuliaLowering we use this rather than the
# special form `K"the_exception"` to reduce the number of special forms.
Base.@assume_effects :removable function current_exception()
    @ccall jl_current_exception(current_task()::Any)::Any
end

function __interpolate_expr(@nospecialize(ex), depth, @nospecialize(vals::Tuple), val_i)
    if ex isa QuoteNode
        out = __interpolate_expr(Expr(:inert, ex.value), depth, vals, val_i)
        QuoteNode(only(out.args))
    elseif !(ex isa Expr)
        ex
    else
        inner_depth = ex.head == :quote ? depth + 1 :
            ex.head == :$ ? depth - 1 : depth
        cs_out = Any[]
        for e in ex.args
            if e isa Expr && e.head == :$ && inner_depth == 0
                tup = vals[val_i[] += 1]::Tuple
                for v in tup
                    push!(cs_out, v)
                end
            else
                push!(cs_out, __interpolate_expr(e, inner_depth, vals, val_i))
            end
        end
        Expr(ex.head, cs_out...)
    end
end
function _interpolate_expr(@nospecialize(ex), @nospecialize(values::Tuple))
    @jl_assert !Meta.isexpr(ex, :$) (expr_to_est(ex), "expand_quote should handle this")
    __interpolate_expr(ex, 0, values, Ref(0))
end
function interpolate_expr(@nospecialize(ex), @nospecialize(values...))
    return invoke_in_lowering_world(_interpolate_expr, ex, values)
end

function __interpolate_syntax(st::SyntaxTree, depth, @nospecialize(vals), val_i)
    is_leaf(st) && return mkleaf(st)
    k = kind(st)
    inner_depth = k == K"syntaxquote" ? depth + 1 :
        k == K"syntaxunquote" ? depth - 1 : depth
    cs_out = SyntaxList(st._graph)
    for c in children(st)
        if kind(c) == K"syntaxunquote" && inner_depth == 0
            tup = vals[val_i[] += 1]::Tuple
            @jl_assert numchildren(c) == 1 st
            @jl_assert kind(c[1]) === K"..." || length(tup) == 1 st
            for v in tup
                v2 = !(v isa SyntaxTree) ? expr_to_est(st._graph, v, c._id) :
                   copy_ast(st._graph, v)
                push!(cs_out, v2)
            end
        else
            push!(cs_out, __interpolate_syntax(c, inner_depth, vals, val_i))
        end
    end
    mknode(st, cs_out)
end
function _interpolate_syntax(st::SyntaxTree, @nospecialize(vals::Tuple))
    st = copy_ast(ensure_macro_attributes!(SyntaxGraph()), st)
    val_i = Ref(0)
    out = __interpolate_syntax((@ast st._graph st [K"None" st]), 0, vals, val_i)
    @jl_assert val_i[] == length(vals) st
    @jl_assert numchildren(out) == 1 st
    out[1]
end
function interpolate_syntax(st::SyntaxTree, @nospecialize(vals...))
    return invoke_in_lowering_world(_interpolate_syntax, st, vals)
end

#--------------------------------------------------
# Functions called by closure conversion
function eval_closure_type(mod::Module, closure_type_name::Symbol,
                           capt_sp, field_names, field_is_box)
    type_params = Core.TypeVar[]
    field_types = []
    for name in capt_sp
        push!(type_params, Core.TypeVar(name))
    end
    for (name, isbox) in zip(field_names, field_is_box)
        if !isbox
            T = Core.TypeVar(Symbol(name, "_type"))
            push!(type_params, T)
            push!(field_types, T)
        else
            push!(field_types, Core.Box)
        end
    end
    type = Core._structtype(mod, closure_type_name,
                            Core.svec(type_params...),
                            Core.svec(field_names...),
                            Core.svec(),
                            false,
                            length(field_names))
    Core._setsuper!(type, Core.Function)
    Core.declare_const(mod, closure_type_name, type)
    Core._typebody!(false, type, Core.svec(field_types...))
    type
end

# Interpolate captured local variables into the CodeInfo for a global method
function replace_captured_locals(ci_in::Core.CodeInfo, locals::Core.SimpleVector)
    ci = copy(ci_in)
    for (i, ex) in enumerate(ci.code)
        ci.code[i] = _replace_captured_locals(ex, locals)
    end
    ci
end
function _replace_captured_locals(@nospecialize(e), locals)
    if e isa Expr
        if e.head === :captured_local
            v = locals[e.args[1]::Int]
            isa_lowering_ast_node(v) ? QuoteNode(v) : v
        else
            # could possibly limit to foreigncall
            Expr(e.head, map(a->_replace_captured_locals(a, locals), e.args)...)
        end
    elseif e isa QuoteNode
        QuoteNode(_replace_captured_locals(e.value, locals))
    else
        e
    end
end

#--------------------------------------------------
# Functions which create modules or mutate their bindings

const _Base_has_eval_import = isdefined(Base, :_eval_import)

function eval_import(imported::Bool, to::Module, from::Union{Expr, Nothing}, paths::Expr...)
    if _Base_has_eval_import
        Base._eval_import(imported, to, from, paths...)
    else
        head = imported ? :import : :using
        ex = isnothing(from) ?
            Expr(head, paths...) :
            Expr(head, Expr(Symbol(":"), from, paths...))
        Core.eval(to, ex)
    end
end

function eval_using(to::Module, path::Expr)
    if _Base_has_eval_import
        Base._eval_using(to, path)
    else
        Core.eval(to, Expr(:using, path))
    end
end

function eval_public(mod::Module, is_exported::Bool, identifiers)
    # symbol jl_module_public is no longer exported as of #57765
    Core.eval(mod, Expr((is_exported ? :export : :public), map(Symbol, identifiers)...))
end

#--------------------------------------------------
# Docsystem integration
function _bind_func_docs!(f, docstr, method_metadata::Core.SimpleVector)
    mod = parentmodule(f)
    bind = Base.Docs.Binding(mod, nameof(f))
    full_sig = method_metadata[1]
    arg_sig = Tuple{full_sig[2:end]...}
    lineno = method_metadata[3]
    metadata = Dict{Symbol, Any}(
        :linenumber => lineno.line,
        :module => mod,
    )
    if !isnothing(lineno.file)
        push!(metadata, :path => string(lineno.file))
    end
    Docs.doc!(mod, bind, Base.Docs.docstr(docstr, metadata), arg_sig)
end

function bind_docs!(f::Function, docstr, method_metadata::Core.SimpleVector)
    _bind_func_docs!(f, docstr, method_metadata)
end

# Document constructors
function bind_docs!(::Type{Type{T}}, docstr, method_metadata::Core.SimpleVector) where T
    _bind_func_docs!(T, docstr, method_metadata)
end

function bind_docs!(type::Type, docstr, method_metadata::Core.SimpleVector)
    _bind_func_docs!(type, docstr, method_metadata)
end

function bind_docs!(type::Type, docstr, lineno::LineNumberNode; field_docs=Core.svec())
    mod = parentmodule(type)
    bind = Base.Docs.Binding(mod, nameof(type))
    metadata = Dict{Symbol, Any}(
        :linenumber => lineno,
        :module => mod,
    )
    if !isnothing(lineno.file)
        push!(metadata, :path => string(lineno.file))
    end
    if !isempty(field_docs)
        fd = Dict{Symbol, Any}()
        fns = fieldnames(type)
        for i = 1:2:length(field_docs)
            fd[fns[field_docs[i]]] = field_docs[i+1]
        end
        metadata[:fields] = fd
    end
    Docs.doc!(mod, bind, Base.Docs.docstr(docstr, metadata), Union{})
end

"""
Called in the unfortunate cases (K"call", K".", K"Identifier") where docstrings
change the semantics of the expressions they annotate, no longer requiring the
expression to execute.
"""
function bind_static_docs!(mod::Module, name::Symbol, docstr, lnn::LineNumberNode, sigtypes::Type)
    metadata = Dict{Symbol, Any}(
        :linenumber => lnn.line,
        :module => mod,
        :path => something(lnn.file, "none"),
    )
    bind = Base.Docs.Binding(mod, name)
    Docs.doc!(mod, bind, Base.Docs.docstr(docstr, metadata), sigtypes)
end

#--------------------------------------------------
# Runtime support infrastructure for `@generated`

# An alternative to Core.GeneratedFunctionStub which works on SyntaxTree rather
# than Expr.
struct GeneratedFunctionStub
    syntax_context::SyntaxContext
    gen::Function
    srcref::Union{LineNumberNode,SourceRef}
    argnames::Core.SimpleVector
    spnames::Core.SimpleVector
end

function _gen_args_from_syms(ctx, src, args, sc)
    out = SyntaxList(ctx.graph)
    for a in args
        id = newleaf(syntax_graph(ctx), src, K"Identifier", string(a))
        id = _est_to_dst_ident(id) # support placeholders
        id = setattr!(id, :context, sc)
        push!(out, id)
    end
    out
end

# Call the `@generated` code generator function and wrap the results of the
# expression into a CodeInfo.
#
# `args` passed into stub by the Julia runtime are (parent_func, static_params..., arg_types...)
function (g::GeneratedFunctionStub)(world::UInt, source::Method, @nospecialize args...)
    # Some of the lowering pipeline from lower() and the pass-specific setup is
    # re-implemented here because generated functions are very much (but not
    # entirely) like macro expansion.
    #
    # TODO: Reduce duplication where possible.

    graph = ensure_desugaring_attributes!(SyntaxGraph())
    __module__ = source.module

    sc = g.syntax_context

    # Run code generator - this acts like a macro expander
    ex0 = g.gen(sc, args...)

    # Note that we expand in `tls_world_age()` (see Core.GeneratedFunctionStub)
    world = Base.tls_world_age()

    # Lower the generated code in the lowering world
    return invoke_in_lowering_world(_lower_generated_code, g, source, graph, sc,
                                   __module__, world, ex0)
end

function _lower_generated_code(g::GeneratedFunctionStub, source::Method, graph,
                               sc::SyntaxContext, __module__::Module,
                               world::UInt, @nospecialize(ex0))
    if ex0 isa Expr
        ex0 = expr_to_est(
            graph, ex0, source_location(LineNumberNode, g.srcref))
    end
    if ex0 isa SyntaxTree
        if !is_compatible_graph(graph, ex0)
            ex0 = copy_ast(graph, ex0)
        end
    else
        ex0 isa Expr && throw(LoweringError(
            ex0, "implicit expr->syntaxtree: may later be allowed, but is probably a mistake today"))
        ex0 = expr_to_est(graph, ex0, g.srcref)
    end

    @jl_assert base_layer(sc).mod == __module__ ex0
    ex0 = JuliaSyntax.fill_context!(ex0, sc)
    ctx1 = MacroExpansionContext(ex0, world, true)
    ex1 = expand_forms_1(ctx1, ex0)
    # Desugaring
    ctx2, ex2 = expand_forms_2(ex1, world)

    # Wrap expansion in a non-toplevel lambda and run scope resolution
    ex2 = @ast ctx2 ex0 [K"lambda"(is_toplevel_thunk=false, toplevel_pure=true)
        [K"block" _gen_args_from_syms(ctx2, ex1, g.argnames, sc)...]
        [K"block" _gen_args_from_syms(ctx2, ex1, g.spnames, sc)...]
        ex2
    ]
    ctx3, ex3 = resolve_scopes(ctx2, ex2)

    # Rest of lowering
    ctx4, ex4 = convert_closures(ctx3, ex3)
    _ctx5, ex5 = linearize_ir(ctx4, ex4)
    ci = to_lowered_expr(ex5)
    @assert ci isa Core.CodeInfo

    # See GeneratedFunctionStub code in base/expr.jl
    ci.isva = source.isva
    code = ci.code
    bindings = IdSet{Core.Binding}()
    for i = 1:length(code)
        stmt = code[i]
        if isa(stmt, GlobalRef)
            push!(bindings, convert(Core.Binding, stmt))
        end
    end
    if !isempty(bindings)
        ci.edges = Core.svec(bindings...)
    end

    return ci
end


#-------------------------------------------------------------------------------
# The following functions are called directly by lowering to inspect Julia's state.

# Get the binding for `name` if one is already resolved in module `mod`. Note
# that we cannot use `isdefined(::Module, ::Symbol)` here, because that causes
# binding resolution which is a massive side effect we must avoid in lowering.
function _get_module_binding(mod, name; create=false)
    b = @ccall jl_get_module_binding(mod::Module, name::Symbol, create::Cint)::Ptr{Core.Binding}
    b == C_NULL ? nothing : unsafe_pointer_to_objref(b)
end

# Return true if a `name` is defined in and *by* the module `mod`.
# Has no side effects, unlike isdefined()
#
# (This should do what fl_defined_julia_global does for flisp lowering)
function is_defined_and_owned_global(mod, name, world::UInt=Base.get_world_counter())
    return _invoke_in_world(world, Base.binding_kind, mod, name) === Base.PARTITION_KIND_GLOBAL
end

# "Reserve" a binding: create the binding if it doesn't exist but do not assign
# to it.
function reserve_module_binding(mod, name)
    # TODO: Fix the race condition here: We should really hold the Module's
    # binding lock during this test-and-set type operation. But the binding
    # lock is only accessible from C. See also the C code in
    # `fl_module_unique_name`.
    if _get_module_binding(mod, name; create=false) === nothing
        return _get_module_binding(mod, name; create=true) !== nothing
    else
        return false
    end
end

# Reserve a global binding named "$basename#$i" in module `mod` for the
# smallest `i` starting at `0`.
#
# TODO: Remove the use of this where possible. Currently this is used within
# lowering to create unique global names for keyword function bodies and
# closure types as a more local alternative to current-julia-module-counter.
# However, we should ideally defer it to eval-time to make lowering itself
# completely non-mutating.
function reserve_module_binding_i(mod, basename)
    i = 0
    while true
        name = "$basename$i"
        if reserve_module_binding(mod, Symbol(name))
            return name
        end
        i += 1
    end
end

function lookup_method_instance(func, args, world::Integer)
    allargs = Vector{Any}(undef, length(args) + 1)
    allargs[1] = func
    allargs[2:end] = args
    mi = @ccall jl_method_lookup(allargs::Ptr{Any}, length(allargs)::Csize_t,
                                 world::Csize_t)::Ptr{Cvoid}
    return mi == C_NULL ? nothing : unsafe_pointer_to_objref(mi)
end

# Like `Base.methods()` but with world age support
function methods_in_world(func, arg_sig, world, err_ex)
    out = Base._methods(func, arg_sig, -1, world)
    @jl_assert(out isa Vector{Any},
               (err_ex, string(
                   "Base._methods returned non-vector;",
                   " bad world age provided? (", world, ")")))
    out::Vector{Any}
end
