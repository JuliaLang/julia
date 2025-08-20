# Runtime support for
# 1. Functions called by the code emitted from lowering
# 2. Introspecting Julia's state during lowering
#
# These should probably all move to `Core` at some point.

#-------------------------------------------------------------------------------
# Functions/types used by code emitted from lowering, but not called by it directly

# Return the current exception. In JuliaLowering we use this rather than the
# special form `K"the_exception"` to reduces the number of special forms.
Base.@assume_effects :removable :nothrow function current_exception()
    @ccall jl_current_exception(current_task()::Any)::Any
end

#--------------------------------------------------
# Supporting functions for AST interpolation (`quote`)
struct InterpolationContext{Graph} <: AbstractLoweringContext
    graph::Graph
    values::Tuple
    current_index::Ref{Int}
end

function _contains_active_interp(ex, depth)
    k = kind(ex)
    if k == K"$" && depth == 0
        return true
    end
    inner_depth = k == K"quote" ? depth + 1 :
                  k == K"$"     ? depth - 1 :
                  depth
    return any(_contains_active_interp(c, inner_depth) for c in children(ex))
end

# Produce interpolated node for `$x` syntax
function _interpolated_value(ctx, srcref, ex)
    if ex isa SyntaxTree
        if !is_compatible_graph(ctx, ex)
            ex = copy_ast(ctx, ex)
        end
        append_sourceref(ctx, ex, srcref)
    else
        makeleaf(ctx, srcref, K"Value", ex)
    end
end

function _interpolate_ast(ctx::InterpolationContext, ex, depth)
    if ctx.current_index[] > length(ctx.values) || !_contains_active_interp(ex, depth)
        return ex
    end

    # We have an interpolation deeper in the tree somewhere - expand to an
    # expression 
    inner_depth = kind(ex) == K"quote" ? depth + 1 :
                  kind(ex) == K"$"     ? depth - 1 :
                  depth
    expanded_children = SyntaxList(ctx)
    for e in children(ex)
        if kind(e) == K"$" && inner_depth == 0
            vals = ctx.values[ctx.current_index[]]::Tuple
            ctx.current_index[] += 1
            for (i,v) in enumerate(vals)
                srcref = numchildren(e) == 1 ? e : e[i]
                push!(expanded_children, _interpolated_value(ctx, srcref, v))
            end
        else
            push!(expanded_children, _interpolate_ast(ctx, e, inner_depth))
        end
    end

    makenode(ctx, ex, head(ex), expanded_children)
end

function interpolate_ast(ex, values...)
    # Construct graph for interpolation context. We inherit this from the macro
    # context where possible by detecting it using __macro_ctx__. This feels
    # hacky though.
    #
    # Perhaps we should use a ScopedValue for this instead or get it from
    # the macro __context__? None of the options feel great here.
    graph = nothing
    for vals in values
        for v in vals
            if v isa SyntaxTree && hasattr(syntax_graph(v), :__macro_ctx__)
                graph = syntax_graph(v)
                break
            end
        end
    end
    if isnothing(graph)
        graph = ensure_attributes(SyntaxGraph(), kind=Kind, syntax_flags=UInt16, source=SourceAttrType,
                                  value=Any, name_val=String, scope_layer=LayerId)
    end
    ctx = InterpolationContext(graph, values, Ref(1))
    # We must copy the AST into our context to use it as the source reference
    # of generated expressions.
    ex1 = copy_ast(ctx, ex)
    if kind(ex1) == K"$"
        @assert length(values) == 1
        vs = values[1]
        if length(vs) > 1
            # :($($(xs...))) where xs is more than length 1
            throw(LoweringError(ex1, "More than one value in bare `\$` expression"))
        end
        _interpolated_value(ctx, ex1, only(vs))
    else
        _interpolate_ast(ctx, ex1, 0)
    end
end

#--------------------------------------------------
# Functions called by closure conversion
function eval_closure_type(mod, closure_type_name, field_names, field_is_box)
    type_params = Core.TypeVar[]
    field_types = []
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
    Base.eval(mod, :(const $closure_type_name = $type))
    Core._typebody!(false, type, Core.svec(field_types...))
    type
end

# Interpolate captured local variables into the CodeInfo for a global method
function replace_captured_locals!(codeinfo, locals)
    for (i, ex) in enumerate(codeinfo.code)
        if Meta.isexpr(ex, :captured_local)
            codeinfo.code[i] = locals[ex.args[1]]
        end
    end
    codeinfo
end

#--------------------------------------------------
# Functions which create modules or mutate their bindings

# Construct new bare module including only the "default names"
#
#     using Core
#     const modname = modval
#     public modname
#
# And run statments in the toplevel expression `body`
function eval_module(parentmod, modname, body)
    # Here we just use `eval()` with an Expr.
    # If we wanted to avoid this we'd need to reproduce a lot of machinery from
    # jl_eval_module_expr()
    #
    # 1. Register / deparent toplevel modules
    # 2. Set binding in parent module
    # 3. Deal with replacing modules
    #    * Warn if replacing
    #    * Root old module being replaced
    # 4. Run __init__
    #    * Also run __init__ for any children after parent is defined
    # mod = @ccall jl_new_module(Symbol(modname)::Symbol, parentmod::Module)::Any
    # ...
    name = Symbol(modname)
    eval(parentmod, :(
        baremodule $name
            $eval($name, $body)
        end
    ))
end

# Evaluate content of `import` or `using` statement
function module_import(into_mod::Module, is_using::Bool,
                       from_mod::Union{Nothing,Core.SimpleVector}, paths::Core.SimpleVector)
    # For now, this function converts our lowered representation back to Expr
    # and calls eval() to avoid replicating all of the fiddly logic in
    # jl_toplevel_eval_flex.
    # TODO: ccall Julia runtime functions directly?
    #   * jl_module_using jl_module_use_as
    #   * import_module jl_module_import_as
    path_args = []
    i = 1
    while i < length(paths)
        nsyms = paths[i]::Int
        n = i + nsyms
        path = Expr(:., [Symbol(paths[i+j]::String) for j = 1:nsyms]...)
        as_name = paths[i+nsyms+1]
        push!(path_args, isnothing(as_name) ? path :
                         Expr(:as, path, Symbol(as_name)))
        i += nsyms + 2
    end
    ex = if isnothing(from_mod)
        Expr(is_using ? :using : :import,
             path_args...)
    else
        from_path = Expr(:., [Symbol(s::String) for s in from_mod]...)
        Expr(is_using ? :using : :import,
             Expr(:(:), from_path, path_args...))
    end
    eval(into_mod, ex)
    nothing
end

function module_public(mod::Module, is_exported::Bool, identifiers...)
    # symbol jl_module_public is no longer exported as of #57765
    eval(mod, Expr((is_exported ? :export : :public), map(Symbol, identifiers)...))
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

#--------------------------------------------------
# Runtime support infrastructure for `@generated`

# An alternative to Core.GeneratedFunctionStub which works on SyntaxTree rather
# than Expr.
struct GeneratedFunctionStub
    gen
    srcref
    argnames::Core.SimpleVector
    spnames::Core.SimpleVector
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

    mod = parentmodule(g.gen)

    # Attributes from parsing
    graph = ensure_attributes(SyntaxGraph(), kind=Kind, syntax_flags=UInt16, source=SourceAttrType,
                              value=Any, name_val=String)

    # Attributes for macro expansion
    graph = ensure_attributes(graph,
                              var_id=IdTag,
                              scope_layer=LayerId,
                              __macro_ctx__=Nothing,
                              meta=CompileHints,
                              # Additional attribute for resolve_scopes, for
                              # adding our custom lambda below
                              is_toplevel_thunk=Bool
                              )

    # Macro expansion
    ctx1 = MacroExpansionContext(graph, mod)

    # Run code generator - this acts like a macro expander and like a macro
    # expander it gets a MacroContext.
    mctx = MacroContext(syntax_graph(ctx1), g.srcref, ctx1.scope_layers[end])
    ex0 = g.gen(mctx, args...)
    if ex0 isa SyntaxTree
        if !is_compatible_graph(ctx1, ex0)
            # If the macro has produced syntax outside the macro context, copy it over.
            # TODO: Do we expect this always to happen?  What is the API for access
            # to the macro expansion context?
            ex0 = copy_ast(ctx1, ex0)
        end
    else
        ex0 = @ast ctx ex expanded::K"Value"
    end
    # Expand any macros emitted by the generator
    ex1 = expand_forms_1(ctx1, reparent(ctx1, ex0))
    ctx1 = MacroExpansionContext(delete_attributes(graph, :__macro_ctx__),
                                 ctx1.bindings, ctx1.scope_layers, LayerId[])
    ex1 = reparent(ctx1, ex1)

    # Desugaring
    ctx2, ex2 = expand_forms_2(ctx1, ex1)

    # Wrap expansion in a non-toplevel lambda and run scope resolution
    ex2 = @ast ctx2 ex0 [K"lambda"(is_toplevel_thunk=false)
        [K"block"
            (string(n)::K"Identifier" for n in g.argnames)...
        ]
        [K"block"
            (string(n)::K"Identifier" for n in g.spnames)...
        ]
        ex2
    ]
    ctx3, ex3 = resolve_scopes(ctx2, ex2)

    # Rest of lowering
    ctx4, ex4 = convert_closures(ctx3, ex3)
    ctx5, ex5 = linearize_ir(ctx4, ex4)
    ci = to_lowered_expr(mod, ex5)
    @assert ci isa Core.CodeInfo
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
function is_defined_and_owned_global(mod, name)
    Base.binding_kind(mod, name) === Base.PARTITION_KIND_GLOBAL
end

# "Reserve" a binding: create the binding if it doesn't exist but do not assign
# to it.
function reserve_module_binding(mod, name)
    # TODO: Fix the race condition here: We should really hold the Module's
    # binding lock during this test-and-set type operation. But the binding
    # lock is only accessible from C. See also the C code in
    # `fl_module_unique_name`.
    if _get_module_binding(mod, name; create=false) === nothing
        _get_module_binding(mod, name; create=true) !== nothing
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
function methods_in_world(func, arg_sig, world)
    Base._methods(func, arg_sig, -1, world)
end
