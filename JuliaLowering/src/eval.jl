function lower(mod::Module, ex)
    ctx1, ex1 = expand_forms_1(mod, ex)
    ctx2, ex2 = expand_forms_2(ctx1, ex1)
    ctx3, ex3 = resolve_scopes!(ctx2, ex2)
    ctx4, ex4 = linearize_ir(ctx3, ex3)
    ex4
end

_CodeInfo_need_ver = v"1.12.0-DEV.512"
if VERSION < _CodeInfo_need_ver
    function _CodeInfo(args...)
        error("Constructing a CodeInfo using JuliaLowering currently requires Julia version $_CodeInfo_need_ver or greater")
    end
else
    # debuginfo changed completely as of https://github.com/JuliaLang/julia/pull/52415
    # nargs / isva was added as of       https://github.com/JuliaLang/julia/pull/54341
    # CodeInfo constructor. TODO: Should be in Core
    let
        fns = fieldnames(Core.CodeInfo)
        fts = fieldtypes(Core.CodeInfo)
        conversions = [:(convert($t, $n)) for (t,n) in zip(fts, fns)]

        expected_fns = (:code, :debuginfo, :ssavaluetypes, :ssaflags, :slotnames, :slotflags, :slottypes, :parent, :method_for_inference_limit_heuristics, :edges, :min_world, :max_world, :nargs, :propagate_inbounds, :has_fcall, :nospecializeinfer, :isva, :inlining, :constprop, :purity, :inlining_cost)
        expected_fts = (Vector{Any}, Core.DebugInfo, Any, Vector{UInt32}, Vector{Symbol}, Vector{UInt8}, Any, Any, Any, Any, UInt64, UInt64, UInt64, Bool, Bool, Bool, Bool, UInt8, UInt8, UInt16, UInt16)

        code = if fns != expected_fns || fts != expected_fts
            :(function _CodeInfo(args...)
                error("Unrecognized CodeInfo layout: Maybe version $VERSION is to new for this version of JuliaLowering?")
            end)
        else
            :(function _CodeInfo($(fns...))
                $(Expr(:new, :(Core.CodeInfo), conversions...))
            end)
        end

        eval(@__MODULE__, code)
    end
end

function ir_debug_info(ex)
    code = children(ex)
    # Record low resolution locations in debug info
    num_stmts = length(code)
    codelocs = zeros(Int32, 3*num_stmts)

    topfile = Symbol(filename(ex))
    topline,_ = source_location(ex)

    edges = Core.DebugInfo[]

    for i in 1:num_stmts
        line,_ = source_location(code[i])
        # TODO: Macro inlining stack filename(code[i])
        codelocs[3*i-2]   = line
        codelocs[3*i-1] = 0 # Index into edges
        codelocs[3*i  ] = 0 # Index into edges[linetable]
    end

    codelocs = @ccall jl_compress_codelocs(topline::Int32, codelocs::Any,
                                            num_stmts::Csize_t)::String
    edges = Core.svec(edges...)
    Core.DebugInfo(topfile, nothing, edges, codelocs)
end

# Convert SyntaxTree to the CodeInfo+Expr data stuctures understood by the
# Julia runtime
function to_code_info(ex, mod, funcname, nargs, var_info, slot_rewrites)
    input_code = children(ex)
    code = Any[to_lowered_expr(mod, var_info, ex) for ex in input_code]

    debuginfo = ir_debug_info(ex)

    # TODO: Set ssaflags based on call site annotations:
    # - @inbounds annotations
    # - call site @inline / @noinline
    # - call site @assume_effects
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

    # TODO: Set true for @propagate_inbounds
    propagate_inbounds  = false
    # TODO: Set true if there's a foreigncall
    has_fcall           = false
    # TODO: Set for @nospecializeinfer
    nospecializeinfer   = false
    # TODO: Set based on @inline -> 0x01 or @noinline -> 0x02
    inlining            = 0x00
    # TODO: Set based on @constprop :aggressive -> 0x01 or @constprop :none -> 0x02
    constprop           = 0x00
    # TODO: Set based on Base.@assume_effects
    purity              = 0x0000

    # The following CodeInfo fields always get their default values for
    # uninferred code.
    ssavaluetypes      = length(code) # Why does the runtime code do this?
    slottypes          = nothing
    parent             = nothing
    method_for_inference_limit_heuristics = nothing
    edges               = nothing
    min_world           = Csize_t(1)
    max_world           = typemax(Csize_t)
    isva                = false
    inlining_cost       = 0xffff

    _CodeInfo(
        code,
        debuginfo,
        ssavaluetypes,
        ssaflags,
        slotnames,
        slotflags,
        slottypes,
        parent,
        method_for_inference_limit_heuristics,
        edges,
        min_world,
        max_world,
        nargs,
        propagate_inbounds,
        has_fcall,
        nospecializeinfer,
        isva,
        inlining,
        constprop,
        purity,
        inlining_cost
    )
end

function to_lowered_expr(mod, var_info, ex)
    k = kind(ex)
    if is_literal(k)
        ex.value
    elseif k == K"core"
        GlobalRef(Core, Symbol(ex.name_val))
    elseif k == K"top"
        GlobalRef(Base, Symbol(ex.name_val))
    elseif k == K"globalref"
        if mod === ex.mod
            # Implicitly refers to name in parent module.
            Symbol(ex.name_val)
        else
            GlobalRef(ex.mod, Symbol(ex.name_val))
        end
    elseif k == K"Identifier"
        # Implicitly refers to name in parent module
        # TODO: Should we even have plain identifiers at this point or should
        # they all effectively be resolved into GlobalRef earlier?
        Symbol(ex.name_val)
    elseif k == K"Symbol"
        QuoteNode(Symbol(ex.name_val))
    elseif k == K"slot"
        Core.SlotNumber(ex.var_id)
    elseif k == K"SSAValue"
        Core.SSAValue(ex.var_id)
    elseif k == K"return"
        Core.ReturnNode(to_lowered_expr(mod, var_info, ex[1]))
    elseif is_quoted(k)
        if k == K"inert"
            ex[1]
        else
            TODO(ex, "Convert SyntaxTree to Expr")
        end
    elseif k == K"lambda"
        funcname = ex.lambda_info.is_toplevel_thunk ?
            "top-level scope" :
            "none"              # FIXME
        nargs = length(ex.lambda_info.args)
        ir = to_code_info(ex[1], mod, funcname, nargs, var_info, ex.slot_rewrites)
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
        Expr(head, map(e->to_lowered_expr(mod, var_info, e), children(ex))...)
    end
end

#-------------------------------------------------------------------------------
# Runtime support functions called by lowering
# TODO: Move to runtime.jl

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
function _interpolated_value(ctx, srcref, x)
    if x isa SyntaxTree
        x.graph === ctx.graph ? x : copy_ast(ctx, x)
    else
        makeleaf(ctx, srcref, K"Value", x)
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
                srcref = numchildren(e) == 1 ? e[1] : e[i]
                push!(expanded_children, _interpolated_value(ctx, srcref, v))
            end
        else
            push!(expanded_children, _interpolate_ast(ctx, e, inner_depth))
        end
    end

    makenode(ctx, ex, head(ex), expanded_children)
end

function interpolate_ast(ex, values...)
    if kind(ex) == K"$"
        TODO(ex, "\$ in interpolate_ast")
    end
    # Construct graph for interpolation context. We inherit this from the macro
    # context where possible by detecting it using __macro_ctx__. This feels
    # hacky though.
    #
    # Perhaps we should use a ScopedValue for this instead or get it from
    # the macro __context__? Nothing feels great here.
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
        graph = SyntaxGraph()
        ensure_attributes!(graph, kind=Kind, syntax_flags=UInt16, source=SourceAttrType,
                           value=Any, name_val=String, scope_layer=LayerId)
    end
    ctx = InterpolationContext(graph, values, Ref(1))
    # We must copy the AST into our context to use it as the source reference
    # of generated expressions.
    ex1 = copy_ast(ctx, ex)
    _interpolate_ast(ctx, ex1, 0)
end


# Produce node corresponding to `srcref` when there was an interpolation among
# `children`
function interpolate_node(ctx::InterpolationContext, srcref, children...)
    makenode(ctx, sourceref(srcref), head(srcref), children...)
end

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

function bind_docs!(f::Function, docstr, method_metadata)
    mod = parentmodule(f)
    bind = Base.Docs.Binding(mod, nameof(f))
    full_sig = method_metadata[1]
    arg_sig = Tuple{full_sig[2:end]...}
    linenum = method_metadata[3]
    metadata = Dict{Symbol, Any}(
        :linenumber => linenum.line,
        :module => mod,
    )
    if !isnothing(linenum.file)
        push!(metadata, :path => string(linenum.file))
    end
    Docs.doc!(mod, bind, Base.Docs.docstr(docstr, metadata), arg_sig)
end

#-------------------------------------------------------------------------------
# Our version of eval takes our own data structures
function Core.eval(mod::Module, ex::SyntaxTree)
    k = kind(ex)
    if k == K"toplevel"
        x = nothing
        for e in children(ex)
            x = eval(mod, e)
        end
        return x
    end
    linear_ir = lower(mod, ex)
    expr_form = to_lowered_expr(mod, linear_ir.var_info, linear_ir)
    eval(mod, expr_form)
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
function include_string(mod::Module, code::AbstractString, filename::AbstractString="string")
    eval(mod, parseall(SyntaxTree, code; filename=filename))
end

