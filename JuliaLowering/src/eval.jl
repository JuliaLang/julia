function lower(mod::Module, ex)
    ctx1, ex1 = expand_forms_1(mod, ex)
    ctx2, ex2 = expand_forms_2(ctx1, ex1)
    ctx3, ex3 = resolve_scopes!(ctx2, ex2)
    ctx4, ex4 = linearize_ir(ctx3, ex3)
    ex4
end

# CodeInfo constructor. TODO: Should be in Core?
function _CodeInfo(code,
         codelocs,
         ssavaluetypes,
         ssaflags,
         method_for_inference_limit_heuristics,
         linetable,
         slotnames,
         slotflags,
         slottypes,
         rettype,
         parent,
         edges,
         min_world,
         max_world,
         inferred,
         propagate_inbounds,
         has_fcall,
         nospecializeinfer,
         inlining,
         constprop,
         purity,
         inlining_cost)
    @eval $(Expr(:new, :(Core.CodeInfo),
           convert(Vector{Any}, code),
           convert(Vector{Int32}, codelocs),
           convert(Any, ssavaluetypes),
           convert(Vector{UInt32}, ssaflags),
           convert(Any, method_for_inference_limit_heuristics),
           convert(Any, linetable),
           convert(Vector{Symbol}, slotnames),
           convert(Vector{UInt8}, slotflags),
           convert(Any, slottypes),
           convert(Any, rettype),
           convert(Any, parent),
           convert(Any, edges),
           convert(UInt64, min_world),
           convert(UInt64, max_world),
           convert(Bool, inferred),
           convert(Bool, propagate_inbounds),
           convert(Bool, has_fcall),
           convert(Bool, nospecializeinfer),
           convert(UInt8, inlining),
           convert(UInt8, constprop),
           convert(UInt16, purity),
           convert(UInt16, inlining_cost)))
end

# Convert SyntaxTree to the CodeInfo+Expr data stuctures understood by the
# Julia runtime
function to_code_info(ex, mod, funcname, var_info, slot_rewrites)
    input_code = children(ex)
    # Convert code to Expr and record low res locations in table
    num_stmts = length(input_code)
    code = Vector{Any}(undef, num_stmts)
    codelocs = Vector{Int32}(undef, num_stmts)
    linetable_map = Dict{Tuple{Int,String}, Int32}()
    linetable = Any[]
    for i in 1:length(code)
        code[i] = to_lowered_expr(mod, var_info, input_code[i])
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
        Expr(head, map(e->to_lowered_expr(mod, var_info, e), children(ex))...)
    end
end

#-------------------------------------------------------------------------------
# Runtime support functions called by lowering

struct InterpolationContext{Graph} <: AbstractLoweringContext
    graph::Graph
end

function InterpolationContext()
    graph = SyntaxGraph()
    ensure_attributes!(graph, kind=Kind, syntax_flags=UInt16, source=SourceAttrType,
                       value=Any, name_val=String)
    InterpolationContext(freeze_attrs(graph))
end

# Produce interpolated node for `$x` syntax
function interpolate_value(ctx, srcref, x)
    if x isa SyntaxTree
        if x.graph === ctx.graph
            x
        else
            copy_ast(ctx, x)
        end
    else
        makeleaf(ctx, sourceref(srcref), K"Value", x)
    end
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

