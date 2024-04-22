function lower(mod, ex)
    ctx1, ex1 = expand_forms(ex)
    ctx2, ex2 = resolve_scopes!(ctx1, mod, ex1)
    ctx3, ex3 = linearize_ir(ctx2, ex2)
    ex3
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
        Core.ReturnNode(to_lowered_expr(mod, var_info, ex[1]))
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
        Expr(head, map(e->to_lowered_expr(mod, var_info, e), children(ex))...)
    end
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

