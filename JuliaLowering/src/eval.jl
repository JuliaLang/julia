function lower(mod::Module, ex0)
    ctx1, ex1 = expand_forms_1(  mod,  ex0)
    ctx2, ex2 = expand_forms_2(  ctx1, ex1)
    ctx3, ex3 = resolve_scopes(  ctx2, ex2)
    ctx4, ex4 = convert_closures(ctx3, ex3)
    ctx5, ex5 = linearize_ir(    ctx4, ex4)
    ex5
end

function macroexpand(mod::Module, ex)
    ctx1, ex1 = expand_forms_1(mod, ex)
    ex1
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

function _compress_debuginfo(info)
    filename, edges, codelocs = info
    edges = Core.svec(map(_compress_debuginfo, edges)...)
    codelocs = @ccall jl_compress_codelocs((-1)::Int32, codelocs::Any,
                                           div(length(codelocs),3)::Csize_t)::String
    Core.DebugInfo(Symbol(filename), nothing, edges, codelocs)
end

function ir_debug_info(ex)
    code = children(ex)

    e1 = first(flattened_provenance(ex))
    topfile = filename(e1)

    current_codelocs_stack = [(topfile, [], Vector{Int32}())]
    for i in 1:length(code)
        locstk = [(filename(e), source_location(e)[1]) for e in flattened_provenance(code[i])]
        for j in 1:max(length(locstk), length(current_codelocs_stack))
            if j > length(locstk) || (length(current_codelocs_stack) >= j &&
                                      current_codelocs_stack[j][1] != locstk[j][1])
                while length(current_codelocs_stack) >= j
                    info = pop!(current_codelocs_stack)
                    push!(last(current_codelocs_stack)[2], info)
                end
            end
            if j > length(locstk)
                break
            elseif j > length(current_codelocs_stack)
                push!(current_codelocs_stack, (locstk[j][1], [], Vector{Int32}()))
            end
        end
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
    while length(current_codelocs_stack) > 1
        info = pop!(current_codelocs_stack)
        push!(last(current_codelocs_stack)[2], info)
    end

    _compress_debuginfo(only(current_codelocs_stack))
end

# Convert SyntaxTree to the CodeInfo+Expr data stuctures understood by the
# Julia runtime
function to_code_info(ex, mod, funcname, nargs, bindings, slot_rewrites)
    input_code = children(ex)
    code = Any[to_lowered_expr(mod, bindings, ex) for ex in input_code]

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
        info = lookup_binding(bindings, id)
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

function to_lowered_expr(mod, bindings, ex)
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
        Core.ReturnNode(to_lowered_expr(mod, bindings, ex[1]))
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
        ir = to_code_info(ex[1], mod, funcname, nargs, bindings, ex.slot_rewrites)
        if ex.lambda_info.is_toplevel_thunk
            Expr(:thunk, ir)
        else
            ir
        end
    elseif k == K"Value"
        ex.value
    elseif k == K"goto"
        Core.GotoNode(ex[1].id)
    elseif k == K"gotoifnot"
        Core.GotoIfNot(to_lowered_expr(mod, bindings, ex[1]), ex[2].id)
    elseif k == K"enter"
        catch_idx = ex[1].id
        numchildren(ex) == 1 ?
            Core.EnterNode(catch_idx) :
            Core.EnterNode(catch_idx, to_lowered_expr(ex[2]))
    elseif k == K"method"
        name = ex[1]
        @chk kind(name) == K"Symbol"
        namesym = Symbol(name.name_val)
        cs = map(e->to_lowered_expr(mod, bindings, e), ex[2:end])
        Expr(:method, namesym, cs...)
    else
        # Allowed forms according to https://docs.julialang.org/en/v1/devdocs/ast/
        #
        # call invoke static_parameter `=` method struct_type abstract_type
        # primitive_type global const new splatnew isdefined
        # enter leave pop_exception inbounds boundscheck loopinfo copyast meta
        # foreigncall new_opaque_closure lambda
        head = k == K"call"   ? :call   :
               k == K"="      ? :(=)    :
               k == K"global" ? :global :
               k == K"const"  ? :const  :
               k == K"leave"  ? :leave  :
               k == K"pop_exception"  ? :pop_exception  :
               nothing
        if isnothing(head)
            TODO(ex, "Unhandled form for kind $k")
        end
        Expr(head, map(e->to_lowered_expr(mod, bindings, e), children(ex))...)
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
    expr_form = to_lowered_expr(mod, linear_ir.bindings, linear_ir)
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

