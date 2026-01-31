attrsummary(name, value) = string(name)
attrsummary(name, value::Number) = "$name=$value"

function _value_string(ex)
    k = kind(ex)
    str = k == K"Identifier" || JuliaSyntax.is_operator(k) ? ex.name_val :
          k == K"Placeholder" ? ex.name_val           :
          k == K"SSAValue"    ? "%"                   :
          k == K"BindingId"   ? "#"                   :
          k == K"label"       ? "label"               :
          k == K"core"        ? "core.$(ex.name_val)" :
          k == K"top"         ? "top.$(ex.name_val)"  :
          k == K"Symbol"      ? ":$(ex.name_val)" :
          k == K"globalref"   ? "$(ex.mod).$(ex.name_val)" :
          k == K"slot"        ? "slot" :
          k == K"latestworld" ? "latestworld" :
          k == K"static_parameter" ? "static_parameter" :
          k == K"symboliclabel" ? "label:$(ex.name_val)" :
          k == K"symbolicgoto" ? "goto:$(ex.name_val)" :
          k == K"SourceLocation" ?
              "SourceLocation:$(JuliaSyntax.filename(ex)):$(join(source_location(ex), ':'))" :
          k == K"Value" && ex.value isa SourceRef ?
              "SourceRef:$(JuliaSyntax.filename(ex)):$(join(source_location(ex), ':'))" :
          repr(get(ex, :value, nothing))
    id = get(ex, :var_id, nothing)
    if isnothing(id)
        id = get(ex, :id, nothing)
    end
    if !isnothing(id)
        idstr = subscript_str(id)
        str = "$(str)$idstr"
    end
    if k == K"slot" || k == K"BindingId"
        p = provenance(ex)[1]
        while p isa SyntaxTree
            if kind(p) == K"Identifier"
                str = "$(str)/$(p.name_val)"
                break
            end
            p = provenance(p)[1]
        end
    end
    return str
end

function _show_syntax_tree(io, ex, indent, show_kinds)
    val = get(ex, :value, nothing)
    nodestr = !is_leaf(ex) ? "[$(untokenize(head(ex)))]" : _value_string(ex)

    treestr = rpad(string(indent, nodestr), 40)
    if show_kinds && is_leaf(ex)
        treestr = treestr*" :: "*string(kind(ex))
    end

    std_attrs = Set([:name_val,:value,:kind,:syntax_flags,:source,:var_id])
    attrstr = join([attrsummary(n, getproperty(ex, n))
                    for n in JuliaSyntax.attrnames(ex) if n ∉ std_attrs], ",")
    treestr = string(rpad(treestr, 60), " │ $attrstr")

    println(io, treestr)
    if !is_leaf(ex)
        new_indent = indent*"  "
        for n in children(ex)
            _show_syntax_tree(io, n, new_indent, show_kinds)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", ex::SyntaxTree, show_kinds=true)
    anames = join(string.(JuliaSyntax.attrnames(syntax_graph(ex))), ",")
    println(io, "SyntaxTree with attributes $anames")
    _show_syntax_tree(io, ex, "", show_kinds)
end
function _show_syntax_tree_sexpr(io, ex)
    if is_leaf(ex)
        if JuliaSyntax.is_error(ex)
            print(io, "(", untokenize(head(ex)), ")")
        else
            print(io, _value_string(ex))
        end
    else
        print(io, "(", untokenize(head(ex)))
        first = true
        for n in children(ex)
            print(io, ' ')
            _show_syntax_tree_sexpr(io, n)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxTree)
    _show_syntax_tree_sexpr(io, node)
end

function Base.show(io::IO, node::SyntaxTree)
    _show_syntax_tree_sexpr(io, node)
end

#-------------------------------------------------------------------------------
# Error handling

TODO(msg::AbstractString) = throw(ErrorException("Lowering TODO: $msg"))
TODO(ex::SyntaxTree, msg="") = throw(LoweringError(ex, "Lowering TODO: $msg"))

"""
An error generated while lowering user code `ex` (flisp: `Expr(:error, msg)`).
For errors in lowering itself, use `@assert`.
"""
struct LoweringError <: Exception
    ex::SyntaxTree
    msg::String
end

function Base.showerror(io::IO, exc::LoweringError; show_detail=true)
    print(io, "LoweringError:\n")
    src = sourceref(exc.ex)
    highlight(io, src; note=exc.msg)

    if show_detail
        print(io, "\n\nDetailed provenance:\n")
        showprov(io, exc.ex, tree=true)
    end
end

function _show_provtree(io::IO, ex::SyntaxTree, indent)
    print(io, ex, "\n")
    prov = provenance(ex)
    for (i, e) in enumerate(prov)
        islast = i == length(prov)
        printstyled(io, "$indent$(islast ? "└─ " : "├─ ")", color=:light_black)
        inner_indent = indent * (islast ? "   " : "│  ")
        _show_provtree(io, e, inner_indent)
    end
end

function _show_provtree(io::IO, prov, indent)
    fn = filename(prov)
    line, _ = source_location(prov)
    printstyled(io, "@ $fn:$line\n", color=:light_black)
end

function showprov(io::IO, exs::AbstractVector;
                  note=nothing, include_location::Bool=true, highlight_kwargs...)
    for (i,ex) in enumerate(Iterators.reverse(exs))
        sr = sourceref(ex)
        if i > 1
            print(io, "\n\n")
        end
        k = kind(ex)
        ex_note = !isnothing(note) ? note :
            i > 1 && k == K"macrocall"  ? "in macro expansion" :
            i > 1 && k == K"$"          ? "interpolated here"  :
            "in source"
        highlight(io, sr; note=ex_note, highlight_kwargs...)

        if include_location
            line, _ = source_location(sr)
            locstr = "$(filename(sr)):$line"
            JuliaSyntax._printstyled(io, "\n# @ $locstr", fgcolor=:light_black)
        end
    end
end

function showprov(io::IO, ex::SyntaxTree; tree::Bool=false, showprov_kwargs...)
    if tree
        _show_provtree(io, ex, "")
    else
        showprov(io, flattened_provenance(ex); showprov_kwargs...)
    end
end

function showprov(x; kws...)
    showprov(stdout, x; kws...)
end

function subscript_str(i)
     replace(string(i),
             "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
             "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
end

function _deref_ssa(stmts, ex)
    while kind(ex) == K"SSAValue"
        ex = stmts[ex.var_id]
    end
    ex
end

function _find_method_lambda(ex, name)
    @assert kind(ex) == K"code_info"
    # Heuristic search through outer thunk for the method in question.
    method_found = false
    stmts = children(ex[1])
    for e in stmts
        if kind(e) == K"method" && numchildren(e) >= 2
            sig = _deref_ssa(stmts, e[2])
            @assert kind(sig) == K"call"
            arg_types = _deref_ssa(stmts, sig[2])
            @assert kind(arg_types) == K"call"
            self_type = _deref_ssa(stmts, arg_types[2])
            if kind(self_type) == K"globalref" && occursin(name, self_type.name_val)
                return e[3]
            end
        end
    end
end

function print_ir(io::IO, ex, method_filter=nothing)
    @assert kind(ex) == K"code_info"
    if !isnothing(method_filter)
        filtered = _find_method_lambda(ex, method_filter)
        if isnothing(filtered)
            @warn "Method not found with method filter $method_filter"
        else
            ex = filtered
        end
    end
    _print_ir(io, ex, "")
end

# TODO: JuliaLowering-the-module should always print the same way, ignoring parent modules
function _print_ir(io::IO, ex, indent)
    added_indent = "    "
    @assert (kind(ex) == K"lambda" || kind(ex) == K"code_info") && kind(ex[1]) == K"block"
    if !ex.is_toplevel_thunk && kind(ex) == K"code_info"
        slots = ex.slots
        print(io, indent, "slots: [")
        for (i,slot) in enumerate(slots)
            print(io, "slot$(subscript_str(i))/$(slot.name)")
            flags = String[]
            slot.is_nospecialize   && push!(flags, "nospecialize")
            !slot.is_read          && push!(flags, "!read")
            slot.is_single_assign  && push!(flags, "single_assign")
            slot.is_maybe_undef    && push!(flags, "maybe_undef")
            slot.is_called         && push!(flags, "called")
            if !isempty(flags)
                print(io, "($(join(flags, ",")))")
            end
            if i < length(slots)
                print(io, " ")
            end
        end
        println(io, "]")
    end
    stmts = children(ex[1])
    for (i, e) in enumerate(stmts)
        lno = rpad(i, 3)
        if kind(e) == K"method" && numchildren(e) == 3
            print(io, indent, lno, " --- method ", string(e[1]), " ", string(e[2]))
            if kind(e[3]) == K"lambda" || kind(e[3]) == K"code_info"
                println(io)
                _print_ir(io, e[3], indent*added_indent)
            else
                println(io, " ", string(e[3]))
            end
        elseif kind(e) == K"opaque_closure_method"
            @assert numchildren(e) == 5
            print(io, indent, lno, " --- opaque_closure_method ")
            for i=1:4
                print(io, " ", e[i])
            end
            println(io)
            _print_ir(io, e[5], indent*added_indent)
        elseif kind(e) == K"code_info"
            println(io, indent, lno, " --- ", e.is_toplevel_thunk ? "thunk" : "code_info")
            _print_ir(io, e, indent*added_indent)
        else
            code = string(e)
            println(io, indent, lno, " ", code)
        end
    end
end

# Wrap a function body in Base.Compiler.@zone for profiling
if isdefined(Base.Compiler, Symbol("@zone"))
    macro fzone(str, f)
        @assert f isa Expr && f.head === :function && length(f.args) === 2 && str isa String
        esc(Expr(:function, f.args[1],
                 # Use source of our caller, not of this macro.
                 Expr(:macrocall, :(Base.Compiler.var"@zone"), __source__, str, f.args[2])))
    end
else
    macro fzone(str, f)
        esc(f)
    end
end

#-------------------------------------------------------------------------------
# @SyntaxTree(::Expr)

function _find_SyntaxTree_macro(ex, line)
    @assert !is_leaf(ex)
    for c in children(ex)
        rng = byte_range(c)
        firstline = JuliaSyntax.source_line(sourcefile(c), first(rng))
        lastline = JuliaSyntax.source_line(sourcefile(c), last(rng))
        if line < firstline || lastline < line
            continue
        end
        # We're in the line range. Either
        if firstline == line && kind(c) == K"macrocall" && begin
                    name = c[1]
                    if kind(name) == K"."
                        name = name[2]
                    end
                    @assert kind(name) == K"Identifier"
                    name.name_val == "@SyntaxTree"
                end
            # We find the node we're looking for. NB: Currently assuming a max
            # of one @SyntaxTree invocation per line. Though we could relax
            # this with more heuristic matching of the Expr-AST...
            @assert numchildren(c) == 2
            return c[2]
        elseif !is_leaf(c)
            # Recurse
            ex1 = _find_SyntaxTree_macro(c, line)
            if !isnothing(ex1)
                return ex1
            end
        end
    end
    return nothing # Will get here if multiple children are on the same line.
end

# Translate JuliaLowering hygiene to esc() for use in @SyntaxTree
function _scope_layer_1_to_esc!(ex)
    if ex isa Expr
        if ex.head == :scope_layer
            @assert ex.args[2] === 1
            return esc(_scope_layer_1_to_esc!(ex.args[1]))
        else
            map!(_scope_layer_1_to_esc!, ex.args, ex.args)
            return ex
        end
    else
        return ex
    end
end

"""
Macro to construct quoted SyntaxTree literals (instead of quoted Expr literals)
in normal Julia source code.

Example:

```julia
tree1 = @SyntaxTree :(some_unique_identifier)
tree2 = @SyntaxTree quote
    x = 1
    \$tree1 = x
end
```
"""
macro SyntaxTree(ex_old)
    # The implementation here is hilarious and arguably very janky: we
    # 1. Briefly check but throw away the Expr-AST
    if !(Meta.isexpr(ex_old, :quote) || ex_old isa QuoteNode)
        throw(ArgumentError("@SyntaxTree expects a `quote` block or `:`-quoted expression"))
    end
    # 2. Re-parse the current source file as SyntaxTree instead
    fname = isnothing(__source__.file) ? error("No current file") : String(__source__.file)
    if occursin(r"REPL\[\d+\]", fname)
        # Assume we should look at last history entry in REPL
        try
            # Wow digging in like this is an awful hack but `@SyntaxTree` is
            # already a hack so let's go for it I guess 😆
            text = Base.active_repl.mistate.interface.modes[1].hist.history[end]
            if !occursin("@SyntaxTree", text)
                error("Text not found in last REPL history line")
            end
        catch
            error("Text not found in REPL history")
        end
    else
        text = read(fname, String)
    end
    full_ex = parseall(SyntaxTree, text)
    # 3. Using the current file and line number, dig into the re-parsed tree and
    # discover the piece of AST which should be returned.
    ex = _find_SyntaxTree_macro(full_ex, __source__.line)
    isnothing(ex) && error("_find_SyntaxTree_macro failed")
    # 4. Do the first step of JuliaLowering's syntax lowering to get
    # syntax interpolations to work
    _, ex1 = expand_forms_1(__module__, ex, false, Base.tls_world_age())
    @assert kind(ex1) == K"call" && ex1[1].value == interpolate_ast
    Expr(:call, :interpolate_ast, SyntaxTree, ex1[3][1],
         map(e->_scope_layer_1_to_esc!(Expr(e)), ex1[4:end])...)
end
