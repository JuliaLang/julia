using Test

using JuliaLowering
using JuliaSyntax

using JuliaSyntax: sourcetext

using JuliaLowering:
    SyntaxGraph, newnode!, ensure_attributes!,
    Kind, SourceRef, SyntaxTree, NodeId,
    makenode, makeleaf, setattr!, sethead!,
    is_leaf, numchildren, children,
    @ast, flattened_provenance, showprov, LoweringError,
    syntax_graph, Bindings, ScopeLayer

function _ast_test_graph()
    graph = SyntaxGraph()
    ensure_attributes!(graph,
                       kind=Kind, source=Union{SourceRef,NodeId,LineNumberNode},
                       var_id=Int, value=Any, name_val=String)
end

function _source_node(graph, src)
    id = newnode!(graph)
    sethead!(graph, id, K"None")
    setattr!(graph, id, source=src)
    SyntaxTree(graph, id)
end

macro ast_(tree)
    defs = []
    ex = JuliaLowering._expand_ast_tree(defs, :graph, :srcref, tree)
    quote
        graph = _ast_test_graph()
        srcref = _source_node(graph, $(QuoteNode(__source__)))
        $(defs...)
        $ex
    end
end

function ~(ex1, ex2)
    if kind(ex1) != kind(ex2) || is_leaf(ex1) != is_leaf(ex2)
        return false
    end
    if is_leaf(ex1)
        if numchildren(ex1) != numchildren(ex2)
            return false
        end
        return all(c1 ~ c2 for (c1,c2) in zip(children(ex1), children(ex2)))
    else
        return get(ex1, :value,    nothing) == get(ex2, :value,    nothing) &&
               get(ex1, :name_val, nothing) == get(ex2, :name_val, nothing)
    end
end


#-------------------------------------------------------------------------------
function _format_as_ast_macro(io, ex, indent)
    k = kind(ex)
    kind_str = repr(k)
    if !is_leaf(ex)
        println(io, indent, "[", kind_str)
        ind2 = indent*"    "
        for c in children(ex)
            _format_as_ast_macro(io, c, ind2)
        end
        println(io, indent, "]")
    else
        val_str = if k == K"Identifier" || k == K"core" || k == K"top"
            repr(ex.name_val)
        elseif k == K"BindingId"
            repr(ex.var_id)
        else
            repr(get(ex, :value, nothing))
        end
        println(io, indent, val_str, "::", kind_str)
    end
end

function format_as_ast_macro(io::IO, ex)
    print(io, "@ast_ ")
    _format_as_ast_macro(io, ex, "")
end

"""
    format_as_ast_macro(ex)

Format AST `ex` as a Juila source code call to the `@ast_` macro for generating
test case comparisons with the `~` function.
"""
format_as_ast_macro(ex) = format_as_ast_macro(stdout, ex)

#-------------------------------------------------------------------------------

# Test tools

function desugar(mod::Module, src::String)
    ex = parsestmt(SyntaxTree, src, filename="foo.jl")
    ctx = JuliaLowering.DesugaringContext(syntax_graph(ex), Bindings(), ScopeLayer[], mod)
    JuliaLowering.expand_forms_2(ctx, ex)
end

function match_ir_test_case(case_str)
    m = match(r"# *([^\n]*)\n((?:.|\n)*)#----*\n((?:.|\n)*)"m, strip(case_str))
    if isnothing(m)
        error("Malformatted IR test case:\n$(repr(case_str))")
    end
    (description=strip(m[1]), input=strip(m[2]), output=strip(m[3]))
end

function read_ir_test_cases(filename)
    str = read(filename, String)
    parts = split(str, r"#\*+")
    if length(parts) == 2
        preamble_str = strip(parts[1])
        cases_str = parts[2]
    else
        preamble_str = ""
        cases_str = only(parts)
    end
    (preamble_str,
     [match_ir_test_case(s) for s in split(cases_str, r"####*") if strip(s) != ""])
end

function format_ir_for_test(mod, input)
    ex = parsestmt(SyntaxTree, input)
    x = JuliaLowering.lower(mod, ex)
    ir = strip(sprint(JuliaLowering.print_ir, x))
    return replace(ir, string(mod)=>"TestMod")
end

function test_ir_cases(filename::AbstractString)
    preamble, cases = read_ir_test_cases(filename)
    test_mod = Module(:TestMod)
    Base.include_string(test_mod, preamble)
    for (description,input,ref) in cases
        output = format_ir_for_test(test_mod, input)
        @testset "$description" begin
            if output != ref
                # Do our own error dumping, as @test will 
                @error "Test \"$description\" failed" output=Text(output) ref=Text(ref)
            end
            @test output == ref
        end
    end
end

function format_ir_test_case(mod, input, description="-- Add description here --")
    ir = format_ir_for_test(mod, input)
    """
    ########################################
    # $description
    $(strip(input))
    #---------------------
    $ir
    """
end

"""
Update all IR test cases in `filename` when the IR format has changed.
"""
function refresh_ir_test_cases(filename)
    preamble, cases = read_ir_test_cases(filename)
    test_mod = Module(:TestMod)
    Base.include_string(test_mod, preamble)
    io = IOBuffer()
    if !isempty(preamble)
        println(io, preamble, "\n")
        println(io, "#*******************************************************************************")
    end
    for (description,input,ref) in cases
        ir = format_ir_for_test(test_mod, input)
        if ir != ref
            @info "Refreshing test case $(repr(description)) in $filename"
        end
        println(io,
            """
            ########################################
            # $description
            $(strip(input))
            #---------------------
            $ir
            """
        )
    end
    # Write only at the end to ensure we don't write rubbish if we crash!
    write(filename, take!(io))
    nothing
end

function refresh_all_ir_test_cases(test_dir=".")
    foreach(refresh_ir_test_cases, filter(fn->endswith(fn, "ir.jl"), readdir(test_dir, join=true)))
end
