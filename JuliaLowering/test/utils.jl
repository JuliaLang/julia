using Test

using JuliaLowering
using JuliaSyntax
import FileWatching

# The following are for docstrings testing. We need to load the REPL module
# here for `Base.@doc` lookup to work at all. Yes this does seem really,
# really, REALLY messed up.
using Markdown
import REPL

using JuliaSyntax: sourcetext, set_numeric_flags

using JuliaLowering:
    SyntaxGraph, newnode!, ensure_attributes!,
    Kind, SourceRef, SyntaxTree, NodeId,
    makenode, makeleaf, setattr!, sethead!,
    is_leaf, numchildren, children,
    @ast, flattened_provenance, showprov, LoweringError, MacroExpansionError,
    syntax_graph, Bindings, ScopeLayer

function _ast_test_graph()
    graph = SyntaxGraph()
    ensure_attributes!(graph,
                       kind=Kind, syntax_flags=UInt16,
                       source=Union{SourceRef,NodeId,Tuple,LineNumberNode},
                       var_id=Int, value=Any, name_val=String, is_toplevel_thunk=Bool)
end

function _source_node(graph, src)
    id = newnode!(graph)
    sethead!(graph, id, K"None")
    setattr!(graph, id, source=src)
    SyntaxTree(graph, id)
end

macro ast_(tree)
    # TODO: Implement this in terms of new-style macros.
    quote
        graph = _ast_test_graph()
        srcref = _source_node(graph, $(QuoteNode(__source__)))
        @ast graph srcref $tree
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

function uncomment_description(desc)
    replace(desc, r"^# ?"m=>"")
end

function comment_description(desc)
    replace(desc, r"^"m=>"# ")
end

function match_ir_test_case(case_str)
    m = match(r"(^#(?:.|\n)*?)^([^#](?:.|\n)*)"m, strip(case_str))
    if isnothing(m)
        error("Malformatted IR test case:\n$(repr(case_str))")
    end
    description = uncomment_description(m[1])
    inout = split(m[2], r"#----*")
    input, output = length(inout) == 2 ? inout          :
                    length(inout) == 1 ? (inout[1], "") :
                    error("Too many sections in IR test case")
    expect_error = startswith(description, "Error")
    is_broken = startswith(description, "FIXME")
    (; expect_error=expect_error, is_broken=is_broken,
     description=strip(description),
     input=strip(input), output=strip(output))
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
     [match_ir_test_case(s) for s in split(cases_str, r"######*") if strip(s) != ""])
end

function setup_ir_test_module(preamble)
    test_mod = Module(:TestMod)
    JuliaLowering.include_string(test_mod, preamble)
    Base.eval(test_mod, :(const var"@ast_" = $(var"@ast_")))
    test_mod
end

function format_ir_for_test(mod, description, input, expect_error=false, is_broken=false)
    ex = parsestmt(SyntaxTree, input)
    try
        if kind(ex) == K"macrocall" && kind(ex[1]) == K"MacroName" && ex[1].name_val == "@ast_"
            # Total hack, until @ast_ can be implemented in terms of new-style
            # macros.
            ex = JuliaLowering.eval(mod, Expr(ex))
        end
        x = JuliaLowering.lower(mod, ex)
        if expect_error
            error("Expected a lowering error in test case \"$description\"")
        end
        ir = strip(sprint(JuliaLowering.print_ir, x))
        return replace(ir, string(mod)=>"TestMod")
    catch exc
        if exc isa InterruptException
            rethrow()
        elseif expect_error && (exc isa LoweringError)
            return sprint(io->Base.showerror(io, exc, show_detail=false))
        elseif expect_error && (exc isa MacroExpansionError)
            return sprint(io->Base.showerror(io, exc))
        elseif is_broken
            return sprint(io->Base.showerror(io, exc))
        else
            throw("Error in test case \"$description\"")
        end
    end
end

function test_ir_cases(filename::AbstractString)
    preamble, cases = read_ir_test_cases(filename)
    test_mod = setup_ir_test_module(preamble)
    for (expect_error, is_broken, description, input, ref) in cases
        if is_broken
            continue
        end
        output = format_ir_for_test(test_mod, description, input, expect_error)
        @testset "$description" begin
            if output != ref
                # Do additional error dumping, as @test will not format errors in a nice way
                @error "Test \"$description\" failed" output=Text(output) ref=Text(ref)
            end
            @test output == ref
        end
    end
end

"""
Update all IR test cases in `filename` when the IR format has changed.

When `pattern` is supplied, update only those tests where
`occursin(pattern, description)` is true.
"""
function refresh_ir_test_cases(filename, pattern=nothing)
    preamble, cases = read_ir_test_cases(filename)
    test_mod = setup_ir_test_module(preamble)
    io = IOBuffer()
    if !isempty(preamble)
        println(io, preamble, "\n")
        println(io, "#*******************************************************************************")
    end
    for (expect_error, is_broken, description, input, ref) in cases
        if isnothing(pattern) || occursin(pattern, description)
            ir = format_ir_for_test(test_mod, description, input, expect_error, is_broken)
            if rstrip(ir) != ref
                @info "Refreshing test case $(repr(description)) in $filename"
            end
        else
            ir = ref
        end
        println(io,
            """
            ########################################
            $(comment_description(description))
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

function watch_ir_tests(dir, delay=0.5)
    dir = abspath(dir)
    while true
        (name, event) = FileWatching.watch_folder(dir)
        if endswith(name, "_ir.jl") && (event.changed || event.renamed)
            FileWatching.unwatch_folder(dir)
            sleep(delay)
            try
                refresh_ir_test_cases(joinpath(dir, name))
            catch
                @error "Error refreshing test case" exception=current_exceptions()
            end
        end
    end
end

# See Julia Base tests in "test/docs.jl"
function docstrings_equal(d1, d2; debug=true)
    io1 = IOBuffer()
    io2 = IOBuffer()
    show(io1, MIME"text/markdown"(), d1)
    show(io2, MIME"text/markdown"(), d2)
    s1 = String(take!(io1))
    s2 = String(take!(io2))
    if debug && s1 != s2
        print(s1)
        println("--------------------------------------------------------------------------------")
        print(s2)
        println("================================================================================")
    end
    return s1 == s2
end
docstrings_equal(d1::Docs.DocStr, d2) = docstrings_equal(Docs.parsedoc(d1), d2)

