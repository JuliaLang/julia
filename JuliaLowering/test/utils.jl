# Shared testing code which should be included before running individual test files.
using Test

using JuliaLowering
using JuliaSyntax

import FileWatching

# The following are for docstrings testing. We need to load the REPL module
# here for `Base.@doc` lookup to work at all. Yes this does seem really,
# really, REALLY messed up.
using Markdown
import REPL

using .JuliaSyntax: sourcetext, set_numeric_flags

using .JuliaLowering:
    SyntaxGraph, newnode!, ensure_attributes!,
    Kind, SourceRef, SyntaxTree, NodeId,
    makenode, makeleaf, setattr!, sethead!,
    is_leaf, numchildren, children,
    @ast, flattened_provenance, showprov, LoweringError, MacroExpansionError,
    syntax_graph, Bindings, ScopeLayer, mapchildren

function _ast_test_graph()
    graph = SyntaxGraph()
    ensure_attributes!(graph,
                       kind=Kind, syntax_flags=UInt16,
                       source=Union{SourceRef,NodeId,Tuple,LineNumberNode},
                       var_id=Int, value=Any, name_val=String, is_toplevel_thunk=Bool,
                       toplevel_pure=Bool)
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

function ≈(ex1, ex2)
    if kind(ex1) != kind(ex2) || is_leaf(ex1) != is_leaf(ex2)
        return false
    end
    if is_leaf(ex1)
        return get(ex1, :value,    nothing) == get(ex2, :value,    nothing) &&
               get(ex1, :name_val, nothing) == get(ex2, :name_val, nothing)
    else
        if numchildren(ex1) != numchildren(ex2)
            return false
        end
        return all(c1 ≈ c2 for (c1,c2) in zip(children(ex1), children(ex2)))
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
test case comparisons with the `≈` function.
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
    method_filter = begin
        mf = match(r"\[method_filter: *(.*)\]", description)
        isnothing(mf) ? nothing : strip(mf[1])
    end
    (; expect_error=expect_error, is_broken=is_broken,
     description=strip(description),
     method_filter=method_filter,
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
    Base.eval(test_mod, :(const JuliaLowering = $JuliaLowering))
    Base.eval(test_mod, :(const var"@ast_" = $(var"@ast_")))
    JuliaLowering.include_string(test_mod, preamble)
    test_mod
end

function format_ir_for_test(mod, case)
    ex = parsestmt(SyntaxTree, case.input)
    try
        if kind(ex) == K"macrocall" && kind(ex[1]) == K"macro_name" && ex[1][1].name_val == "ast_"
            # Total hack, until @ast_ can be implemented in terms of new-style
            # macros.
            ex = Base.eval(mod, Expr(ex))
        end
        x = JuliaLowering.lower(mod, ex)
        if case.expect_error
            error("Expected a lowering error in test case \"$(case.description)\"")
        end
        ir = strip(sprint(JuliaLowering.print_ir, x, case.method_filter))
        return replace(ir, string(mod)=>"TestMod")
    catch exc
        if exc isa InterruptException
            rethrow()
        elseif case.expect_error && (exc isa LoweringError)
            return sprint(io->Base.showerror(io, exc, show_detail=false))
        elseif case.expect_error && (exc isa MacroExpansionError)
            return sprint(io->Base.showerror(io, exc))
        elseif case.is_broken
            return sprint(io->Base.showerror(io, exc))
        else
            throw("Error in test case \"$(case.description)\"")
        end
    end
end

function test_ir_cases(filename::AbstractString)
    preamble, cases = read_ir_test_cases(filename)
    test_mod = setup_ir_test_module(preamble)
    for case in cases
        if case.is_broken
            continue
        end
        output = format_ir_for_test(test_mod, case)
        @testset "$(case.description)" begin
            if output != case.output
                # Do additional error dumping, as @test will not format errors in a nice way
                @error "Test \"$(case.description)\" failed" output=Text(output) ref=Text(case.output)
            end
            @test output == case.output
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
    for case in cases
        if isnothing(pattern) || occursin(pattern, case.description)
            ir = format_ir_for_test(test_mod, case)
            if rstrip(ir) != case.output
                @info "Refreshing test case $(repr(case.description)) in $filename"
            end
        else
            ir = case.output
        end
        println(io,
            """
            ########################################
            $(comment_description(case.description))
            $(strip(case.input))
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

function lower_str(mod::Module, s::AbstractString)
    ex = parsestmt(JuliaLowering.SyntaxTree, s)
    return JuliaLowering.to_lowered_expr(JuliaLowering.lower(mod, ex))
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

#-------------------------------------------------------------------------------
# Tools for test case reduction

function block_reduction_1(is_lowering_error::Function, orig_ex::ST, ex::ST,
                           curr_path = Int[]) where {ST <: SyntaxTree}
    if !is_leaf(ex)
        if kind(ex) == K"block"
            for i in 1:numchildren(ex)
                trial_ex = delete_block_child(orig_ex, orig_ex, curr_path, i)
                if is_lowering_error(trial_ex)
                    # @info "Reduced expression" curr_path i
                    return trial_ex
                end
            end
        end
        for (i,e) in enumerate(children(ex))
            push!(curr_path, i)
            res = block_reduction_1(is_lowering_error, orig_ex, e, curr_path)
            if !isnothing(res)
                return res
            end
            pop!(curr_path)
        end
    end
    return nothing
end

# Find children of all `K"block"`s in an expression and try deleting them while
# preserving the invariant `is_lowering_error(reduced) == true`.
function block_reduction(is_lowering_error, ex)
    reduced = ex
    was_reduced = false
    while true
        r = block_reduction_1(is_lowering_error, reduced, reduced)
        if isnothing(r)
            return (reduced, was_reduced)
        end
        reduced = r
        was_reduced = true
    end
end

function delete_block_child(ctx, ex, block_path, child_idx, depth=1)
    if depth > length(block_path)
        cs = copy(children(ex))
        deleteat!(cs, child_idx)
        @ast ctx ex [ex cs...]
    else
        j = block_path[depth]
        mapchildren(ctx, ex, j:j) do e
            delete_block_child(ctx, e, block_path, child_idx, depth+1)
        end
    end
end

function throws_lowering_exc(mod, ex)
    try
        debug_lower(mod, ex)
        return false
    catch exc
        if exc isa LoweringError
            return true
        else
            rethrow()
        end
    end
end

# Parse a file and lower the top level expression one child at a time, finding
# any top level statement that fails lowering and producing a partially reduced
# test case.
function reduce_any_failing_toplevel(mod::Module, filename::AbstractString; do_eval::Bool=false)
    text = read(filename, String)
    ex0 = parseall(SyntaxTree, text; filename)
    for ex in children(ex0)
        try
            ex_compiled = JuliaLowering.lower(mod, ex)
            ex_expr = JuliaLowering.to_lowered_expr(ex_compiled)
            if do_eval
                Base.eval(mod, ex_expr)
            end
        catch exc
            @error "Failure lowering code" ex
            if !(exc isa LoweringError)
                rethrow()
            end
            (reduced,was_reduced) = block_reduction(e->throws_lowering_exc(mod,e), ex)
            if !was_reduced
                @info "No reduction possible"
                return ex
            else
                @info "Reduced code" reduced
                return reduced
            end
        end
    end
    nothing
end
