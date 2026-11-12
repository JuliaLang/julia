# Shared testing code which should be included before running individual test files.
using Test

using JuliaLowering
using JuliaSyntax

const JS = JuliaSyntax
const JL = JuliaLowering

# The following are for docstrings testing. We need to load the REPL module
# here for `Base.@doc` lookup to work at all. Yes this does seem really,
# really, REALLY messed up.
using Markdown
import REPL

using Base: OLDEST_EDITION,
    SyntaxContext, adopt_scope, remove_scope, syntax_module, fill_context,
    assert_syntax, @mknode

using .JuliaSyntax: sourcetext

using .JuliaLowering: JL_OLD_EDITION, JL_NEW_EDITION,
    @ast, LoweringError, MacroExpansionError, SyntaxTree,
    children, flattened_provenance, head, is_leaf, numchildren,
    syntax_name, syntax_id

function Base.:≈(ex1::SyntaxTree, ex2::SyntaxTree)
    if head(ex1) != head(ex2) || is_leaf(ex1) != is_leaf(ex2)
        return false
    end
    if is_leaf(ex1)
        return ex1.value == ex2.value
    else
        if numchildren(ex1) != numchildren(ex2)
            return false
        end
        return all(c1 ≈ c2 for (c1,c2) in zip(children(ex1), children(ex2)))
    end
end

function _source_node(src)
    SyntaxTree(:tombstone, nothing, nothing, src,
               SyntaxContext(JuliaLowering, JL_NEW_EDITION))
end

macro ast_(tree)
    quote
        srcref = _source_node($(QuoteNode(__source__)))
        @ast _ srcref $tree
    end
end

function uncomment_description(desc)
    replace(desc, r"^# ?"m=>"")
end

function comment_description(desc)
    lines = replace(split(desc, '\n')) do line
        strip("# " * line)
    end
    join(lines, '\n')
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
    Base.set_syntax_version(test_mod, JL_NEW_EDITION)
    Base.eval(test_mod, :(const JuliaLowering = $JuliaLowering))
    Base.eval(test_mod, :(const var"@ast_" = $(var"@ast_")))
    JuliaLowering.include_string(test_mod, preamble)
    test_mod
end

function format_ir_for_test(mod, case)
    @assert !case.is_broken
    ex = parsestmt(SyntaxTree, case.input; version=VersionNumber(JL_NEW_EDITION))
    try
        if (head(ex) == :macrocall && head(ex[1]) == :identifier &&
            syntax_name(ex[1]) == "@ast_")
            # Total hack, until @ast_ can be implemented in terms of new-style
            # macros.
            ex = Base.eval(mod, JuliaLowering.est_to_expr(ex))
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
        if !case.is_broken && (isnothing(pattern) || occursin(pattern, case.description))
            ir = format_ir_for_test(test_mod, case)
            if rstrip(ir) != case.output
                @info "Refreshing test case $(repr(case.description)) in $filename"
            end
        else
            ir = case.output
        end
        (case == cases[end] ? print : println)(io,
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

function lower_str(mod::Module, s::AbstractString)
    ex = parsestmt(SyntaxTree, s; version=VersionNumber(JL_NEW_EDITION))
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

function expr_structure_eq(e1,e2)
    @nospecialize
    typeof(e1) == typeof(e2) || return false
    e1 isa Expr || return e1 == e2
    e1.head === e2.head || return false
    length(e1.args) == length(e2.args) || return false
    for (a, b) in Iterators.zip(e1.args, e2.args)
        expr_structure_eq(a,b) || return false
    end
    true
end

macro newmod(name="newmod_$(string(__source__))", parentmod=__module__,
              edition=VERSION, body...)
    mod_ex = :(
        module $(Symbol(name))
        const JuliaLowering = $(JuliaLowering)
        const JuliaSyntax = $(JuliaSyntax)
        const JL_NEW_EDITION = $(JL_NEW_EDITION)
        const JL_OLD_EDITION = $(JL_OLD_EDITION)
        const adopt_scope = $(adopt_scope)
        const syntax_module = $(syntax_module)
        const var"@legacy_quote_to_syntax" = JuliaLowering.var"@legacy_quote_to_syntax"
        $(body...)
        end)
    Expr(:block,
         :(mod = $(Expr(:escape, :(Core.eval($parentmod, $(QuoteNode(mod_ex))))))),
         :(Base.set_syntax_version(mod, $(esc(edition)))),
         Expr(Symbol("latestworld-if-toplevel")), :mod)
end

function with_lowering(f, enable::Bool)
    lowerer = Core._lower
    toplevel_eval = Core._toplevel_eval
    world = JuliaLowering._lowering_world[]
    try
        JuliaLowering.activate!(enable)
        f()
    finally
        Core._setlowerer!(lowerer)
        Core._set_toplevel_eval!(toplevel_eval)
        JuliaLowering._lowering_world[] = world
    end
end

with_flisp(f) = with_lowering(f, false)

function fl_macroexpand(mod::Module, x::Expr)
    with_flisp() do
        ccall(:jl_macroexpand, Any, (Any, Any, Cint, Cint, Cint), x, mod, true, false, true)
    end
end

function fl_lower(mod::Module, x::Expr)
    with_flisp() do
        Base.fl_lower(mod, x, Base.get_world_counter(), false, Base.first_linenode(x))[1]
    end
end

function fl_eval(mod::Module, x::Expr)
    with_flisp() do
        Core.eval(mod, x)
    end
end

fl_eval(mod::Module, code::AbstractString) =
    fl_eval(mod, Meta.parseall(code; filename="string", mod))

function _force_syntax(x, mod, edition::Tuple{Int, Int})
    if x isa SyntaxTree
        JuliaLowering._ensure_edition(x, edition)
    elseif x isa AbstractString
        # note macroexpand/lower aren't particularly useful with parseall
        JuliaSyntax.parseall(SyntaxTree, x; version=VersionNumber(edition), ignore_warnings=true)
    elseif x isa Expr
        JuliaLowering.expr_to_est(x, Base.first_linenode(x),
                                  SyntaxContext(mod, edition))
    else
        error("expected string or AST, got $x")
    end
end

function _jl_macroexpand(mod::Module, st::SyntaxTree)
    JuliaLowering.macroexpand(mod, st)
end

function _jl_lower(mod::Module, st::SyntaxTree)
    JuliaLowering.lower(mod, st)
end

function _jl_eval(mod::Module, st::SyntaxTree)
    JuliaLowering.eval(mod, st)
end

jl_macroexpand(mod::Module, x; edition=JL_NEW_EDITION) =
    _jl_macroexpand(mod, _force_syntax(x, mod, edition))
jl_lower(mod::Module, x; edition=JL_NEW_EDITION) =
    _jl_lower(mod, _force_syntax(x, mod, edition))
jl_eval(mod::Module, x; edition=JL_NEW_EDITION) =
    _jl_eval(mod, _force_syntax(x, mod, edition))

fl_macroexpand(mod::Module, st::SyntaxTree; kws...) =
    fl_macroexpand(mod, JuliaLowering.est_to_expr(st); kws...)
fl_lower(mod::Module, st::SyntaxTree; kws...) =
    fl_lower(mod, JuliaLowering.est_to_expr(st); kws...)
fl_eval(mod::Module, st::SyntaxTree; kws...) =
    fl_eval(mod, JuliaLowering.est_to_expr(st); kws...)
