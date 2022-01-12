using Test
using JuliaSyntax

using Base.Meta: @dump

using JuliaSyntax:
    # Parsing
    ParseStream,
    SourceFile,
    parse_all,
    @K_str,
    # Nodes
    GreenNode,
    SyntaxNode,
    # Node inspection
    kind,
    flags,
    haschildren,
    children,
    child

function parsers_agree_on_file(path)
    code = read(path, String)
    JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
    JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
end

function find_source_in_path(basedir)
    src_list = String[]
    for (root, dirs, files) in walkdir(basedir)
        append!(src_list, (joinpath(root, f) for f in files if endswith(f, ".jl")))
    end
    src_list
end

function test_parse_all_in_path(basedir)
    for f in find_source_in_path(basedir)
        @testset "Parse $(relpath(f, basedir))" begin
            @test parsers_agree_on_file(f)
        end
    end
end

# Version of test_parse for interactive exploration
function itest_parse(production, code; julia_version::VersionNumber=v"1.6")
    stream = ParseStream(code)
    production(JuliaSyntax.ParseState(stream, julia_version))
    t = JuliaSyntax.build_tree(GreenNode, stream, wrap_toplevel_as_kind=K"toplevel")

    println(stdout, "# Code:\n$code\n")

    println(stdout, "# Green tree:")
    show(stdout, MIME"text/plain"(), t, code)
    JuliaSyntax.show_diagnostics(stdout, stream, code)

    s = SyntaxNode(SourceFile(code, filename="none"), t)
    println(stdout, "\n# SyntaxNode:")
    show(stdout, MIME"text/x.sexpression"(), s)

    ex = Expr(s)
    println(stdout, "\n\n# Julia Expr:")
    show(stdout, MIME"text/plain"(), ex)

    f_ex = JuliaSyntax.remove_linenums!(Meta.parse(code, raise=false))
    if JuliaSyntax.remove_linenums!(ex) != f_ex
        println(stdout, "\n\n# AST dump")
        dump(ex)

        printstyled(stdout, "\n\n# flisp Julia Expr:\n", color=:red)
        show(stdout, MIME"text/plain"(), f_ex)
        # return (code, stream, t, s, ex)
    end
    nothing
end

