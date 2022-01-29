using Test
using JuliaSyntax

using Base.Meta: @dump

using JuliaSyntax:
    # Parsing
    ParseStream,
    SourceFile,
    parse,
    parseall,
    @K_str,
    # Nodes
    GreenNode,
    SyntaxNode,
    # Node inspection
    kind,
    flags,
    is_trivia,
    sourcetext,
    haschildren,
    children,
    child,
    flisp_parse_all

function remove_macro_linenums!(ex)
    if Meta.isexpr(ex, :macrocall)
        ex.args[2] = nothing
    end
    if ex isa Expr
        map!(remove_macro_linenums!, ex.args, ex.args)
    end
    return ex
end

function remove_all_linenums!(ex)
    JuliaSyntax.remove_linenums!(ex)
    remove_macro_linenums!(ex)
end

function parsers_agree_on_file(path)
    text = read(path, String)
    ex = parseall(Expr, text)
    fl_ex = flisp_parse_all(text)
    JuliaSyntax.remove_linenums!(ex) == JuliaSyntax.remove_linenums!(fl_ex)
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

#-------------------------------------------------------------------------------
# Test case reduction

# Check whether a given SyntaxNode converts to the same Expr as the flisp
# parser produces from the source text of the node.
function equals_flisp_parse(tree)
    node_text = sourcetext(tree)
    # Reparse with JuliaSyntax. This is a crude way to ensure we're not missing
    # some context from the parent node.
    ex,_,_ = parse(Expr, node_text)
    fl_ex = flisp_parse_all(node_text)
    if Meta.isexpr(fl_ex, :error)
        return true  # Something went wrong in reduction; ignore these cases 😬
    end
    remove_all_linenums!(ex) == remove_all_linenums!(fl_ex)
end

"""
Select a subtree of `tree` which is inconsistent between flisp and JuliaSyntax
parsers. This isn't very precise yet!

TODO:
* For some syntax elements (eg, the `x in xs` inside `for x in xs`) the
  children can't be parsed out of context. Fix this.
* Replace good siblings of bad nodes with placeholders. For blocks, delete such
  siblings.
"""
function reduce_test(tree)
    if equals_flisp_parse(tree)
        return nothing
    end
    if !haschildren(tree)
        return tree
    else
        for child in children(tree)
            if is_trivia(child) || !haschildren(child)
                continue
            end
            t = reduce_test(child)
            if !isnothing(t)
                return t
            end
        end
    end
    return tree
end

function reduce_all_failures_in_path(basedir, outdir)
    rm(outdir, force=true, recursive=true)
    mkpath(outdir)
    for filename in find_source_in_path(basedir)
        filetext = read(filename, String)
        if !(try parsers_agree_on_file(filename) catch exc false end)
            @info "Found failure" filename
            filetext = read(filename, String)
            text = nothing
            try
                tree, _ = parse(SyntaxNode, filetext)
                rtree = reduce_test(tree)
                text = sourcetext(rtree)
            catch
                @error "Error reducing file" exception=current_exceptions()
                text = filetext
            end
            bn,_ = splitext(basename(filename))
            outname = joinpath(outdir, "$bn.jl")
            i=1
            while isfile(outname)
                outname = joinpath(outdir, "$bn-$i.jl")
                i += 1
            end
            write(outname, text)
        end
    end
end

#-------------------------------------------------------------------------------
"""
    itest_parse(production, code; version::VersionNumber=v"1.6")

Parse `code`, entering the recursive descent parser at the given function
`production`. This function shows the various tree representations on stdout
for debugging.
"""
function itest_parse(production, code; version::VersionNumber=v"1.6")
    stream = ParseStream(code; version=version)
    production(JuliaSyntax.ParseState(stream))
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

