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
    fl_parseall,
    fl_parse

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

function show_expr_text_diff(showfunc, ex, f_ex; context=2)
    if Sys.isunix()
        mktemp() do path1, io1
            mktemp() do path2, io2
                showfunc(io1, ex);   close(io1)
                showfunc(io2, f_ex); close(io2)
                run(ignorestatus(`diff -U$context --color=always $path1 $path2`))
            end
        end
    else
        showfunc(stdout, ex)
        println("------------------------------------")
        showfunc(stdout, f_ex)
    end
end


function parsers_agree_on_file(filename; show_diff=false)
    text = try
        read(filename, String)
    catch
        # Something went wrong reading the file. This isn't a parser failure so
        # ignore this case.
        return true
    end
    fl_ex = fl_parseall(text, filename=filename)
    if Meta.isexpr(fl_ex, :toplevel) && !isempty(fl_ex.args) &&
            Meta.isexpr(fl_ex.args[end], (:error, :incomplete))
        # Reference parser failed. This generally indicates a broken file not a
        # parser problem, so ignore this case.
        return true
    end
    try
        ex, diagnostics, _ = parse(Expr, text, filename=filename)
        if show_diff && ex != fl_ex
            show_expr_text_diff(show, ex, fl_ex)
        end
        return !JuliaSyntax.any_error(diagnostics) &&
            JuliaSyntax.remove_linenums!(ex) ==
            JuliaSyntax.remove_linenums!(fl_ex)
    catch exc
        @error "Parsing failed" filename exception=current_exceptions()
        return false
    end
end

function find_source_in_path(basedir)
    src_list = String[]
    for (root, dirs, files) in walkdir(basedir)
        append!(src_list, (joinpath(root, f) for f in files
                           if endswith(f, ".jl") && isfile(joinpath(root,f))))
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
    fl_ex = fl_parseall(node_text)
    if Meta.isexpr(fl_ex, :error)
        return true  # Something went wrong in reduction; ignore these cases 😬
    end
    remove_all_linenums!(ex) == remove_all_linenums!(fl_ex)
end

"""
    reduce_test(text::AbstractString)
    reduce_test(tree::SyntaxNode)

Select minimal subtrees of `text` or `tree` which are inconsistent between
flisp and JuliaSyntax parsers.
"""
function reduce_test(failing_subtrees, tree)
    if equals_flisp_parse(tree)
        return false
    end
    if !haschildren(tree)
        push!(failing_subtrees, tree)
        return true
    end
    had_failing_subtrees = false
    if haschildren(tree)
        for child in children(tree)
            if is_trivia(child) || !haschildren(child)
                continue
            end
            had_failing_subtrees |= reduce_test(failing_subtrees, child)
        end
    end
    if !had_failing_subtrees
        push!(failing_subtrees, tree)
    end
    return true
end

function reduce_test(tree::SyntaxNode)
    subtrees = Vector{typeof(tree)}()
    reduce_test(subtrees, tree)
    subtrees
end

function reduce_test(text::AbstractString)
    tree, _, _ = parse(SyntaxNode, text)
    reduce_test(tree)
end


"""
    format_reduced_tests(out::IO, file_content)

Reduced the syntax (a string or SyntaxNode) from `file_content` into the
minimal failing subtrees of syntax and write the results to `out`.
"""
function format_reduced_tests(out::IO, file_content; filename=nothing)
    println(out, "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    if !isnothing(filename)
        println(out, "# $filename")
    end
    text = nothing
    try
        rtrees = reduce_test(file_content)
        for rt in rtrees
            print(out, "\n#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
            t = sourcetext(rt)
            print(out, t)
            if !endswith(t, '\n')
                println(out)
            end
        end
    catch exc
        exc isa InterruptException && rethrow()
        @error "Error reducing file" exception=current_exceptions()
        print(out, file_content isa AbstractString ?
              file_content : sourcetext(file_content))
    end
end

function reduce_all_failures_in_path(basedir, outdir)
    rm(outdir, force=true, recursive=true)
    mkpath(outdir)
    for filename in find_source_in_path(basedir)
        if !parsers_agree_on_file(filename)
            @info "Found failure" filename
            bn,_ = splitext(basename(filename))
            outname = joinpath(outdir, "$bn.jl")
            i = 1
            while isfile(outname)
                outname = joinpath(outdir, "$bn-$i.jl")
                i += 1
            end
            open(outname, "w") do io
                format_reduced_tests(io, read(filename, String), filename=filename)
            end
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

    f_ex = JuliaSyntax.remove_linenums!(fl_parse(code, raise=false))
    if JuliaSyntax.remove_linenums!(ex) != f_ex
        printstyled(stdout, "\n\n# flisp Julia Expr:\n", color=:red)
        show(stdout, MIME"text/plain"(), f_ex)

        printstyled(stdout, "\n\n# Diff of AST dump:\n", color=:red)
        show_expr_text_diff(show, ex, f_ex, context=10)
        # return (ex, f_ex)
        # return (code, stream, t, s, ex)
    end
    nothing
end

function show_green_tree(code; version::VersionNumber=v"1.6")
    t = JuliaSyntax.parseall(GreenNode, code, version=version)
    sprint(show, MIME"text/plain"(), t, code)
end
