using Test

# We need a relative include here as JuliaSyntax my come from Base.
using .JuliaSyntax:
    # Parsing
    ParseStream,
    ParseState,
    Diagnostic,
    SourceFile,
    source_location,
    parse!,
    parse,
    parseall,
    parseatom,
    build_tree,
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
    fl_parse,
    highlight

if VERSION < v"1.6"
    # Compat stuff which might not be in Base for older versions
    using JuliaSyntax: isnothing, only, peek
end

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

function show_expr_text_diff(io::IO, showfunc, e1, e2; context=2)
    if Sys.isunix()
        mktemp() do path1, io1
            mktemp() do path2, io2
                showfunc(io1, e1);   close(io1)
                showfunc(io2, e2); close(io2)
                run(pipeline(ignorestatus(`diff -U$context --color=always $path1 $path2`), io))
            end
        end
    else
        showfunc(io, ex)
        println(io, "------------------------------------")
        showfunc(io, e2)
    end
    nothing
end

# Parse text with JuliaSyntax vs reference parser and show a textural diff of
# the resulting expressions
function parse_diff(text, showfunc=dump)
    ex = parse(Expr, text, filename="none")
    fl_ex = fl_parse(text)
    show_expr_text_diff(stdout, showfunc, ex, fl_ex)
end

function kw_to_eq(ex)
    return Meta.isexpr(ex, :kw) ? Expr(:(=), ex.args...) : ex
end

function triple_string_roughly_equal(fl_str, str)
    # Allow some leeway for a bug in the reference parser with
    # triple quoted strings
    lines = split(str, '\n')
    fl_lines = split(fl_str, '\n')
    if length(lines) != length(fl_lines)
        return false
    end
    has_whitespace_only_line =
        any(!isempty(fl_line) && all(c in " \t" for c in fl_line)
            for fl_line in fl_lines)
    if !has_whitespace_only_line
        return str == fl_str
    end
    for (line, fl_line) in zip(lines, fl_lines)
        if !all(c in " \t" for c in fl_line) && !endswith(line, fl_line)
            return false
        end
    end
    return true
end

function exprs_equal_no_linenum(fl_ex, ex)
    remove_all_linenums!(deepcopy(ex)) == remove_all_linenums!(deepcopy(fl_ex))
end

# Compare Expr from reference parser expression to JuliaSyntax parser, ignoring
# differences due to bugs in the reference parser.
function exprs_roughly_equal(fl_ex, ex)
    if fl_ex isa Float64 && Meta.isexpr(ex, :call, 3) &&
                            ex.args[1] == :* &&
                            ex.args[2] == fl_ex &&
                            (ex.args[3] == :f || ex.args[3] == :f0)
        # 0x1p0f
        return true
    elseif !(fl_ex isa Expr) || !(ex isa Expr)
        if fl_ex isa String && ex isa String
            if fl_ex == ex
                return true
            else
                return triple_string_roughly_equal(fl_ex, ex)
            end
        else
            return fl_ex == ex
        end
    end
    # Ignore differences in line number nodes within block-like constructs
    fl_args = fl_ex.head in (:block, :quote, :toplevel) ?
              filter(x->!(x isa LineNumberNode), fl_ex.args) :
              fl_ex.args
    args = ex.head in (:block, :quote, :toplevel) ?
           filter(x->!(x isa LineNumberNode), ex.args) :
           ex.args
    if (fl_ex.head == :block && ex.head == :tuple && 
        length(fl_args) == 2 && length(args) == 2 &&
        Meta.isexpr(args[1], :parameters, 1) &&
        exprs_roughly_equal(fl_args[2], args[1].args[1]) &&
        exprs_roughly_equal(fl_args[1], args[2]))
        # Allow `(a; b,)`:
        # * Reference parser produces a block
        # * New parser produces a frankentuple
        return true
    end
    if fl_ex.head != ex.head
        return false
    end
    h = ex.head
    if (h == :global || h == :local) && length(args) == 1 && Meta.isexpr(args[1], :tuple)
        # Allow invalid syntax like `global (x, y)`
        args = args[1].args
    elseif h == :function && Meta.isexpr(fl_args[1], :block)
        blockargs = filter(x->!(x isa LineNumberNode), fl_args[1].args)
        ps = blockargs[2:end]
        for i = 1:length(ps)
            if Meta.isexpr(ps[i], :(=))
                ps[i] = Expr(:kw, ps[i].args...)
            end
        end
        fl_args[1] = Expr(:tuple, Expr(:parameters, ps...), blockargs[1])
    end
    if length(fl_args) != length(args)
        return false
    end
    if h == :do && length(args) >= 1 && Meta.isexpr(fl_args[1], :macrocall)
        # Macrocalls with do, as in `@f(a=1) do\nend` use :kw in the
        # reference parser for the `a=1`, but we regard this as a bug.
        fl_args = copy(fl_args)
        fl_args[1] = Expr(:macrocall, map(kw_to_eq, args[1].args)...)
    end
    for i = 1:length(args)
        flarg = fl_args[i]
        arg = args[i]
        if !exprs_roughly_equal(flarg, arg)
            return false
        end
    end
    return true
end

function parsers_agree_on_file(filename; exprs_equal=exprs_equal_no_linenum,
                               show_diff=false)
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
        stream = ParseStream(text)
        parse!(stream)
        ex = build_tree(Expr, stream, filename=filename)
        if show_diff && ex != fl_ex
            show_expr_text_diff(stdout, show, ex, fl_ex)
        end
        return !JuliaSyntax.any_error(stream) && exprs_equal(fl_ex, ex)
        # Could alternatively use
        # exprs_roughly_equal(fl_ex, ex)
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
function equals_flisp_parse(exprs_equal, tree)
    node_text = sourcetext(tree)
    # Reparse with JuliaSyntax. This is a crude way to ensure we're not missing
    # some context from the parent node.
    fl_ex = fl_parseall(node_text, filename="none")
    if Meta.isexpr(fl_ex, :error) || (Meta.isexpr(fl_ex, :toplevel) &&
                                      length(fl_ex.args) >= 1 &&
                                      Meta.isexpr(fl_ex.args[end], :error))
        return true # Something went wrong in reduction; ignore these cases 😬
    end
    ex = parseall(Expr, node_text, filename="none", ignore_errors=true)
    exprs_equal(fl_ex, ex)
end

function _reduce_test(failing_subtrees, tree; exprs_equal=exprs_equal_no_linenum)
    if equals_flisp_parse(exprs_equal, tree)
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
            had_failing_subtrees |= _reduce_test(failing_subtrees, child; exprs_equal=exprs_equal)
        end
    end
    if !had_failing_subtrees
        push!(failing_subtrees, tree)
    end
    return true
end

"""
    reduce_test(text::AbstractString; exprs_equal=exprs_equal_no_linenum)
    reduce_test(tree::SyntaxNode; exprs_equal=exprs_equal_no_linenum)

Select minimal subtrees of `text` or `tree` which are inconsistent between
flisp and JuliaSyntax parsers.
"""
function reduce_test(tree::SyntaxNode; kws...)
    subtrees = Vector{typeof(tree)}()
    _reduce_test(subtrees, tree; kws...)
    subtrees
end

function reduce_test(text::AbstractString; kws...)
    tree = parseall(SyntaxNode, text)
    reduce_test(tree; kws...)
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
    JuliaSyntax.validate_tokens(stream)
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
        show_expr_text_diff(stdout, show, ex, f_ex, context=10)
        # return (ex, f_ex)
        # return (code, stream, t, s, ex)
    end
    nothing
end

function show_green_tree(code; version::VersionNumber=v"1.6")
    t = JuliaSyntax.parseall(GreenNode, code, version=version)
    sprint(show, MIME"text/plain"(), t, code)
end


#-------------------------------------------------------------------------------
# Parse s-expressions
function parse_sexpr(code)
    st = ParseStream(code)
    pos_stack = ParseStreamPosition[]
    while true
        k = peek(st)
        if k == K"("
            push!(pos_stack, position(st))
            bump(st, TRIVIA_FLAG)
        elseif k == K")"
            if isempty(pos_stack)
                bump(st, error="Mismatched `)` with no opening `(`")
                break
            else
                bump(st, TRIVIA_FLAG)
            end
            emit(st, pop!(pos_stack), K"parens")
        elseif k == K"Identifier" || k == K"Integer"
            bump(st)
        elseif k == K"NewlineWs"
            bump(st, TRIVIA_FLAG)
        elseif k == K"EndMarker"
            if !isempty(pos_stack)
                bump_invisible(st, K"error", error="Mismatched `)`")
            end
            break
        else
            bump(st, error="Unexpected token")
        end
    end
    if JuliaSyntax.any_error(st)
        throw(JuliaSyntax.ParseError(st))
    end
    st
end


