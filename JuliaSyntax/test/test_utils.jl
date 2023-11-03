using Test

# We need a relative include here as JuliaSyntax my come from Base.
using .JuliaSyntax:
    # Parsing
    ParseStream,
    ParseState,
    Diagnostic,
    SourceFile,
    source_location,
    source_line,
    source_line_range,
    parse!,
    parsestmt,
    parseall,
    parseatom,
    build_tree,
    @K_str,
    # Nodes
    GreenNode,
    SyntaxNode,
    ErrorVal,
    # Node inspection
    kind,
    flags,
    head,
    span,
    SyntaxHead,
    is_trivia,
    sourcetext,
    haschildren,
    children,
    child,
    fl_parseall,
    fl_parse,
    highlight,
    tokenize,
    untokenize

if VERSION < v"1.6"
    # Compat stuff which might not be in Base for older versions
    using JuliaSyntax: isnothing, only, peek
end

function toks(str)
    ts = [JuliaSyntax.Tokenize.untokenize(t, str)=>kind(t)
          for t in JuliaSyntax.Tokenize.tokenize(str)]
    @test ts[end] == (""=>K"EndMarker")
    pop!(ts)
    ts
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
    if h == :function && Meta.isexpr(fl_args[1], :block)
        blockargs = filter(x->!(x isa LineNumberNode), fl_args[1].args)
        posargs = blockargs[1:max(0, length(blockargs))]
        kwargs = blockargs[2:end]
        for i = 1:length(kwargs)
            if Meta.isexpr(kwargs[i], :(=))
                kwargs[i] = Expr(:kw, kwargs[i].args...)
            end
        end
        fl_args[1] = Expr(:tuple, Expr(:parameters, kwargs...), posargs...)
    elseif h == :for
        iterspec = args[1]
        if JuliaSyntax.is_eventually_call(iterspec.args[1]) &&
                Meta.isexpr(iterspec.args[2], :block)
            blk = iterspec.args[2]
            if length(blk.args) == 2 && blk.args[1] isa LineNumberNode
                # Ignore short form function location differences in
                # `for f() = 1:3 end`
                iterspec.args[2] = blk.args[2]
            end
        end
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

function parsers_agree_on_file(filename; kws...)
    text = try
        read(filename, String)
    catch
        # Something went wrong reading the file. This isn't a parser failure so
        # ignore this case.
        return true
    end
    parsers_agree_on_file(text, filename; kws...)
end

function parsers_agree_on_file(text, filename; exprs_equal=exprs_equal_no_linenum)
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
        return !JuliaSyntax.any_error(stream) && exprs_equal(fl_ex, ex)
    catch exc
        @error "Parsing failed" filename exception=current_exceptions()
        return false
    end
end

function find_source_in_path(basedir)
    src_list = String[]
    for (root, dirs, files) in walkdir(basedir)
        append!(src_list, (joinpath(root, f) for f in files
                           if endswith(f, ".jl") && (p = joinpath(root,f); !islink(p) && isfile(p))))
    end
    src_list
end

test_parse_all_in_path(basedir) =
    test_parse_all_in_path(path->exprs_equal_no_linenum, basedir)

function test_parse_all_in_path(compare_for_path::Function, basedir)
    for filepath in find_source_in_path(basedir)
        cmp = compare_for_path(filepath)
        if isnothing(cmp)
            continue
        end
        @testset "Parse $(relpath(filepath, basedir))" begin
            text = try
                read(filepath, String)
            catch
                # Something went wrong reading the file. This isn't a parser failure so
                # ignore this case.
                continue
            end
            parsers_agree = parsers_agree_on_file(text, filepath, exprs_equal=cmp)
            @test parsers_agree
            if !parsers_agree
                reduced_failures = reduce_text.(reduce_tree(text),
                                                parsers_fuzzy_disagree)
                @test reduced_failures == []
            end
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

function _reduce_tree(failing_subtrees, tree; exprs_equal=exprs_equal_no_linenum)
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
            had_failing_subtrees |= _reduce_tree(failing_subtrees, child; exprs_equal=exprs_equal)
        end
    end
    if !had_failing_subtrees
        push!(failing_subtrees, tree)
    end
    return true
end

"""
    reduce_tree(tree::SyntaxNode; exprs_equal=exprs_equal_no_linenum)

Select minimal subtrees of `tree` which are inconsistent between flisp and
JuliaSyntax parsers.
"""
function reduce_tree(tree::SyntaxNode; kws...)
    subtrees = Vector{typeof(tree)}()
    _reduce_tree(subtrees, tree; kws...)
    subtrees
end

"""
    reduce_tree(text::AbstractString; exprs_equal=exprs_equal_no_linenum)

Find the minimal subtrees of the parsed form of `text` which are inconsistent
between flisp and JuliaSyntax parsers and return the source text of those
subtrees.
"""
function reduce_tree(text::AbstractString; kws...)
    tree = parseall(SyntaxNode, text)
    sourcetext.(reduce_tree(tree; kws...))
end


#-------------------------------------------------------------------------------
# Text-based test case reduction
function parser_throws_exception(text)
    try
        JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, text, ignore_errors=true)
        false
    catch
        true
    end
end

function parsers_fuzzy_disagree(text::AbstractString)
    fl_ex = fl_parseall(text, filename="none")
    if Meta.isexpr(fl_ex, (:error,:incomplete)) ||
            (Meta.isexpr(fl_ex, :toplevel) && length(fl_ex.args) >= 1 &&
             Meta.isexpr(fl_ex.args[end], (:error,:incomplete)))
        return false
    end
    try
        ex = parseall(Expr, text, filename="none", ignore_errors=true)
        return !exprs_roughly_equal(fl_ex, ex)
    catch
        @error "Reduction failed" text
        return false
    end
end


"""
Reduce text of a test case via combination of bisection and random deletion.

This is suited to randomly generated strings, but it's surprisingly effective
for code-like strings as well.
"""
function reduce_text(str, parse_differs)
    while true
        if length(str) <= 1
            return str
        end
        m1 = thisind(str, length(str)÷2)
        m2 = nextind(str, m1)
        if parse_differs(str[1:m1])
            str = str[1:m1]
        elseif parse_differs(str[m2:end])
            str = str[m2:end]
        else
            chunklen = clamp(length(str)÷10, 1, 10)
            reduced = false
            for i = 1:100
                m = thisind(str, rand(1:length(str)-chunklen))
                m3 = nextind(str, m+chunklen)
                if m3 == nextind(str, m)
                    continue
                end
                s = str[1:m]*str[m3:end]
                if parse_differs(s)
                    str = s
                    reduced = true
                    break
                end
            end
            if !reduced
                return str
            end
        end
    end
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


#-------------------------------------------------------------------------------
# Tools copied from Base.Meta which call core_parser_hook as if called by
# Meta.parse(), but without installing the global hook.

function _Meta_parse_string(text::AbstractString, filename::AbstractString,
                            lineno::Integer, index::Integer, options)
    if index < 1 || index > ncodeunits(text) + 1
        throw(BoundsError(text, index))
    end
    ex, offset::Int = JuliaSyntax.core_parser_hook(text, filename, lineno, index-1, options)
    ex, offset+1
end

function Meta_parse(str::AbstractString, pos::Integer;
               filename="none", greedy::Bool=true, raise::Bool=true, depwarn::Bool=true)
    ex, pos = _Meta_parse_string(str, String(filename), 1, pos, greedy ? :statement : :atom)
    if raise && Meta.isexpr(ex, :error)
        err = ex.args[1]
        if err isa String
            err = Meta.ParseError(err) # For flisp parser
        end
        throw(err)
    end
    return ex, pos
end

function Meta_parse(str::AbstractString;
                    filename="none", raise::Bool=true, depwarn::Bool=true)
    ex, pos = Meta_parse(str, 1; filename=filename, greedy=true, raise=raise, depwarn=depwarn)
    if Meta.isexpr(ex, :error)
        return ex
    end
    if pos <= ncodeunits(str)
        raise && throw(Meta.ParseError("extra token after end of expression"))
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end

function Meta_parseatom(text::AbstractString, pos::Integer; filename="none", lineno=1)
    return _Meta_parse_string(text, String(filename), lineno, pos, :atom)
end

function Meta_parseall(text::AbstractString; filename="none", lineno=1)
    ex,_ = _Meta_parse_string(text, String(filename), lineno, 1, :all)
    return ex
end

