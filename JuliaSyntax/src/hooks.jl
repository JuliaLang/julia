# This file provides an adaptor to match the API expected by the Julia runtime
# code in the binding Core._parse

const _has_v1_6_hooks  = VERSION >= v"1.6"
const _has_v1_10_hooks = isdefined(Core, :_setparser!)

# Find the first error in a SyntaxNode tree, returning the index of the error
# within its parent and the node itself.
function _first_error(t::SyntaxNode)
    if is_error(t)
        return 0,t
    end
    if haschildren(t)
        for (i,c) in enumerate(children(t))
            if is_error(c)
                return i,c
            else
                x = _first_error(c)
                if x != (0,nothing)
                    return x
                end
            end
        end
    end
    return 0,nothing
end

# Classify an incomplete expression, returning a Symbol compatible with
# Base.incomplete_tag().
#
# Roughly, the intention here is to classify which expression head is expected
# next if the incomplete stream was to continue. (Though this is just rough. In
# practice several categories are combined for the purposes of the REPL -
# perhaps we can/should do something more precise in the future.)
function _incomplete_tag(n::SyntaxNode, codelen)
    i,c = _first_error(n)
    if isnothing(c) || last_byte(c) < codelen || codelen == 0
        return :none
    elseif first_byte(c) <= codelen
        if kind(c) == K"ErrorEofMultiComment" && last_byte(c) == codelen
            # This is the one weird case where the token itself is an
            # incomplete error
            return :comment
        else
            return :none
        end
    end
    if kind(c) == K"error" && begin
                cs = children(c)
                length(cs) > 0
            end
        for cc in cs
            if kind(cc) == K"error"
                return :other
            end
        end
    end
    if isnothing(c.parent)
        return :other
    end
    kp = kind(c.parent)
    if kp == K"string"
        return :string
    elseif kp == K"cmdstring"
        return :cmd
    elseif kp == K"char"
        return :char
    elseif kp in KSet"block quote let try"
        return :block
    elseif kp in KSet"for while function if"
        return i == 1 ? :other : :block
    elseif kp in KSet"module struct"
        return i == 1 ? :other : :block
    elseif kp == K"do"
        return i < 3  ? :other : :block
    else
        return :other
    end
end

#-------------------------------------------------------------------------------
function _set_core_parse_hook(parser)
    @static if _has_v1_10_hooks
        Core._setparser!(parser)
    else
        # HACK! Fool the runtime into allowing us to set Core._parse, even during
        # incremental compilation. (Ideally we'd just arrange for Core._parse to be
        # set to the JuliaSyntax parser. But how do we signal that to the dumping
        # code outside of the initial creation of Core?)
        i = Base.fieldindex(Base.JLOptions, :incremental)
        ptr = convert(Ptr{fieldtype(Base.JLOptions, i)},
                      cglobal(:jl_options, Base.JLOptions) + fieldoffset(Base.JLOptions, i))
        incremental = unsafe_load(ptr)
        if incremental != 0
            unsafe_store!(ptr, 0)
        end

        Base.eval(Core, :(_parse = $parser))

        if incremental != 0
            unsafe_store!(ptr, incremental)
        end
    end
end


# Wrap the function `f` so that it's always invoked in the given `world_age`
#
# NB: We need an inference barrier of one type or another here to prevent
# invalidations. The invokes provide this currently.
function fix_world_age(f, world_age::UInt)
    if world_age == typemax(UInt)
        function invoke_latestworld(args...; kws...)
            Base.invokelatest(f, args...; kws...)
        end
    else
        function invoke_fixedworld(args...; kws...)
            Base.invoke_in_world(world_age, f, args...; kws...)
        end
    end
end

function _has_nested_error(ex)
    if ex isa Expr
        if ex.head == :error
            return true
        else
            return any(_has_nested_error(e) for e in ex.args)
        end
    elseif ex isa QuoteNode
        return _has_nested_error(ex.value)
    else
        return false
    end
end

# Debug log file for dumping parsed code
const _debug_log = Ref{Union{Nothing,IO}}(nothing)

function core_parser_hook(code, filename::String, lineno::Int, offset::Int, options::Symbol)
    try
        # TODO: Check that we do all this input wrangling without copying the
        # code buffer
        if code isa Core.SimpleVector
            # The C entry points will pass us this form.
            (ptr,len) = code
            code = String(unsafe_wrap(Array, ptr, len))
        elseif !(code isa String || code isa SubString || code isa Vector{UInt8})
            # For non-Base string types, convert to UTF-8 encoding, using an
            # invokelatest to avoid world age issues.
            code = Base.invokelatest(String, code)
        end
        if !isnothing(_debug_log[])
            print(_debug_log[], """
                  #-#-#-------------------------------
                  # ENTER filename=$filename, lineno=$lineno, offset=$offset, options=$options"
                  #-#-#-------------------------------
                  """)
            write(_debug_log[], code)
        end

        stream = ParseStream(code, offset+1)
        if options === :statement || options === :atom
            # To copy the flisp parser driver:
            # * Parsing atoms      consumes leading trivia
            # * Parsing statements consumes leading+trailing trivia
            bump_trivia(stream)
            if peek(stream) == K"EndMarker"
                # If we're at the end of stream after skipping whitespace, just
                # return `nothing` to indicate this rather than attempting to
                # parse a statement or atom and failing.
                return Core.svec(nothing, last_byte(stream))
            end
        end
        parse!(stream; rule=options)
        if options === :statement
            bump_trivia(stream)
        end

        if any_error(stream)
            tree = build_tree(SyntaxNode, stream, first_line=lineno, filename=filename)
            tag = _incomplete_tag(tree, lastindex(code))
            if _has_v1_10_hooks
                exc = ParseError(stream, filename=filename, first_line=lineno,
                                 incomplete_tag=tag)
                msg = sprint(showerror, exc)
                error_ex = Expr(tag === :none ? :error : :incomplete,
                                Meta.ParseError(msg, exc))
            elseif tag !== :none
                # Hack: For older Julia versions, replicate the messages which
                # Base.incomplete_tag() will match
                msg =
                    tag === :string  ? "incomplete: invalid string syntax"     :
                    tag === :comment ? "incomplete: unterminated multi-line comment #= ... =#" :
                    tag === :block   ? "incomplete: construct requires end"    :
                    tag === :cmd     ? "incomplete: invalid \"`\" syntax"      :
                    tag === :char    ? "incomplete: invalid character literal" :
                                       "incomplete: premature end of input"
                error_ex = Expr(:incomplete, msg)
            else
                # In the flisp parser errors are normally `Expr(:error, msg)` where
                # `msg` is a String. By using a JuliaSyntax.ParseError for msg
                # we can do fancy error reporting instead.
                error_ex = Expr(:error, ParseError(stream, filename=filename, first_line=lineno))
            end
            ex = if options === :all
                # When encountering a toplevel error, the reference parser
                # * truncates the top level expression arg list before that error
                # * includes the last line number
                # * appends the error message
                topex = Expr(tree)
                @assert topex.head == :toplevel
                i = findfirst(_has_nested_error, topex.args)
                if i > 1 && topex.args[i-1] isa LineNumberNode
                    i -= 1
                end
                resize!(topex.args, i-1)
                _,errort = _first_error(tree)
                push!(topex.args, LineNumberNode(source_line(errort), filename))
                push!(topex.args, error_ex)
                topex
            else
                error_ex
            end
        else
            # TODO: Figure out a way to show warnings. Meta.parse() has no API
            # to communicate this, and we also can't show them to stdout as
            # this is too side-effectful and can result in double-reporting in
            # the REPL.
            #
            # show_diagnostics(stdout, stream.diagnostics, code)
            #
            ex = build_tree(Expr, stream; filename=filename, first_line=lineno)
        end

        # Note the next byte in 1-based indexing is `last_byte(stream) + 1` but
        # the Core hook must return an offset (ie, it's 0-based) so the factors
        # of one cancel here.
        last_offset = last_byte(stream)

        if !isnothing(_debug_log[])
            println(_debug_log[], """
                    #-#-#-
                    # EXIT last_offset=$last_offset
                    #-#-#-
                    """)
        end

        # Rewrap result in an svec for use by the C code
        return Core.svec(ex, last_offset)
    catch exc
        if !isnothing(_debug_log[])
            println(_debug_log[], """
                    #-#-#-
                    # ERROR EXIT
                    # $exc
                    #-#-#-
                    """)
        end
        @error("""JuliaSyntax parser failed — falling back to flisp!
                  This is not your fault. Please submit a bug report to https://github.com/JuliaLang/JuliaSyntax.jl/issues""",
               exception=(exc,catch_backtrace()),
               offset=offset,
               code=code)

        _fl_parse_hook(code, filename, lineno, offset, options)
    end
end

# Core._parse gained a `lineno` argument in
# https://github.com/JuliaLang/julia/pull/43876
# Prior to this, the following signature was needed:
function core_parser_hook(code, filename, offset, options)
    core_parser_hook(code, filename, 1, offset, options)
end

if _has_v1_10_hooks
    Base.incomplete_tag(e::JuliaSyntax.ParseError) = e.incomplete_tag
else
    # Hack: Meta.parse() attempts to construct a ParseError from a string if it
    # receives `Expr(:error)`. Add an override to the ParseError constructor to
    # prevent this.
    Base.Meta.ParseError(e::JuliaSyntax.ParseError) = e
end

"""
    enable_in_core!([enable=true; freeze_world_age=true, debug_filename=nothing])

Connect the JuliaSyntax parser to the Julia runtime so that it replaces the
flisp parser for all parsing work. That is, JuliaSyntax will be used for
`include()`, `Meta.parse()`, the REPL, etc. To reset to the reference parser,
use `enable_in_core!(false)`.

Keyword arguments:
* `freeze_world_age` - Use a fixed world age for the parser to prevent
  recompilation of the parser due to any user-defined methods (default `true`).
* `debug_filename` - File name of parser debug log (defaults to `nothing` or
  the value of `ENV["JULIA_SYNTAX_DEBUG_FILE"]`).
"""
function enable_in_core!(enable=true; freeze_world_age = true,
        debug_filename   = get(ENV, "JULIA_SYNTAX_DEBUG_FILE", nothing))
    if !_has_v1_6_hooks
        error("Cannot use JuliaSyntax as the main Julia parser in Julia version $VERSION < 1.6")
    end
    if enable && !isnothing(debug_filename)
        _debug_log[] = open(debug_filename, "w")
    elseif !enable && !isnothing(_debug_log[])
        close(_debug_log[])
        _debug_log[] = nothing
    end
    if enable
        world_age = freeze_world_age ? Base.get_world_counter() : typemax(UInt)
        _set_core_parse_hook(fix_world_age(core_parser_hook, world_age))
    else
        _set_core_parse_hook(Core.Compiler.fl_parse)
    end
    nothing
end


#-------------------------------------------------------------------------------
# Tools to call the reference flisp parser
#
# Call the flisp parser
function _fl_parse_hook(code, filename, lineno, offset, options)
    @static if VERSION >= v"1.8.0-DEV.1370" # https://github.com/JuliaLang/julia/pull/43876
        return Core.Compiler.fl_parse(code, filename, lineno, offset, options)
    elseif _has_v1_6_hooks
        return Core.Compiler.fl_parse(code, filename, offset, options)
    else
        if options === :all
            ex = Base.parse_input_line(String(code), filename=filename, depwarn=false)
            if !@isexpr(ex, :toplevel)
                ex = Expr(:toplevel, ex)
            end
            return ex, sizeof(code)
        elseif options === :statement || options === :atom
            ex, pos = Meta.parse(code, offset+1, greedy=options==:statement, raise=false)
            return ex, pos-1
        else
            error("Unknown parse options $options")
        end
    end
end

#------------------------------------------------
# Copy of the Meta.parse() API, but ensuring that we call the flisp parser
# rather than using Meta.parse() which may be using the JuliaSyntax parser.

"""
Like Meta.parse() but always call the flisp reference parser.
"""
function fl_parse(str::AbstractString; raise::Bool=true, depwarn::Bool=true)
    ex, pos = fl_parse(str, 1, greedy=true, raise=raise, depwarn=depwarn)
    if isa(ex,Expr) && ex.head === :error
        return ex
    end
    if pos <= ncodeunits(str)
        raise && throw(Meta.ParseError("extra token after end of expression"))
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end

function fl_parse(str::AbstractString, pos::Integer; greedy::Bool=true, raise::Bool=true,
                  depwarn::Bool=true)
    ex, pos = _fl_parse_string(str, "none", 1, pos, greedy ? :statement : :atom)
    if raise && isa(ex,Expr) && ex.head === :error
        throw(Meta.ParseError(ex.args[1]))
    end
    return ex, pos
end

"""
Like Meta.parseall() but always call the flisp reference parser.
"""
function fl_parseall(text::AbstractString; filename="none", lineno=1)
    ex,_ = _fl_parse_string(text, String(filename), lineno, 1, :all)
    return ex
end

function _fl_parse_string(text::AbstractString, filename::AbstractString,
                          lineno::Integer, index::Integer, options)
    if index < 1 || index > ncodeunits(text) + 1
        throw(BoundsError(text, index))
    end
    ex, offset::Int = _fl_parse_hook(text, filename, lineno, index-1, options)
    ex, offset+1
end

# Convenience functions to mirror `JuliaSyntax.parsestmt(Expr, ...)` in simple cases.
fl_parse(::Type{Expr}, args...; kws...) = fl_parse(args...; kws...)
fl_parseall(::Type{Expr}, args...; kws...) = fl_parseall(args...; kws...)

