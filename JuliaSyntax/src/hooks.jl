# This file provides an adaptor to match the API expected by the Julia runtime
# code in the binding Core._parse

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
function _incomplete_tag(n::SyntaxNode)
    i,c = _first_error(n)
    if isnothing(c)
        return :none
    end
    # TODO: Check error hits last character
    if kind(c) == K"ErrorEofMultiComment"
        return :comment
    elseif kind(c) == K"error" && begin
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
        return i == 2 ? :other : :block
    elseif kp == K"do"
        return i < 3  ? :other : :block
    else
        return :other
    end
end

#-------------------------------------------------------------------------------
@static if isdefined(Core, :_setparser!)
    const _set_core_parse_hook = Core._setparser!
elseif isdefined(Core, :set_parser)
    const _set_core_parse_hook = Core.set_parser
else
    function _set_core_parse_hook(parser)
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

# Use caller's world age.
const _latest_world = typemax(UInt)
const _parser_world_age = Ref{UInt}(_latest_world)

function core_parser_hook(code, filename, lineno, offset, options)
    # NB: We need an inference barrier of one type or another here to prevent
    # invalidations. The invokes provide this currently.
    if _parser_world_age[] != _latest_world
        Base.invoke_in_world(_parser_world_age[], _core_parser_hook,
                             code, filename, lineno, offset, options)
    else
        Base.invokelatest(_core_parser_hook, code, filename, lineno, offset, options)
    end
end

# Core._parse gained a `lineno` argument in
# https://github.com/JuliaLang/julia/pull/43876
# Prior to this, the following signature was needed:
function core_parser_hook(code, filename, offset, options)
    core_parser_hook(code, filename, 1, offset, options)
end

# Debug log file for dumping parsed code
const _debug_log = Ref{Union{Nothing,IO}}(nothing)

function _core_parser_hook(code, filename, lineno, offset, options)
    try
        # TODO: Check that we do all this input wrangling without copying the
        # code buffer
        if code isa Core.SimpleVector
            # The C entry points will pass us this form.
            (ptr,len) = code
            code = String(unsafe_wrap(Array, ptr, len))
        end
        if !isnothing(_debug_log[])
            print(_debug_log[], """
                  #-#-#-------------------------------
                  # ENTER filename=$filename, lineno=$lineno, offset=$offset, options=$options"
                  #-#-#-------------------------------
                  """)
            write(_debug_log[], code)
        end

        io = IOBuffer(code)
        seek(io, offset)

        stream = ParseStream(io)
        rule = options === :all ? :toplevel : options
        if rule === :statement || rule === :atom
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
        parse!(stream; rule=rule)
        if rule === :statement
            bump_trivia(stream)
        end

        if any_error(stream)
            tree = build_tree(SyntaxNode, stream, wrap_toplevel_as_kind=K"None")
            _,err = _first_error(tree)
            # In the flisp parser errors are normally `Expr(:error, msg)` where
            # `msg` is a String. By using a ParseError for msg we can do fancy
            # error reporting instead.
            if last_byte(err) == lastindex(code)
                tag = _incomplete_tag(tree)
                # Here we replicate the particular messages 
                msg =
                    tag === :string  ? "incomplete: invalid string syntax"     :
                    tag === :comment ? "incomplete: unterminated multi-line comment #= ... =#" :
                    tag === :block   ? "incomplete: construct requires end"    :
                    tag === :cmd     ? "incomplete: invalid \"`\" syntax"      :
                    tag === :char    ? "incomplete: invalid character literal" :
                                       "incomplete: premature end of input"
                error_ex = Expr(:incomplete, msg)
            else
                error_ex = Expr(:error, ParseError(stream, filename=filename))
            end
            ex = options === :all ? Expr(:toplevel, error_ex) : error_ex
        else
            # TODO: Figure out a way to show warnings. Meta.parse() has no API
            # to communicate this, and we also can't show them to stdout as
            # this is too side-effectful and can result in double-reporting in
            # the REPL.
            #
            # show_diagnostics(stdout, stream.diagnostics, code)
            #
            # FIXME: Add support to lineno to this tree build (via SourceFile?)
            ex = build_tree(Expr, stream; filename=filename, wrap_toplevel_as_kind=K"None")
            if Meta.isexpr(ex, :None)
                # The None wrapping is only to give somewhere for trivia to be
                # attached; unwrap!
                ex = only(ex.args)
            end
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
        @error("JuliaSyntax parser failed — falling back to flisp!",
               exception=(exc,catch_backtrace()),
               offset=offset,
               code=code)

        _fl_parse_hook(code, filename, lineno, offset, options)
    end
end

# Hack:
# Meta.parse() attempts to construct a ParseError from a string if it receives
# `Expr(:error)`. Add an override to the ParseError constructor to prevent this.
# FIXME: Improve this in Base somehow?
Base.Meta.ParseError(e::JuliaSyntax.ParseError) = e

const _default_parser = VERSION < v"1.6" ? nothing : Core._parse

"""
    enable_in_core!([enable=true; freeze_world_age, debug_filename])

Connect the JuliaSyntax parser to the Julia runtime so that it replaces the
flisp parser for all parsing work. That is, JuliaSyntax will be used for
`include()` `Meta.parse()`, the REPL, etc. To disable, set use
`enable_in_core!(false)`.

Keyword arguments:
* `freeze_world_age` - Use a fixed world age for the parser to prevent
  recompilation of the parser due to any user-defined methods (default `true`).
* `debug_filename` - File name of parser debug log (defaults to `nothing` or
  the value of `ENV["JULIA_SYNTAX_DEBUG_FILE"]`).
"""
function enable_in_core!(enable=true; freeze_world_age = true,
        debug_filename   = get(ENV, "JULIA_SYNTAX_DEBUG_FILE", nothing))
    if VERSION < v"1.6"
        error("Cannot use JuliaSyntax as the main Julia parser in Julia version $VERSION < 1.6")
    end
    _parser_world_age[] = freeze_world_age ? Base.get_world_counter() : _latest_world
    if enable && !isnothing(debug_filename)
        _debug_log[] = open(debug_filename, "w")
    elseif !enable && !isnothing(_debug_log[])
        close(_debug_log[])
        _debug_log[] = nothing
    end
    _set_core_parse_hook(enable ? core_parser_hook : _default_parser)
    nothing
end


#-------------------------------------------------------------------------------
# Tools to call the reference flisp parser
#
# Call the flisp parser
function _fl_parse_hook(code, filename, lineno, offset, options)
    @static if VERSION >= v"1.8.0-DEV.1370" # https://github.com/JuliaLang/julia/pull/43876
        return Core.Compiler.fl_parse(code, filename, lineno, offset, options)
    elseif VERSION >= v"1.6"
        return Core.Compiler.fl_parse(code, filename, offset, options)
    else
        if options === :all
            ex = Base.parse_input_line(String(code), filename=filename, depwarn=false)
            if !Meta.isexpr(ex, :toplevel)
                ex = Expr(:toplevel, ex)
            end
            return ex, sizeof(code)
        elseif options === :statement || options == :atom
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

# Convenience functions to mirror `JuliaSyntax.parse(Expr, ...)` in simple cases.
fl_parse(::Type{Expr}, args...; kws...) = fl_parse(args...; kws...)
fl_parseall(::Type{Expr}, args...; kws...) = fl_parseall(args...; kws...)

