# This file provides an adaptor to match the API expected by the Julia runtime
# code in the binding Core._parse

if isdefined(Core, :set_parser)
    const _set_core_parse_hook = Core.set_parser
else
    function _set_core_parse_hook(parser)
        # HACK! Fool the runtime into allowing us to set Core._parse, even during
        # incremental compilation. (Ideally we'd just arrange for Core._parse to be
        # set to the JuliaSyntax parser. But how do we signal that to the dumping
        # code outside of the initial creation of Core?)
        i = findfirst(==(:incremental), fieldnames(Base.JLOptions))
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
const _caller_world = typemax(UInt)
const _parser_world_age = Ref{UInt}(_caller_world)

function core_parser_hook(code, filename, lineno, offset, options)
    # `hook` is always _core_parser_hook, but that's hidden from the compiler
    # via a Ref to prevent invalidation / recompilation when other packages are
    # loaded. This wouldn't seem like it should be necessary given the use of
    # invoke_in_world, but it is in Julia-1.7.3. I'm not sure exactly which
    # latency it's removing.
    hook = _core_parser_hook_ref[]
    if _parser_world_age[] != _caller_world
        Base.invoke_in_world(_parser_world_age[], hook,
                             code, filename, lineno, offset, options)
    else
        hook(code, filename, lineno, offset, options)
    end
end

# Core._parse gained a `lineno` argument in
# https://github.com/JuliaLang/julia/pull/43876
# Prior to this, the following signature was needed:
function core_parser_hook(code, filename, offset, options)
    core_parser_hook(code, filename, LineNumberNode(0), offset, options)
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
        JuliaSyntax.parse(stream; rule=rule)
        if rule === :statement
            bump_trivia(stream)
        end

        if any_error(stream)
            e = Expr(:error, ParseError(SourceFile(code, filename=filename), stream.diagnostics))
            ex = options === :all ? Expr(:toplevel, e) : e
        else
            ex = build_tree(Expr, stream, filename=filename, wrap_toplevel_as_kind=K"None")
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

        if VERSION >= v"1.8.0-DEV.1370" # https://github.com/JuliaLang/julia/pull/43876
            return Core.Compiler.fl_parse(code, filename, lineno, offset, options)
        else
            return Core.Compiler.fl_parse(code, filename, offset, options)
        end
    end
end

# Hack:
# Meta.parse() attempts to construct a ParseError from a string if it receives
# `Expr(:error)`. Add an override to the ParseError constructor to prevent this.
# FIXME: Improve this in Base somehow?
Base.Meta.ParseError(e::JuliaSyntax.ParseError) = e

const _default_parser = Core._parse
# NB: Never reassigned, but the compiler doesn't know this!
const _core_parser_hook_ref = Ref{Function}(_core_parser_hook)

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
    _parser_world_age[] = freeze_world_age ? Base.get_world_counter() : _caller_world
    if enable && !isnothing(debug_filename)
        _debug_log[] = open(debug_filename, "w")
    elseif !enable && !isnothing(_debug_log[])
        close(_debug_log[])
        _debug_log[] = nothing
    end
    _set_core_parse_hook(enable ? core_parser_hook : _default_parser)
    nothing
end

