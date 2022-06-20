# Adaptor for the API/ABI expected by the Julia runtime code.
function core_parser_hook(code, filename, lineno, offset, options)
    try
        # TODO: Check that we do all this input wrangling without copying the
        # code buffer
        if code isa Core.SimpleVector
            # The C entry points will pass us this form.
            (ptr,len) = code
            code = String(unsafe_wrap(Array, ptr, len))
        end
        io = IOBuffer(code)
        seek(io, offset)

        stream = ParseStream(io)
        rule = options === :all ? :toplevel : options
        if rule !== :toplevel
            # To copy the flisp parser driver, we ignore leading trivia when
            # parsing statements or atoms
            bump_trivia(stream)
        end
        JuliaSyntax.parse(stream; rule=rule)

        if any_error(stream)
            e = Expr(:error, ParseError(SourceFile(code), stream.diagnostics))
            ex = options === :all ? Expr(:toplevel, e) : e
        else
            ex = build_tree(Expr, stream, wrap_toplevel_as_kind=K"None")
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

        # Rewrap result in an svec for use by the C code
        return Core.svec(ex, last_offset)
    catch exc
        @error("JuliaSyntax parser failed — falling back to flisp!",
               exception=(exc,catch_backtrace()),
               offset=offset,
               code=code)
    end
    return Core.Compiler.fl_parse(code, filename, offset, options)
end

# Core._parse gained a `lineno` argument in
# https://github.com/JuliaLang/julia/pull/43876
# Prior to this, the following signature was needed:
function core_parser_hook(code, filename, offset, options)
    core_parser_hook(code, filename, LineNumberNode(0), offset, options)
end

# Hack:
# Meta.parse() attempts to construct a ParseError from a string if it receives
# `Expr(:error)`.
Base.Meta.ParseError(e::JuliaSyntax.ParseError) = e

"""
Connect the JuliaSyntax parser to the Julia runtime so that it replaces the
flisp parser for all parsing work.

That is, JuliaSyntax will be used for `include()` `Meta.parse()`, the REPL, etc.
"""
function enable_in_core!(enable=true)
    parser = enable ? core_parser_hook : Core.Compiler.fl_parse
    Base.eval(Core, :(_parse = $parser))
    nothing
end

