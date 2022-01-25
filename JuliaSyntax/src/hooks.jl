# Adaptor for the API/ABI expected by the Julia runtime code.
function core_parser_hook(code, filename, offset, options)
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
        rule = options == :all ? :toplevel : options
        JuliaSyntax.parse(stream; rule=rule)

        ex = any_error(stream) ?
            Expr(:error, ParseError(SourceFile(code), stream.diagnostics)) :
            build_tree(Expr, stream)

        pos = last_byte(stream) - 1

        # Rewrap result in an svec for use by the C code
        return Core.svec(ex, pos)
    catch exc
        @error("JuliaSyntax parser failed — falling back to flisp!",
               exception=(exc,catch_backtrace()),
               offset=offset,
               code=code)
    end
    return Core.Compiler.fl_parse(code, filename, offset, options)
end

"""
Connect the JuliaSyntax parser to the Julia runtime so that it replaces the
flisp parser for all parsing work.

That is, JuliaSyntax will be used for `include()` `Meta.parse()`, the REPL, etc.
"""
function enable_in_core!()
    # TODO: Use invoke_in_world to freeze the world age at the time this was enabled.
    Base.eval(Core, :(_parse = $core_parser_hook))
    nothing
end

"""
Revert to the flisp parser for all parsing work.
"""
function disable_in_core!()
    Base.eval(Core, :(_parse = Core.Compiler.fl_parse))
    nothing
end

