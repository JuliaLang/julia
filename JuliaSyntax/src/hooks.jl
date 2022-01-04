# Error type for displaying errors in the Julia REPL
struct ParseError <: Exception
    code::String
    stream::ParseStream
end

function Base.showerror(io::IO, err::ParseError, bt; backtrace=false)
    show_diagnostics(io, err.stream, err.code)
end
Base.display_error(io::IO, err::ParseError, bt) = Base.showerror(io, err, bt)

function Base.showerror(io::IO, err::ParseError)
    show_diagnostics(io, err.stream, err.code)
end

# Adaptor for the API/ABI expected by the Julia runtime code.
function core_parser_hook(code, filename, offset, options)
    try
        if code isa Core.SimpleVector # May be passed in from C entry points
            (ptr,len) = code
            code = String(unsafe_wrap(Array, ptr, len))
        end

        code = code[offset+1:end]  # FIXME!!

        stream = ParseStream(code)
        if options === :atom
            parse_atom(ParseState(stream))
        elseif options === :statement
            parse_stmts(ParseState(stream))
        elseif options === :all
            parse_all(stream)
        end

        if !isempty(stream.diagnostics)
            ex = Expr(:error, ParseError(code, stream))
        else
            green_tree = build_tree(GreenNode, stream)
            src = SourceFile(code; filename=filename)
            tree = SyntaxNode(src, green_tree)
            ex = Expr(tree)
        end
        pos = offset + stream.next_byte-1

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

