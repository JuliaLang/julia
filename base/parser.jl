## interface to parser ##

function parse(str::AbstractString, pos::Int; greedy::Bool=true, raise::Bool=true)
    # returns (expr, end_pos). expr is () in case of parse error.
    bstr = bytestring(str)
    ex, pos = ccall(:jl_parse_string, Any,
                    (Ptr{UInt8}, Csize_t, Int32, Int32),
                    bstr, sizeof(bstr), pos-1, greedy ? 1:0)
    if raise && isa(ex,Expr) && is(ex.head,:error)
        throw(ParseError(ex.args[1]))
    end
    if ex == ()
        raise && throw(ParseError("end of input"))
        ex = Expr(:error, "end of input")
    end
    ex, pos+1 # C is zero-based, Julia is 1-based
end

function parse(str::AbstractString; raise::Bool=true)
    ex, pos = parse(str, start(str), greedy=true, raise=raise)
    if isa(ex,Expr) && ex.head === :error
        return ex
    end
    if !done(str, pos)
        raise && throw(ParseError("extra token after end of expression"))
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end
