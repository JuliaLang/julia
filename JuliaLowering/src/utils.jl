# Error handling

TODO(msg::AbstractString) = throw(ErrorException("Lowering TODO: $msg"))
TODO(ex::SyntaxTree, msg="") = throw(LoweringError(ex, "Lowering TODO: $msg"))

# Errors found during lowering will result in LoweringError being thrown to
# indicate the syntax causing the error.
struct LoweringError <: Exception
    ex::SyntaxTree
    msg::String
end

function Base.showerror(io::IO, exc::LoweringError)
    print(io, "LoweringError:\n")
    src = sourceref(exc.ex)
    highlight(io, src.file, first_byte(src):last_byte(src), note=exc.msg)
end

function _chk_code(ex, cond)
    cond_str = string(cond)
    quote
        ex = $(esc(ex))
        @assert ex isa SyntaxTree
        try
            ok = $(esc(cond))
            if !ok
                throw(LoweringError(ex, "Expected `$($cond_str)`"))
            end
        catch
            throw(LoweringError(ex, "Structure error evaluating `$($cond_str)`"))
        end
    end
end

# Internal error checking macro.
# Check a condition involving an expression, throwing a LoweringError if it
# doesn't evaluate to true. Does some very simple pattern matching to attempt
# to extract the expression variable from the left hand side.
macro chk(cond)
    ex = cond
    while true
        if ex isa Symbol
            break
        elseif ex.head == :call
            ex = ex.args[2]
        elseif ex.head == :ref
            ex = ex.args[1]
        elseif ex.head == :.
            ex = ex.args[1]
        elseif ex.head in (:(==), :(in), :<, :>)
            ex = ex.args[1]
        else
            error("Can't analyze $cond")
        end
    end
    _chk_code(ex, cond)
end

macro chk(ex, cond)
    _chk_code(ex, cond)
end

