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
    highlight(io, src; note=exc.msg)
end

