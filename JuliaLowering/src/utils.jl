# Error handling

TODO(msg) = throw(ErrorException("Lowering TODO: $msg"))
TODO(ex, msg) = throw(LoweringError(ex, "Lowering TODO: $msg"))

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


#-------------------------------------------------------------------------------
# CodeInfo constructor. TODO: Should be in Core?
function _CodeInfo(code,
         codelocs,
         ssavaluetypes,
         ssaflags,
         method_for_inference_limit_heuristics,
         linetable,
         slotnames,
         slotflags,
         slottypes,
         rettype,
         parent,
         edges,
         min_world,
         max_world,
         inferred,
         propagate_inbounds,
         has_fcall,
         nospecializeinfer,
         inlining,
         constprop,
         purity,
         inlining_cost)
    @eval $(Expr(:new, :(Core.CodeInfo),
           convert(Vector{Any}, code),
           convert(Vector{Int32}, codelocs),
           convert(Any, ssavaluetypes),
           convert(Vector{UInt32}, ssaflags),
           convert(Any, method_for_inference_limit_heuristics),
           convert(Any, linetable),
           convert(Vector{Symbol}, slotnames),
           convert(Vector{UInt8}, slotflags),
           convert(Any, slottypes),
           convert(Any, rettype),
           convert(Any, parent),
           convert(Any, edges),
           convert(UInt64, min_world),
           convert(UInt64, max_world),
           convert(Bool, inferred),
           convert(Bool, propagate_inbounds),
           convert(Bool, has_fcall),
           convert(Bool, nospecializeinfer),
           convert(UInt8, inlining),
           convert(UInt8, constprop),
           convert(UInt16, purity),
           convert(UInt16, inlining_cost)))
end

