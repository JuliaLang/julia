module JuliaLowering

using JuliaSyntax

using JuliaSyntax: SyntaxHead, highlight, Kind, GreenNode, @KSet_str
using JuliaSyntax: haschildren, children, child, numchildren, head, kind, flags, has_flags
using JuliaSyntax: filename, first_byte, last_byte, source_location, span

using JuliaSyntax: is_literal, is_number, is_operator, is_prec_assignment, is_infix_op_call, is_postfix_op_call, is_error

include("kinds.jl")
_insert_kinds()

include("syntax_graph.jl")
include("ast.jl")
include("utils.jl")

abstract type AbstractLoweringContext end

include("desugaring.jl")
include("scope_analysis.jl")
include("linear_ir.jl")

include("eval.jl")

function __init__()
    _insert_kinds()
end

end
