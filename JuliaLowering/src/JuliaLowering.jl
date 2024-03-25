module JuliaLowering

using JuliaSyntax

using JuliaSyntax: SyntaxHead, highlight, Kind, GreenNode, @KSet_str
using JuliaSyntax: haschildren, children, child, numchildren, head, kind, flags
using JuliaSyntax: filename, first_byte, last_byte, source_location

using JuliaSyntax: is_literal, is_number, is_operator, is_prec_assignment, is_infix_op_call, is_postfix_op_call

include("syntax_graph.jl")
include("utils.jl")

include("desugaring.jl")
include("scope_analysis.jl")
include("linear_ir.jl")

end
