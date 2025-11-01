# Use a baremodule because we're implementing `include` and `eval`
baremodule JuliaLowering

using Base
# We define a separate _include() for use in this module to avoid mixing method
# tables with the public `JuliaLowering.include()` API
const _include = Base.IncludeInto(JuliaLowering)

if parentmodule(JuliaLowering) === Base
    using Base.JuliaSyntax
else
    using JuliaSyntax
end

using .JuliaSyntax: highlight, Kind, @KSet_str, is_leaf, children, numchildren,
    head, kind, flags, has_flags, numeric_flags, filename, first_byte,
    last_byte, byte_range, sourcefile, source_location, span, sourcetext,
    is_literal, is_number, is_operator, is_prec_assignment, is_prefix_call,
    is_infix_op_call, is_postfix_op_call, is_error

_include("kinds.jl")
_register_kinds()

_include("syntax_graph.jl")
_include("ast.jl")
_include("bindings.jl")
_include("utils.jl")

_include("macro_expansion.jl")
_include("desugaring.jl")
_include("scope_analysis.jl")
_include("closure_conversion.jl")
_include("linear_ir.jl")
_include("runtime.jl")
_include("syntax_macros.jl")

_include("eval.jl")
_include("compat.jl")
_include("hooks.jl")

function __init__()
    _register_kinds()
end

_include("precompile.jl")

end
