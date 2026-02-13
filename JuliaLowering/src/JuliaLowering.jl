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
    head, kind, flags, has_flags, filename, first_byte, last_byte, byte_range,
    sourcefile, source_location, span, sourcetext, is_literal, is_infix_op_call,
    is_postfix_op_call, @isexpr, SyntaxHead, is_syntactic_operator,
    is_contextual_keyword,
    SyntaxGraph, SyntaxTree, SyntaxList, NodeId, SourceRef, SourceAttrType,
    ensure_attributes, ensure_attributes!, delete_attributes, new_id!, hasattr,
    copy_attrs, setattr, setattr!, syntax_graph, is_compatible_graph,
    check_compatible_graph, copy_node, copy_ast, provenance, sourceref,
    reparent, mapchildren, flattened_provenance, mkleaf, mknode, newleaf,
    newnode, tree_ids, @stm, mapsyntax

const DEBUG = true

_include("kinds.jl")
_register_kinds()

_include("ast.jl")
_include("bindings.jl")
_include("utils.jl")
_include("validation.jl")

_include("macro_expansion.jl")
_include("desugaring.jl")
_include("scope_analysis.jl")
_include("binding_analysis.jl")
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
