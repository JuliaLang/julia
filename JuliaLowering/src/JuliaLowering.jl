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

using .JuliaSyntax: @KSet_str, @stm, Kind, NodeId, SourceAttrType, SourceRef, SyntaxGraph,
    SyntaxList, SyntaxTree, byte_range, check_compatible_graph, children, copy_ast,
    copy_attrs, copy_node, delete_attributes, ensure_attributes!, filename, first_byte,
    flags, flattened_provenance, has_flags, hasattr, head, highlight, is_compatible_graph,
    is_leaf, is_literal, kind, last_byte, mapchildren, mapsyntax, mkleaf, mknode, newleaf,
    newnode, node_string, numchildren, provenance, reparent, setattr, setattr!,
    source_location, sourcefile, sourceref, syntax_graph, tree_ids

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
