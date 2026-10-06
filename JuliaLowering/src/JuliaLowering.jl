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

using .JuliaSyntax: @stm, SourceAttrType, SourceRef,
    SyntaxList, SyntaxTree, byte_range, children, filename, first_byte,
    flattened_provenance, head, highlight,
    is_leaf, last_byte, mapchildren, mapsyntax, newleaf,
    newnode, node_string, numchildren, provenance, setmeta, setmeta!, getmeta,
    CompileHints, source_location, sourcefile, sourceref, mapindex, mktree,
    ScopeLayer, SyntaxContext, is_base_layer, base_layer, escape_layer,
    syntax_module, edition, is_flisp_compat, adopt_scope,
    remove_scope, fill_context, JL_NEW_EDITION, JL_OLD_EDITION

const DEBUG = true

# Falls back to `Union{}` so that `loc isa MacroSource` is always false on Julia < 1.14
# where `Core.MacroSource` is not defined.
const MacroSource = isdefinedglobal(Core, :MacroSource) ? Core.MacroSource : Union{}

const TypeEqOf = isdefinedglobal(Core, :TypeEqOf) ? "TypeEqOf" : "Typeof"

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

_include("precompile.jl")

end
