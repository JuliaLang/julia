# Use a baremodule because we're implementing `include` and `eval`
baremodule JuliaLowering

using Base

# We define a separate _include() for use in this module to avoid mixing method
# tables with the public `JuliaLowering.include()` API
const _include = Base.IncludeInto(JuliaLowering)

if parentmodule(JuliaLowering) === Base
    using Base.JuliaSyntax
    const BaseSyntax = Base.BaseSyntax
else
    using JuliaSyntax
    using BaseSyntax: BaseSyntax
end

using .BaseSyntax: ScopeLayer, SyntaxContext, SourceCode, SourceRef, Syntax, MacroContext,
    SourceAttrType, head, flattened_provenance, sourceref, unexpanded_sourceref,
    mapchildren, provenance, DEBUG_LOWERING,
    is_base_layer, base_layer, escape_layer, remove_scope, fill_context,
    trim_provenance,
    syntax_module, edition, adopt_scope, assert_syntax, @mknode, macro_prov,
    isa_lowering_ast_node, filename, source_line, first_linenode

using .JuliaSyntax: children, first_byte, highlight, is_leaf,
    last_byte, numchildren, source_location


const DEBUG = DEBUG_LOWERING
# const DEBUG = isdefinedglobal(Base, :DEBUG_LOWERING) ?
#     Base.DEBUG_LOWERING : true

const JL_NEW_EDITION = BaseSyntax.EXPERIMENTAL_EDITION # TODO: set this to (1, 15)
const JL_OLD_EDITION = BaseSyntax.FLISP_EDITION

# Falls back to `Union{}` so that `loc isa MacroSource` is always false on Julia < 1.14
# where `Core.MacroSource` is not defined.
const MacroSource = isdefinedglobal(Core, :MacroSource) ? Core.MacroSource : Union{}

const TypeEqOf = isdefinedglobal(Core, :TypeEqOf) ? "TypeEqOf" : "Typeof"

# `Base.isa_ast_node` is not defined on Julia 1.12
@static if isdefinedglobal(Base, :isa_ast_node)
    using Base: isa_ast_node
else
    function isa_ast_node(@nospecialize x)
        return x isa Core.NewvarNode ||
               x isa Core.CodeInfo ||
               x isa LineNumberNode ||
               x isa Core.GotoNode ||
               x isa Core.GotoIfNot ||
               x isa Core.EnterNode ||
               x isa Core.ReturnNode ||
               x isa Core.SSAValue ||
               x isa Core.SlotNumber ||
               x isa Core.Argument ||
               x isa QuoteNode ||
               x isa GlobalRef ||
               x isa Core.BindingPartition ||
               x isa Symbol ||
               x isa Core.PiNode ||
               x isa Core.PhiNode ||
               x isa Core.PhiCNode ||
               x isa Core.UpsilonNode ||
               x isa Expr
    end
end

# todo: remove
const SyntaxTree = Syntax
const IdTag = Int
SyntaxList(rest::SyntaxTree...) = SyntaxTree[rest...]
# Type of `ex[i:j]`
const SyntaxView = SubArray{SyntaxTree, 1, Vector{SyntaxTree}, Tuple{UnitRange{Int}}, true}

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
