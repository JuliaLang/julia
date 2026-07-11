module JuliaSyntax

macro _public(syms)
    if VERSION >= v"1.11"
        names = syms isa Symbol ? [syms] : syms.args
        esc(Expr(:public, names...))
    else
        nothing
    end
end

# Public API, in the order of docs/src/api.md

# Parsing.
export parseall, parseatom, parsestmt

@_public parse!,
    ParseStream,
    build_tree

# Tokenization
export Token, tokenize, untokenize

# Source file handling
@_public sourcefile,
    byte_range,
    char_range,
    first_byte,
    last_byte,
    filename,
    source_line,
    source_location,
    sourcetext,
    highlight

export SourceFile
@_public source_line_range

# Expression predicates, kinds and flags
export @K_str, kind
@_public Kind
@_public PrecedenceLevel, PREC_NONE, PREC_ASSIGNMENT,
    PREC_PAIRARROW, PREC_CONDITIONAL, PREC_ARROW, PREC_LAZYOR, PREC_LAZYAND,
    PREC_COMPARISON, PREC_PIPE_LT, PREC_PIPE_GT, PREC_COLON, PREC_PLUS,
    PREC_BITSHIFT, PREC_TIMES, PREC_RATIONAL, PREC_POWER, PREC_DECL,
    PREC_WHERE, PREC_DOT, PREC_QUOTE, PREC_UNICODE_OPS, PREC_COMPOUND_ASSIGN,
    generic_operators_by_level

@_public flags,
    SyntaxHead,
    head,
    is_trivia,
    is_prefix_call,
    is_infix_op_call,
    is_prefix_op_call,
    is_postfix_op_call,
    is_dotted,
    is_decorated,
    numeric_flags,
    has_flags,
    TRIPLE_STRING_FLAG,
    RAW_STRING_FLAG,
    PARENS_FLAG,
    COLON_QUOTE,
    TOPLEVEL_SEMICOLONS_FLAG,
    MUTABLE_FLAG,
    BARE_MODULE_FLAG,
    SHORT_FORM_FUNCTION_FLAG

# Syntax trees
@_public is_leaf,
    numchildren,
    children

export SyntaxNode

@_public GreenNode, RedTreeCursor, GreenTreeCursor,
    span

# The generic tree porcelain lives in UnifiedIR (unifiedir-design.md §3.7
# Level 1): SyntaxTree/SyntaxList are aliases of UnifiedIR.Tree/NodeList and
# the tree functions below are UnifiedIR generics (JuliaSyntax adds methods
# for its own types — SyntaxNode, GreenNode, tree cursors — and the
# genuinely syntax-specific conventions: the Kind registry, SourceRef text
# machinery, and the leaf payload convention).
if parentmodule(@__MODULE__) === Base
    # bootstrapped into Base (base/Base.jl includes UnifiedIR just before us)
    import ..UnifiedIR
else
    import UnifiedIR
end
import .UnifiedIR: Tree, NodeList, children, numchildren, is_leaf, child,
    mapchildren, mknode, mkleaf, mktree, newnode, newleaf, copy_ast, copy_attrs!,
    provenance, prov, prov_end, provenance_terminal, reparent, syntax_graph,
    tree_ids, mapsyntax, mapindex, setattr!, setattr, deleteattr!, hasattr,
    getattr, attrnames, node_string, new_id!, setchildren!,
    check_same_graph, check_compatible_graph, is_compatible_graph,
    foldtree, print_tree

# Helper utilities
include("utils.jl")

include("julia/kinds.jl")

# Lexing uses a significantly modified version of Tokenize.jl
include("julia/tokenize.jl")

# Source and diagnostics
include("core/source_files.jl")
include("core/diagnostics.jl")

# Parsing
include("core/parse_stream.jl")
include("core/tree_cursors.jl")
include("julia/julia_parse_stream.jl")
include("julia/parser.jl")
include("julia/parser_api.jl")
include("julia/literal_parsing.jl")

# Tree data structures
include("porcelain/green_node.jl")
include("porcelain/syntax_node.jl")
include("integration/expr.jl")
if VERSION >= v"1.12"
    # SyntaxGraph runs on the UnifiedIR AttrGraph substrate (§3.7 Level 1)
    include("porcelain/syntax_graph.jl")
end

# Hooks to integrate the parser with Base
include("integration/hooks.jl")

function __init__()
    # Kinds live in the shared UnifiedIR registry (session state, not part of
    # this pkgimage): re-register at load. Deterministic numbering (reserved
    # dialect id, fixed list order) keeps precompiled K"..." constants valid.
    _register_syntax_kinds()
end

include("precompile.jl")

end
