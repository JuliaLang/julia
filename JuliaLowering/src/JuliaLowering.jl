module JuliaLowering

using JuliaSyntax

using JuliaSyntax: SyntaxHead, highlight, Kind, GreenNode, @KSet_str
using JuliaSyntax: haschildren, children, child, numchildren, head, kind, flags
using JuliaSyntax: filename, first_byte, last_byte, source_location, span

using JuliaSyntax: is_literal, is_number, is_operator, is_prec_assignment, is_infix_op_call, is_postfix_op_call, is_error

function _insert_kinds()
    JuliaSyntax.insert_kinds!(JuliaLowering, 1, [
        "BEGIN_LOWERING_KINDS"
            # Compiler metadata hints
            "meta"
            "extension"
            # A literal Julia value of any kind, as might be inserted by the AST
            # during macro expansion
            "Value"
            "inbounds"
            "inline"
            "noinline"
            "loopinfo"
            # Identifier for a value which is only assigned once
            "SSAValue"
            # Scope expressions `(hygienic_scope ex s)` mean `ex` should be
            # interpreted as being in scope `s`.
            "hygienic_scope"
            # Various heads harvested from flisp lowering.
            # (TODO: May or may not need all these - assess later)
            "break_block"
            "scope_block"
            "local_def"
            "_while"
            "_do_while"
            "with_static_parameters"
            "top"
            "core"
            "toplevel_butfirst"
            "thunk"
            "lambda"
            "moved_local"
            "the_exception"
            "foreigncall"
            "new"
            "globalref"
            "outerref"
            "enter"
            "leave"
            "label"
            "goto"
            "gotoifnot"
            "trycatchelse"
            "tryfinally"
            "method"
            "slot"
            "unnecessary"
            "decl"
        "END_LOWERING_KINDS"
    ])
end
_insert_kinds()

include("syntax_graph.jl")
include("utils.jl")

include("desugaring.jl")
include("scope_analysis.jl")
include("linear_ir.jl")

function __init__()
    _insert_kinds()
end

end
