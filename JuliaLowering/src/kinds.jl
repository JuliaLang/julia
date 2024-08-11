
# The following kinds are used in intermediate forms by lowering but are not
# part of the surface syntax
function _register_kinds()
    JuliaSyntax.register_kinds!(JuliaLowering, 1, [
        "BEGIN_LOWERING_KINDS"
            # Compiler metadata hints
            "meta"
            "extension"
            # Semantic assertions used by lowering. The content of an assertion
            # is not considered to be quoted, so use K"Symbol" inside where necessary.
            "assert"
            # A literal Julia value of any kind, as might be inserted by the AST
            # during macro expansion
            "Value"
            # A (quoted) `Symbol`
            "Symbol"
            # TODO: Use `meta` for inbounds and loopinfo etc?
            "inbounds"
            "inline"
            "noinline"
            "loopinfo"
            # Identifier for a value which is only assigned once
            "SSAValue"
            # Unique identifying integer for bindings (of variables, constants, etc)
            "BindingId"
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
            "symbolic_label"
            "symbolic_goto"
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
