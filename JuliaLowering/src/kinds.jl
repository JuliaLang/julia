
# The following kinds are used in intermediate forms by lowering but are not
# part of the surface syntax
function _register_kinds()
    JuliaSyntax.register_kinds!(JuliaLowering, 1, [
        # "Syntax extensions" - expression kinds emitted by macros or macro
        # expansion, and known to lowering. These are part of the AST API but
        # without having surface syntax.
        "BEGIN_EXTENSION_KINDS"
            # atomic fields or accesses (see `@atomic`)
            "atomic"
            # A literal Julia value of any kind, as might be inserted into the
            # AST during macro expansion
            "Value"
            # A (quoted) `Symbol`
            "Symbol"
            # Compiler metadata hints
            "meta"
            # TODO: Use `meta` for inbounds and loopinfo etc?
            "inbounds"
            "inline"
            "noinline"
            "loopinfo"
            # Call into foreign code. Emitted by `@ccall`
            "foreigncall"
            # Test whether a variable is defined
            "isdefined"
            # named labels for `@label` and `@goto`
            "symbolic_label"
            # Goto named label
            "symbolic_goto"
            # Internal initializer for struct types, for inner constructors/functions
            "new"
            "splatnew"
            # Catch-all for additional syntax extensions without the need to
            # extend `Kind`. Known extensions include:
            #   locals, islocal
            "extension"
        "END_EXTENSION_KINDS"

        # The following kinds are internal to lowering
        "BEGIN_LOWERING_KINDS"
            # Semantic assertions used by lowering. The content of an assertion
            # is not considered to be quoted, so use K"Symbol" etc inside where necessary.
            "assert"
            # Unique identifying integer for bindings (of variables, constants, etc)
            "BindingId"
            # Various heads harvested from flisp lowering.
            # (TODO: May or may not need all these - assess later)
            "break_block"
            "scope_block"
            "local_def" # TODO: Replace with K"local" plus BindingFlags attribute?
            "_while"
            "_do_while"
            "_typevars" # used for supplying already-allocated `TypeVar`s to `where`
            "with_static_parameters"
            "top"
            "core"
            "lambda"
            # [K"function_decl" name]
            # Declare a zero-method generic function with global `name` or
            # creates a closure object and assigns it to the local `name`.
            "function_decl"
            # [K"function_type name]
            # Evaluates to the type of the function or closure with given `name`
            "function_type"
            # [K"method_defs" name block]
            # The code in `block` defines methods for generic function `name`
            "method_defs"
            # The enclosed statements must be executed at top level
            "toplevel_butfirst"
            "const_if_global"
            "moved_local"
            "label"
            "trycatchelse"
            "tryfinally"
            "unnecessary"
            "decl"
            # [K"captured_local" index]
            # A local variable captured into a global method. Contains the
            # `index` of the associated `Box` in the rewrite list.
            "captured_local"
        "END_LOWERING_KINDS"

        # The following kinds are emitted by lowering and used in Julia's untyped IR
        "BEGIN_IR_KINDS"
            # Identifier for a value which is only assigned once
            "SSAValue"
            # Local variable in a `CodeInfo` code object (including lambda arguments)
            "slot"
            # Static parameter to a `CodeInfo` code object ("type parameters" to methods)
            "static_parameter"
            # Reference to a global variable within a module
            "globalref"
            # Unconditional goto
            "goto"
            # Conditional goto
            "gotoifnot"
            # Exception handling
            "enter"
            "leave"
            "pop_exception"
            # Lowering targets for method definitions arising from `function` etc
            "method"
            # (re-)initialize a slot to undef
            # See Core.NewvarNode
            "newvar"
            # Result of lowering a `K"lambda"` after bindings have been
            # converted to slot/globalref/SSAValue.
            "code_info"
        "END_IR_KINDS"
    ])
end
