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
            # Flag for @generated parts of a functon
            "generated"
            # Temporary rooting of identifiers (GC.@preserve)
            "gc_preserve_begin"
            "gc_preserve_end"
            # A literal Julia value of any kind, as might be inserted into the
            # AST during macro expansion
            "Value"
            # A (quoted) `Symbol`
            "Symbol"
            # QuoteNode; not quasiquote
            "inert"
            # Compiler metadata hints
            "meta"
            # TODO: Use `meta` for inbounds and loopinfo etc?
            "inbounds"
            "boundscheck"
            "inline"
            "noinline"
            "loopinfo"
            # Call into foreign code. Emitted by `@ccall`
            "foreigncall"
            # Special form for constructing a function callable from C
            "cfunction"
            # Special form emitted by `Base.Experimental.@opaque`
            "opaque_closure"
            # Test whether a variable is defined
            "isdefined"
            # [K"throw_undef_if_not" var cond]
            # This form is used internally in Core.Compiler but might be
            # emitted by packages such as Diffractor. In principle it needs to
            # be passed through lowering in a similar way to `isdefined`
            "throw_undef_if_not"
            # named labels for `@label` and `@goto`
            "symbolic_label"
            # Goto named label
            "symbolic_goto"
            # Internal initializer for struct types, for inner constructors/functions
            "new"
            "splatnew"
            # Used for converting `esc()`'d expressions arising from old macro
            # invocations during macro expansion (gone after macro expansion)
            "escape"
            # Used for converting the old-style macro hygienic-scope form (gone
            # after macro expansion)
            "hygienic_scope"
            # An expression which will eventually be evaluated "statically" in
            # the context of a CodeInfo and thus allows access only to globals
            # and static parameters. Used for ccall, cfunction, cglobal
            # TODO: Use this for GeneratedFunctionStub also?
            "static_eval"
            # Catch-all for additional syntax extensions without the need to
            # extend `Kind`. Known extensions include:
            #   locals, islocal
            # The content of an assertion is not considered to be quoted, so
            # use K"Symbol" or K"inert" inside where necessary.
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
            # Like block, but introduces a lexical scope; used during scope resolution.
            "scope_block"
            # [K"always_defined" x] is an assertion that variable `x` is assigned before use
            # ('local-def in flisp implementation is K"local" plus K"always_defined"
            "always_defined"
            "_while"
            "_do_while"
            "_typevars" # used for supplying already-allocated `TypeVar`s to `where`
            "with_static_parameters"
            "top"
            "core"
            "lambda"
            # "A source location literal" - a node which exists only to record
            # a sourceref
            "SourceLocation"
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
            # The code in `block` defines methods for generic function `name`
            "_opaque_closure"
            # The enclosed statements must be executed at top level
            "toplevel_butfirst"
            "assign_or_constdecl_if_global"
            "moved_local"
            "label"
            "trycatchelse"
            "tryfinally"
            # The contained block of code causes no side effects and can be
            # removed by a later lowering pass if its value isn't used.
            # (That is, it's removable in the same sense as
            #  `@assume_effects :removable`.)
            "removable"
            "decl"
            # [K"captured_local" index]
            # A local variable captured into a global method. Contains the
            # `index` of the associated `Box` in the rewrite list.
            "captured_local"
            # Causes the linearization pass to conditionally emit a world age increment
            "latestworld_if_toplevel"
        "END_LOWERING_KINDS"

        # The following kinds are emitted by lowering and used in Julia's untyped IR
        "BEGIN_IR_KINDS"
            # Identifier for a value which is only assigned once
            "SSAValue"
            # Local variable in a `CodeInfo` code object (including lambda arguments)
            "slot"
            # Static parameter to a `CodeInfo` code object ("type parameters" to methods)
            "static_parameter"
            # References/declares a global variable within a module
            "globalref"
            "globaldecl"
            # Two-argument constant declaration and assignment.
            # Translated to :const in the IR for now (we use K"const" already in parsing).
            "constdecl"
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
            # Internal initializer for opaque closures
            "new_opaque_closure"
            # Wrapper for the lambda of around opaque closure methods
            "opaque_closure_method"
            # World age increment
            "latestworld"
        "END_IR_KINDS"
    ])
end
