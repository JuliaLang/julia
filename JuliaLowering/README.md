# JuliaLowering

JuliaLowering.jl is an implementation of Julia's macro expansion and lowering
passes.  It's intended to replace the original femtolisp implementation of macro
expansion and lowering, but it's not active by default yet.

## Goals

* Bring automatic hygiene to Julia macros
* Preserve code provenance through lowering (more than just line number nodes).
  * Lowered IR should be traceable to the corresponding lowering input (AST).
    Combining this with the AST-to-source text provenance from JuliaSyntax lets
    us produce byte-precise error messages in lowering and downstream.
  * Give each expression its own provenance rather than tracking provenance per
    statement.
* Retain compatibility (albeit not bug-for-bug compatibility) with existing code
  and macros

## Trying it out

```julia
# ./julia --project=./JuliaLowering
julia> using Pkg; Pkg.instantiate(); using JuliaLowering

julia> JuliaLowering.include_string(Main, """
       macro macro_with_no_escape(x)
           x
       end

       global @macro_with_no_escape(g) = 1
       @show g
       """)
g = 1
1

julia> JuliaLowering.include_string(Main, """
       function f(x,(y,z)=(1,2),(a,b))
       end
       """)
ERROR: LoweringError:
function f(x,(y,z)=(1,2),(a,b))
#                        └───┘ ── all function parameters after an optional parameter must also be optional
end

julia> JuliaLowering.include_string(Main, """
       function f(x,(y,z)=(1,2),(a,b)=(3,4))
           function g(@nospecialize(x:Int))
           end
       end
       """)
ERROR: LoweringError:
function f(x,(y,z)=(1,2),(a,b)=(3,4))
    function g(@nospecialize(x:Int))
#                            └───┘ ── expected identifier or `identifier::type`
    end
end
```

You can also activate JuliaLowering as default in the REPL:

```
julia> @activate JuliaLowering
```

Further input will be lowered with JuliaLowering, but note that all the good
things about doing so are TODO, since parser output takes a round-trip through
`Expr` (the current focus is finding and fixing lowering bugs where that doesn't
matter).

## Syntax type

JuliaLowering uses a new type for syntax (`SyntaxTree`), which is similar to the
existing `Expr`, but:
1. Every node is of the same type.  With `expr::Expr`, you may have
   `expr.args[1]::Symbol` with `string(expr.args[1]) === "foo"`.  In
   JuliaLowering, the equivalent would be `kind(syntax[1]) === K"Identifier"`
   with `syntax_name(syntax[1]) === "foo"`.
2. For code provenance reasons, it contains a reference to the syntax it was
   lowered from.  This forms a linked list back to either a LineNumberNode (if
   this was converted from an Expr) or a JuliaSyntax structure containing source
   text information.  This information does not affect lowering.
3. For macro hygiene, syntax versioning, and code provenance reasons, it
   contains a reference to a "syntax context", which is internal to lowering and
   unique per macro expansion and top-level thunk.

This is like a stripped-down version of Racket's [syntax
objects](https://docs.racket-lang.org/reference/syntax-model.html), which are
also a layer atop simple symbols and expressions.  Compared to Racket, though:
- We don't put lexical scope information in the AST
- Racket uses a ["sets of scopes"](https://www-old.cs.utah.edu/plt/scope-sets/)
  model for macro hygiene.  We only use a single "scope" per piece of syntax for
  macro hygiene (JuliaLowering calls these scopes "layers" to distinguish them
  from lexical scopes).

This syntax type is the argument and return type for hygienic "new" macros.
Quoting with `quote` and `:` will continue to produce `Expr` and `Symbol` like
before, but a new kind of quote/unquote producing `SyntaxTree` may be added.
For now, `JuliaLowering.@legacy_quote_to_syntax :(x + $y)` can be used instead
with some limitations.

This syntax type can represent exactly the same syntax as `Expr` can, but with
additional hygiene that may change semantics.  `Expr` syntax converted to
`SyntaxTree` must be able to round-trip to the exact same `Expr` syntax as
macros see it.  (Within reason, LineNumberNodes may differ.) `SyntaxTree` should
similarly be able to round-trip through `Expr` with degraded provenance,
assuming the tree contains zero or one hygiene layers.  (Erasing hygiene by
converting a tree with >1 hygiene layer should be an error.)

## New macros with automatic hygiene

Julia's old (pre-JuliaLowering) macros are already partially hygienic according
to the usual definition of macro hygiene: an expansion shouldn't accidentally
introduce or reference names from the calling context [1].  Unfortunately, the
old macro expander does this by using the macro-definition context for the
entire expansion, including parts of the expansion that were passed by the
macro-caller context.  This means almost every useful macro needs to wrap
arguments (or whatever pieces of them show up in the expansion) in `escape`
nodes.  Worse yet, `escape` nodes change the AST structure, and may be passed to
inner macrocalls, so any macro that takes input needs to unwrap and rewrap every
piece of syntax it observes to correctly handle hygiene, which doesn't happen in
practice ([Julia issue #37691](https://github.com/JuliaLang/julia/issues/37691)).

As mentioned above, JuliaLowering gives every piece of syntax a "syntax context"
containing a hygiene layer before expansion.  A new layer is generated per macro
expansion, but is only applied to "new" syntax in the expansion.  For example,
the argument to `@addone` retains its local scope or module in the new expander,
but behaves badly in the old expander:

```julia
# A macro like this is likely making a mistake by not escaping `x`
module M
macro addone(x)
    :(let y = 1
        $x + y
    end)
end
end

# Old expander produces `(+ M.x_in mangled_y)` == UndefVarError
@macroexpand let x_in = 0
    M.@addone x_in
end

# Old expander produces `(+ mangled_y mangled_y)` == 0
@macroexpand let y = 0
    M.@addone y
end
```

Later, in scope resolution, the `(name, layer)` pair is used instead of just the
name (the old expander attempts to mangle the name).  The end result is that the
vast majority of escaping done in old `Expr` macros is no longer necessary in
the equivalent new-style macros.

Unhygienic macros (any macro where calling `@define_something` defines some
local visible to the calling context, with a name not passed to the call) are
useful too.  Like in the old system, breaking hygiene in the new system is
opt-in.

[1] "partially hygienic" because the reverse is not true.

## AST input specification, error messages

A new AST validator is available in [validation.jl](./src/validation.jl).  This
runs right before desugaring, since a malformed AST before that stage could be
fixed by some macro at any time, so it can't be validated.

AST validation is partially to make the rest of lowering easier (allowing the
transformation passes to assume the code's structure), but it also serves as
documentation of the Julia AST.  Some lowering errors, but not many, are thrown
from later in lowering.

`LoweringError` with `internal=false` is thrown for all bad user input,
including malformed AST.  Lowering-internal assertions use `@jl_assert`, which
can be compiled out (this also throws `LoweringError`, but with
`internal=true`).

## Compatibility

Assuming it doesn't depend on a bug or internal piece of flisp lowering, all
lowering input should continue to work under JuliaLowering.

JuliaLowering doesn't guarantee (and usually doesn't produce) the same IR as
flisp given the same input.

Macro compatibility is less simple to implement, but the same "old code
continues to work" guarantee applies.
- The syntax version of the `macro ... end` definition determines which of the
  two signatures it has.  Lowering with flisp, all syntax has version
  `JL_OLD_SYNTAX_VERSION` and the `Expr` signature.
- A macro author can choose to implement their macro with the new signature, the
  old signature, or both (but is responsible for both being equivalent if so).
  Adding a new macro to a project running with the old syntax version would look
  something like this:
  ```julia
  macro m(x)
      # x::Expr
      esc(x.args[1])
  end
  @syntax_version some_version_number macro m(x)
      # x::SyntaxTree
      x[1]
  end
  ```
- If no macro with the new signature exists, JuliaLowering converts all macro
  arguments to `Expr`/`Symbol`/etc. syntax, expands the old macro, and converts
  the expansion back to `SyntaxTree` with degraded provenance and all syntax
  with version `JL_OLD_SYNTAX_VERSION`.

JuliaLowering should guarantee that replacing an `Expr` macro with an equivalent
`SyntaxTree` one (with or without deleting the old macro) doesn't break existing
callers of the macro running with JuliaLowering.  Of course, deleting an
existing `Expr` macro may break your code on older Julia versions using flisp,
and not all `SyntaxTree` macros can be written as `Expr` macros.

Macro compatibility and the way of creating `SyntaxTree` are still under
development.  This document will be updated if one of the promises or interfaces
above changes.

### Why do we need to rewrite macros to get provenance and automatic hygiene?

Ideally, `Expr` would continue to work, `esc` could be made a no-op in places
JuliaLowering auto-escapes, provenance flows freely, and macro authors live
happily ever after---what gives?

1. Adding fields to `Expr` to carry the same information as `SyntaxTree` is not
   out of the question, but doing that to `Symbol` is.  The paper [Towards the
   Essence of Hygiene](https://michaeldadams.org/papers/hygiene/hygiene-2015-popl-authors-copy.pdf)
   describes how simple names are not enough in a hygienic system.

2. Even if the point above wasn't a problem, auto-escaping unescaped caller-provided
   syntax breaks the behaviour of current unescaped caller-provided syntax.  For
   example, the following is expected to resolve `foo` in `M`: and not in
   `@__MODULE__`:

   ```julia
   module M
   macro m(x); x; end
   end

   M.@m foo
   ```

   The `@addone` example above was framed as user error, but also illustrates
   the breakage.
