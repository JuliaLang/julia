# BaseSyntax

`BaseSyntax` provides the data structures used by macro-expansion and lowering
(`Syntax`, `SyntaxContext`, `SourceRef`, ...) and their helpers, as defined in
[`src/syntax.jl`](src/syntax.jl).

`Base` includes this package as `Base.BaseSyntax`, and makes its definitions
available as `Base.<name>` as well. Loaded as a package, `BaseSyntax` behaves
differently depending on the Julia version:

- On Julia versions whose `Base` has `Base.BaseSyntax` (1.14 and later), it
  re-exports `Base.BaseSyntax`, so its types are the same as those of `Base`.
- On Julia 1.12 and 1.13, it defines the same things itself.
- On older versions, it defines nothing, so that packages depending on it, such
  as JuliaSyntax, still load there.

JuliaSyntax and JuliaLowering import these definitions from `BaseSyntax`, so
that they share the types with `Base` where possible while still working on
Julia 1.12 and 1.13. You usually don't need to use `BaseSyntax` directly; see
[the JuliaLowering README](../JuliaLowering/README.md#using-julialowering-as-a-package)
for how to use these packages on released Julia versions.
