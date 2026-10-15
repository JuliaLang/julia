# BaseSyntax

`BaseSyntax` provides the data structures used by macro-expansion and lowering
(`Syntax`, `SyntaxContext`, `SourceRef`, ...) and their helpers, as defined in
[`src/syntax.jl`](src/syntax.jl).

`Base` includes this package as `Base.BaseSyntax`, and makes its definitions
available as `Base.<name>` as well. Loaded as a package, `BaseSyntax`
re-exports `Base.BaseSyntax` on Julia versions that have it, and defines the
same things itself on older versions. Packages that work with these types, such
as JuliaSyntax and JuliaLowering, import them from `BaseSyntax` so that they
share the same types with `Base` where possible, while still loading on older
Julia versions.
