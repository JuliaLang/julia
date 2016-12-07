# Talking to the compiler (the `:meta` mechanism)

In some circumstances, one might wish to provide hints or instructions that a given block of code
has special properties: you might always want to inline it, or you might want to turn on special
compiler optimization passes.  Starting with version 0.4, Julia has a convention that these instructions
can be placed inside a `:meta` expression, which is typically (but not necessarily) the first
expression in the body of a function.

`:meta` expressions are created with macros. As an example, consider the implementation of the
`@inline` macro:

```julia
macro inline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :inline) : ex)
end
```

Here, `ex` is expected to be an expression defining a function. A statement like this:

```julia
@inline function myfunction(x)
    x*(x+3)
end
```

gets turned into an expression like this:

```julia
quote
    function myfunction(x)
        Expr(:meta, :inline)
        x*(x+3)
    end
end
```

`Base.pushmeta!(ex, :symbol, args...)` appends `:symbol` to the end of the `:meta` expression,
creating a new `:meta` expression if necessary. If `args` is specified, a nested expression containing
`:symbol` and these arguments is appended instead, which can be used to specify additional information.

To use the metadata, you have to parse these `:meta` expressions. If your implementation can be
performed within Julia, `Base.popmeta!` is very handy: `Base.popmeta!(body, :symbol)` will scan
a function *body* expression (one without the function signature) for the first `:meta` expression
containing `:symbol`, extract any arguments, and return a tuple `(found::Bool, args::Array{Any})`.
If the metadata did not have any arguments, or `:symbol` was not found, the `args` array will
be empty.

Not yet provided is a convenient infrastructure for parsing `:meta` expressions from C++.
