# Base.Cartesian

The (non-exported) Cartesian module provides macros that facilitate writing multidimensional algorithms.
Most often you can write such algorithms with [straightforward techniques](https://julialang.org/blog/2016/02/iteration); however, there are a few cases where `Base.Cartesian` is still useful or necessary.

## Principles of usage

A simple example of usage is:

```julia
@nloops 3 i A begin
    s += @nref 3 A i
end
```

which generates the following code:

```julia
for i_3 = axes(A, 3)
    for i_2 = axes(A, 2)
        for i_1 = axes(A, 1)
            s += A[i_1, i_2, i_3]
        end
    end
end
```

In general, Cartesian allows you to write generic code that contains repetitive elements, like
the nested loops in this example.  Other applications include repeated expressions (e.g., loop
unwinding) or creating function calls with variable numbers of arguments without using the "splat"
construct (`i...`).

## Basic syntax

The (basic) syntax of `@nloops` is as follows:

  * The first argument must be an integer (*not* a variable) specifying the number of loops.
  * The second argument is the symbol-prefix used for the iterator variable. Here we used `i`, and
    variables `i_1, i_2, i_3` were generated.
  * The third argument specifies the range for each iterator variable. If you use a variable (symbol)
    here, it's taken as `axes(A, dim)`. More flexibly, you can use the anonymous-function expression
    syntax described below.
  * The last argument is the body of the loop. Here, that's what appears between the `begin...end`.

There are some additional features of `@nloops` described in the [reference section](@ref dev-cartesian-reference).

`@nref` follows a similar pattern, generating `A[i_1,i_2,i_3]` from `@nref 3 A i`. The general
practice is to read from left to right, which is why `@nloops` is `@nloops 3 i A expr` (as in
`for i_2 = axes(A, 2)`, where `i_2` is to the left and the range is to the right) whereas `@nref`
is `@nref 3 A i` (as in `A[i_1,i_2,i_3]`, where the array comes first).

If you're developing code with Cartesian, you may find that debugging is easier when you examine
the generated code, using `@macroexpand`:

```@meta
DocTestSetup = quote
    import Base.Cartesian: @nref
end
```

```jldoctest
julia> @macroexpand @nref 2 A i
:(A[i_1, i_2])
```

```@meta
DocTestSetup = nothing
```

### Supplying the number of expressions

The first argument to both of these macros is the number of expressions, which must be an integer.
When you're writing a function that you intend to work in multiple dimensions, this may not be
something you want to hard-code. The recommended approach is to use a `@generated function`.  Here's
an example:

```julia
@generated function mysum(A::Array{T,N}) where {T,N}
    quote
        s = zero(T)
        @nloops $N i A begin
            s += @nref $N A i
        end
        s
    end
end
```

Naturally, you can also prepare expressions or perform calculations before the `quote` block.

### Anonymous-function expressions as macro arguments

Perhaps the single most powerful feature in `Cartesian` is the ability to supply anonymous-function
expressions that get evaluated at parsing time.  Let's consider a simple example:

```julia
@nexprs 2 j->(i_j = 1)
```

`@nexprs` generates `n` expressions that follow a pattern. This code would generate the following
statements:

```julia
i_1 = 1
i_2 = 1
```

In each generated statement, an "isolated" `j` (the variable of the anonymous function) gets replaced
by values in the range `1:2`. Generally speaking, Cartesian employs a LaTeX-like syntax.  This
allows you to do math on the index `j`.  Here's an example computing the strides of an array:

```julia
s_1 = 1
@nexprs 3 j->(s_{j+1} = s_j * size(A, j))
```

would generate expressions

```julia
s_1 = 1
s_2 = s_1 * size(A, 1)
s_3 = s_2 * size(A, 2)
s_4 = s_3 * size(A, 3)
```

Anonymous-function expressions have many uses in practice.

#### [Macro reference](@id dev-cartesian-reference)

```@docs
Base.Cartesian.@nloops
Base.Cartesian.@nref
Base.Cartesian.@nextract
Base.Cartesian.@nexprs
Base.Cartesian.@ncall
Base.Cartesian.@ntuple
Base.Cartesian.@nall
Base.Cartesian.@nany
Base.Cartesian.@nif
```
