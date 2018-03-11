# [Performance Tips](@id man-performance-tips)

In the following sections, we briefly go through a few techniques that can help make your Julia
code run as fast as possible.

## Avoid global variables

A global variable might have its value, and therefore its type, change at any point. This makes
it difficult for the compiler to optimize code using global variables. Variables should be local,
or passed as arguments to functions, whenever possible.

Any code that is performance critical or being benchmarked should be inside a function.

We find that global names are frequently constants, and declaring them as such greatly improves
performance:

```julia
const DEFAULT_VAL = 0
```

Uses of non-constant globals can be optimized by annotating their types at the point of use:

```julia
global x
y = f(x::Int + 1)
```

Writing functions is better style. It leads to more reusable code and clarifies what steps are
being done, and what their inputs and outputs are.

!!! note
    All code in the REPL is evaluated in global scope, so a variable defined and assigned
    at toplevel will be a **global** variable.

In the following REPL session:

```julia-repl
julia> x = 1.0
```

is equivalent to:

```julia-repl
julia> global x = 1.0
```

so all the performance issues discussed previously apply.

## Measure performance with [`@time`](@ref) and pay attention to memory allocation

A useful tool for measuring performance is the [`@time`](@ref) macro. The following example
illustrates good working style:

```julia-repl
julia> function f(n)
           s = 0
           for i = 1:n
               s += i/2
           end
           s
       end
f (generic function with 1 method)

julia> @time f(1)
  0.012686 seconds (2.09 k allocations: 103.421 KiB)
0.5

julia> @time f(10^6)
  0.021061 seconds (3.00 M allocations: 45.777 MiB, 11.69% gc time)
2.5000025e11
```

On the first call (`@time f(1)`), `f` gets compiled.  (If you've not yet used [`@time`](@ref)
in this session, it will also compile functions needed for timing.)  You should not take the results
of this run seriously. For the second run, note that in addition to reporting the time, it also
indicated that a large amount of memory was allocated.

Unexpected memory allocation is almost always a sign of some problem with your code, usually a
problem with type-stability. Consequently, in addition to the allocation itself, it's very likely
that the code generated for your function is far from optimal. Take such indications seriously
and follow the advice below.

For more serious benchmarking, consider the [BenchmarkTools.jl](https://github.com/JuliaCI/BenchmarkTools.jl)
package which evaluates the function multiple times in order to reduce noise.

As a teaser, an improved version of this function allocates no memory
(the allocation reported below is due to running the `@time` macro in global scope)
and has an order of magnitude faster execution after the first call:

```julia-repl
julia> @time f_improved(1)
  0.007008 seconds (1.32 k allocations: 63.640 KiB)
0.5

julia> @time f_improved(10^6)
  0.002997 seconds (6 allocations: 192 bytes)
2.5000025e11
```

Below you'll learn how to spot the problem with `f` and how to fix it.

In some situations, your function may need to allocate memory as part of its operation, and this
can complicate the simple picture above. In such cases, consider using one of the [tools](@ref tools)
below to diagnose problems, or write a version of your function that separates allocation from
its algorithmic aspects (see [Pre-allocating outputs](@ref)).

## [Tools](@id tools)

Julia and its package ecosystem includes tools that may help you diagnose problems and improve
the performance of your code:

  * [Profiling](@ref) allows you to measure the performance of your running code and identify lines
    that serve as bottlenecks.  For complex projects, the [ProfileView](https://github.com/timholy/ProfileView.jl)
    package can help you visualize your profiling results.
  * Unexpectedly-large memory allocations--as reported by [`@time`](@ref), [`@allocated`](@ref), or
    the profiler (through calls to the garbage-collection routines)--hint that there might be issues
    with your code.  If you don't see another reason for the allocations, suspect a type problem.
     You can also start Julia with the `--track-allocation=user` option and examine the resulting
    `*.mem` files to see information about where those allocations occur.  See [Memory allocation analysis](@ref).
  * `@code_warntype` generates a representation of your code that can be helpful in finding expressions
    that result in type uncertainty. See [`@code_warntype`](@ref) below.
  * The [Lint](https://github.com/tonyhffong/Lint.jl)
    package can also warn you of certain types of programming errors.

## Avoid containers with abstract type parameters

When working with parameterized types, including arrays, it is best to avoid parameterizing with
abstract types where possible.

Consider the following:

```julia
a = Real[]    # typeof(a) = Array{Real,1}
if (f = rand()) < .8
    push!(a, f)
end
```

Because `a` is a an array of abstract type [`Real`](@ref), it must be able to hold any
`Real` value.  Since `Real` objects can be of arbitrary size and structure, `a` must be
represented as an array of pointers to individually allocated `Real` objects. Because `f`
will always be a [`Float64`](@ref), we should instead, use:

```julia
a = Float64[] # typeof(a) = Array{Float64,1}
```

which will create a contiguous block of 64-bit floating-point values that can be manipulated efficiently.

See also the discussion under [Parametric Types](@ref).

## Type declarations

In many languages with optional type declarations, adding declarations is the principal way to
make code run faster. This is *not* the case in Julia. In Julia, the compiler generally knows
the types of all function arguments, local variables, and expressions. However, there are a few
specific instances where declarations are helpful.

### Avoid fields with abstract type

Types can be declared without specifying the types of their fields:

```jldoctest myambig
julia> struct MyAmbiguousType
           a
       end
```

This allows `a` to be of any type. This can often be useful, but it does have a downside: for
objects of type `MyAmbiguousType`, the compiler will not be able to generate high-performance
code.  The reason is that the compiler uses the types of objects, not their values, to determine
how to build code. Unfortunately, very little can be inferred about an object of type `MyAmbiguousType`:

```jldoctest myambig
julia> b = MyAmbiguousType("Hello")
MyAmbiguousType("Hello")

julia> c = MyAmbiguousType(17)
MyAmbiguousType(17)

julia> typeof(b)
MyAmbiguousType

julia> typeof(c)
MyAmbiguousType
```

`b` and `c` have the same type, yet their underlying representation of data in memory is very
different. Even if you stored just numeric values in field `a`, the fact that the memory representation
of a [`UInt8`](@ref) differs from a [`Float64`](@ref) also means that the CPU needs to handle
them using two different kinds of instructions. Since the required information is not available
in the type, such decisions have to be made at run-time. This slows performance.

You can do better by declaring the type of `a`. Here, we are focused on the case where `a` might
be any one of several types, in which case the natural solution is to use parameters. For example:

```jldoctest myambig2
julia> mutable struct MyType{T<:AbstractFloat}
           a::T
       end
```

This is a better choice than

```jldoctest myambig2
julia> mutable struct MyStillAmbiguousType
           a::AbstractFloat
       end
```

because the first version specifies the type of `a` from the type of the wrapper object.  For
example:

```jldoctest myambig2
julia> m = MyType(3.2)
MyType{Float64}(3.2)

julia> t = MyStillAmbiguousType(3.2)
MyStillAmbiguousType(3.2)

julia> typeof(m)
MyType{Float64}

julia> typeof(t)
MyStillAmbiguousType
```

The type of field `a` can be readily determined from the type of `m`, but not from the type of
`t`.  Indeed, in `t` it's possible to change the type of field `a`:

```jldoctest myambig2
julia> typeof(t.a)
Float64

julia> t.a = 4.5f0
4.5f0

julia> typeof(t.a)
Float32
```

In contrast, once `m` is constructed, the type of `m.a` cannot change:

```jldoctest myambig2
julia> m.a = 4.5f0
4.5f0

julia> typeof(m.a)
Float64
```

The fact that the type of `m.a` is known from `m`'s type--coupled with the fact that its type
cannot change mid-function--allows the compiler to generate highly-optimized code for objects
like `m` but not for objects like `t`.

Of course, all of this is true only if we construct `m` with a concrete type.  We can break this
by explicitly constructing it with an abstract type:

```jldoctest myambig2
julia> m = MyType{AbstractFloat}(3.2)
MyType{AbstractFloat}(3.2)

julia> typeof(m.a)
Float64

julia> m.a = 4.5f0
4.5f0

julia> typeof(m.a)
Float32
```

For all practical purposes, such objects behave identically to those of `MyStillAmbiguousType`.

It's quite instructive to compare the sheer amount code generated for a simple function

```julia
func(m::MyType) = m.a+1
```

using

```julia
code_llvm(func,Tuple{MyType{Float64}})
code_llvm(func,Tuple{MyType{AbstractFloat}})
code_llvm(func,Tuple{MyType})
```

For reasons of length the results are not shown here, but you may wish to try this yourself. Because
the type is fully-specified in the first case, the compiler doesn't need to generate any code
to resolve the type at run-time. This results in shorter and faster code.

### Avoid fields with abstract containers

The same best practices also work for container types:

```jldoctest containers
julia> mutable struct MySimpleContainer{A<:AbstractVector}
           a::A
       end

julia> mutable struct MyAmbiguousContainer{T}
           a::AbstractVector{T}
       end
```

For example:

```jldoctest containers
julia> c = MySimpleContainer(1:3);

julia> typeof(c)
MySimpleContainer{UnitRange{Int64}}

julia> c = MySimpleContainer([1:3;]);

julia> typeof(c)
MySimpleContainer{Array{Int64,1}}

julia> b = MyAmbiguousContainer(1:3);

julia> typeof(b)
MyAmbiguousContainer{Int64}

julia> b = MyAmbiguousContainer([1:3;]);

julia> typeof(b)
MyAmbiguousContainer{Int64}
```

For `MySimpleContainer`, the object is fully-specified by its type and parameters, so the compiler
can generate optimized functions. In most instances, this will probably suffice.

While the compiler can now do its job perfectly well, there are cases where *you* might wish that
your code could do different things depending on the *element type* of `a`.  Usually the best
way to achieve this is to wrap your specific operation (here, `foo`) in a separate function:

```julia jldoctest containers
julia> function sumfoo(c::MySimpleContainer)
           s = 0
           for x in c.a
               s += foo(x)
           end
           s
       end
sumfoo (generic function with 1 method)

julia> foo(x::Integer) = x
foo (generic function with 1 method)

julia> foo(x::AbstractFloat) = round(x)
foo (generic function with 2 methods)
```

This keeps things simple, while allowing the compiler to generate optimized code in all cases.

However, there are cases where you may need to declare different versions of the outer function
for different element types of `a`. You could do it like this:

```
function myfun(c::MySimpleContainer{Vector{T}}) where T<:AbstractFloat
    ...
end
function myfun(c::MySimpleContainer{Vector{T}}) where T<:Integer
    ...
end
```

This works fine for `Vector{T}`, but we'd also have to write explicit versions for `UnitRange{T}`
or other abstract types. To prevent such tedium, you can use two parameters in the declaration
of `MyContainer`:

```jldoctest containers2
julia> mutable struct MyContainer{T, A<:AbstractVector}
           a::A
       end

julia> MyContainer(v::AbstractVector) = MyContainer{eltype(v), typeof(v)}(v)
MyContainer

julia> b = MyContainer(1:5);

julia> typeof(b)
MyContainer{Int64,UnitRange{Int64}}
```

Note the somewhat surprising fact that `T` doesn't appear in the declaration of field `a`, a point
that we'll return to in a moment. With this approach, one can write functions such as:

```jldoctest containers2
julia> function myfunc(c::MyContainer{<:Integer, <:AbstractArray})
           return c.a[1]+1
       end
myfunc (generic function with 1 method)

julia> function myfunc(c::MyContainer{<:AbstractFloat})
           return c.a[1]+2
       end
myfunc (generic function with 2 methods)

julia> function myfunc(c::MyContainer{T,Vector{T}}) where T<:Integer
           return c.a[1]+3
       end
myfunc (generic function with 3 methods)
```

!!! note
    Because we can only define `MyContainer` for
    `A<:AbstractArray`, and any unspecified parameters are arbitrary,
    the first function above could have been written more succinctly as
    `function myfunc(c::MyContainer{<:Integer})`


```jldoctest containers2
julia> myfunc(MyContainer(1:3))
2

julia> myfunc(MyContainer(1.0:3))
3.0

julia> myfunc(MyContainer([1:3;]))
4
```

As you can see, with this approach it's possible to specialize on both the element type `T` and
the array type `A`.

However, there's one remaining hole: we haven't enforced that `A` has element type `T`, so it's
perfectly possible to construct an object like this:

```jldoctest containers2
julia> b = MyContainer{Int64, UnitRange{Float64}}(UnitRange(1.3, 5.0));

julia> typeof(b)
MyContainer{Int64,UnitRange{Float64}}
```

To prevent this, we can add an inner constructor:

```jldoctest containers3
julia> mutable struct MyBetterContainer{T<:Real, A<:AbstractVector}
           a::A
           MyBetterContainer{T,A}(v::AbstractVector{T}) where {T,A} = new(v)
       end

julia> MyBetterContainer(v::AbstractVector) = MyBetterContainer{eltype(v),typeof(v)}(v)
MyBetterContainer

julia> b = MyBetterContainer(UnitRange(1.3, 5.0));

julia> typeof(b)
MyBetterContainer{Float64,UnitRange{Float64}}

julia> b = MyBetterContainer{Int64, UnitRange{Float64}}(UnitRange(1.3, 5.0));
ERROR: MethodError: Cannot `convert` an object of type UnitRange{Float64} to an object of type MyBetterContainer{Int64,UnitRange{Float64}}
[...]
```

The inner constructor requires that the element type of `A` be `T`.

### Annotate values taken from untyped locations

It is often convenient to work with data structures that may contain values of any type (arrays
of type `Array{Any}`). But, if you're using one of these structures and happen to know the type
of an element, it helps to share this knowledge with the compiler:

```julia
function foo(a::Array{Any,1})
    x = a[1]::Int32
    b = x+1
    ...
end
```

Here, we happened to know that the first element of `a` would be an [`Int32`](@ref). Making
an annotation like this has the added benefit that it will raise a run-time error if the
value is not of the expected type, potentially catching certain bugs earlier.

In the case that the type of `a[1]` is not known precisely, `x` can be declared via
`x = convert(Int32,a[1])::Int32`. The use of the [`convert`](@ref) function allows `a[1]`
to be any object convertible to an `Int32` (such as `UInt8`), thus increasing the genericity
of the code by loosening the type requirement. Notice that `convert` itself needs a type
annotation in this context in order to achieve type stability. This is because the compiler
cannot deduce the type of the return value of a function, even `convert`, unless the types of
all the function's arguments are known.

Type annotation will not enhance (and can actually hinder) performance if the type is constructed
at run-time. This is because the compiler cannot use the annotation to specialize the subsequent
code, and the type-check itself takes time. For example, in the code:

```julia
function nr(a, prec)
    ctype = prec == 32 ? Float32 : Float64
    b = Complex{ctype}(a)
    c = (b + 1.0f0)::Complex{ctype}
    abs(c)
end
```

the annotation of `c` harms performance. To write performant code involving types constructed at
run-time, use the [function-barrier technique](@ref kernal-functions) discussed below, and ensure
that the constructed type appears among the argument types of the kernel function so that the kernel
operations are properly specialized by the compiler. For example, in the above snippet, as soon as
`b` is constructed, it can be passed to another function `k`, the kernel. If, for example, function
`k` declares `b` as an argument of type `Complex{T}`, where `T` is a type parameter, then a type annotation
appearing in an assignment statement within `k` of the form:

```julia
c = (b + 1.0f0)::Complex{T}
```

does not hinder performance (but does not help either) since the compiler can determine the type of `c`
at the time `k` is compiled.

### Declare types of keyword arguments

Keyword arguments can have declared types:

```julia
function with_keyword(x; name::Int = 1)
    ...
end
```

Functions are specialized on the types of keyword arguments, so these declarations will not affect
performance of code inside the function. However, they will reduce the overhead of calls to the
function that include keyword arguments.

Functions with keyword arguments have near-zero overhead for call sites that pass only positional
arguments.

Passing dynamic lists of keyword arguments, as in `f(x; keywords...)`, can be slow and should
be avoided in performance-sensitive code.

## Break functions into multiple definitions

Writing a function as many small definitions allows the compiler to directly call the most applicable
code, or even inline it.

Here is an example of a "compound function" that should really be written as multiple definitions:

```julia
function norm(A)
    if isa(A, Vector)
        return sqrt(real(dot(A,A)))
    elseif isa(A, Matrix)
        return maximum(svd(A)[2])
    else
        error("norm: invalid argument")
    end
end
```

This can be written more concisely and efficiently as:

```julia
norm(x::Vector) = sqrt(real(dot(x,x)))
norm(A::Matrix) = maximum(svd(A)[2])
```

## Write "type-stable" functions

When possible, it helps to ensure that a function always returns a value of the same type. Consider
the following definition:

```julia
pos(x) = x < 0 ? 0 : x
```

Although this seems innocent enough, the problem is that `0` is an integer (of type `Int`) and
`x` might be of any type. Thus, depending on the value of `x`, this function might return a value
of either of two types. This behavior is allowed, and may be desirable in some cases. But it can
easily be fixed as follows:

```julia
pos(x) = x < 0 ? zero(x) : x
```

There is also a [`one`](@ref) function, and a more general [`oftype(x, y)`](@ref) function, which
returns `y` converted to the type of `x`.

## Avoid changing the type of a variable

An analogous "type-stability" problem exists for variables used repeatedly within a function:

```julia
function foo()
    x = 1
    for i = 1:10
        x = x/bar()
    end
    return x
end
```

Local variable `x` starts as an integer, and after one loop iteration becomes a floating-point
number (the result of [`/`](@ref) operator). This makes it more difficult for the compiler to
optimize the body of the loop. There are several possible fixes:

  * Initialize `x` with `x = 1.0`
  * Declare the type of `x`: `x::Float64 = 1`
  * Use an explicit conversion: `x = oneunit(T)`
  * Initialize with the first loop iteration, to `x = 1/bar()`, then loop `for i = 2:10`

## [Separate kernel functions (aka, function barriers)](@id kernal-functions)

Many functions follow a pattern of performing some set-up work, and then running many iterations
to perform a core computation. Where possible, it is a good idea to put these core computations
in separate functions. For example, the following contrived function returns an array of a randomly-chosen
type:

```@meta
DocTestSetup = quote
    import Random
    Random.srand(1234)
end
```

```jldoctest
julia> function strange_twos(n)
           a = Vector{rand(Bool) ? Int64 : Float64}(undef, n)
           for i = 1:n
               a[i] = 2
           end
           return a
       end
strange_twos (generic function with 1 method)

julia> strange_twos(3)
3-element Array{Float64,1}:
 2.0
 2.0
 2.0
```

This should be written as:

```jldoctest
julia> function fill_twos!(a)
           for i = eachindex(a)
               a[i] = 2
           end
       end
fill_twos! (generic function with 1 method)

julia> function strange_twos(n)
           a = Vector{rand(Bool) ? Int64 : Float64}(undef, n)
           fill_twos!(a)
           return a
       end
strange_twos (generic function with 1 method)

julia> strange_twos(3)
3-element Array{Float64,1}:
 2.0
 2.0
 2.0
```

Julia's compiler specializes code for argument types at function boundaries, so in the original
implementation it does not know the type of `a` during the loop (since it is chosen randomly).
Therefore the second version is generally faster since the inner loop can be recompiled as part
of `fill_twos!` for different types of `a`.

The second form is also often better style and can lead to more code reuse.

This pattern is used in several places in Julia Base. For example, see `hvcat_fill`
in [`abstractarray.jl`](https://github.com/JuliaLang/julia/blob/master/base/abstractarray.jl), or
the [`fill!`](@ref) function, which we could have used instead of writing our own `fill_twos!`.

Functions like `strange_twos` occur when dealing with data of uncertain type, for example data
loaded from an input file that might contain either integers, floats, strings, or something else.

## Types with values-as-parameters

Let's say you want to create an `N`-dimensional array that has size 3 along each axis. Such arrays
can be created like this:

```jldoctest
julia> A = fill(5.0, (3, 3))
3×3 Array{Float64,2}:
 5.0  5.0  5.0
 5.0  5.0  5.0
 5.0  5.0  5.0
```

This approach works very well: the compiler can figure out that `A` is an `Array{Float64,2}` because
it knows the type of the fill value (`5.0::Float64`) and the dimensionality (`(3, 3)::NTuple{2,Int}`).
This implies that the compiler can generate very efficient code for any future usage of `A` in
the same function.

But now let's say you want to write a function that creates a 3×3×... array in arbitrary dimensions;
you might be tempted to write a function

```jldoctest
julia> function array3(fillval, N)
           fill(fillval, ntuple(d->3, N))
       end
array3 (generic function with 1 method)

julia> array3(5.0, 2)
3×3 Array{Float64,2}:
 5.0  5.0  5.0
 5.0  5.0  5.0
 5.0  5.0  5.0
```

This works, but (as you can verify for yourself using `@code_warntype array3(5.0, 2)`) the problem
is that the output type cannot be inferred: the argument `N` is a *value* of type `Int`, and type-inference
does not (and cannot) predict its value in advance. This means that code using the output of this
function has to be conservative, checking the type on each access of `A`; such code will be very
slow.

Now, one very good way to solve such problems is by using the [function-barrier technique](@ref kernal-functions).
However, in some cases you might want to eliminate the type-instability altogether.  In such cases,
one approach is to pass the dimensionality as a parameter, for example through `Val{T}()` (see
["Value types"](@ref)):

```jldoctest
julia> function array3(fillval, ::Val{N}) where N
           fill(fillval, ntuple(d->3, Val(N)))
       end
array3 (generic function with 1 method)

julia> array3(5.0, Val(2))
3×3 Array{Float64,2}:
 5.0  5.0  5.0
 5.0  5.0  5.0
 5.0  5.0  5.0
```

Julia has a specialized version of `ntuple` that accepts a `Val{::Int}` instance as the second
parameter; by passing `N` as a type-parameter, you make its "value" known to the compiler.
Consequently, this version of `array3` allows the compiler to predict the return type.

However, making use of such techniques can be surprisingly subtle. For example, it would be of
no help if you called `array3` from a function like this:

```julia
function call_array3(fillval, n)
    A = array3(fillval, Val(n))
end
```

Here, you've created the same problem all over again: the compiler can't guess what `n` is,
so it doesn't know the *type* of `Val(n)`.  Attempting to use `Val`, but doing so incorrectly, can
easily make performance *worse* in many situations.  (Only in situations where you're effectively
combining `Val` with the function-barrier trick, to make the kernel function more efficient, should
code like the above be used.)

An example of correct usage of `Val` would be:

```julia
function filter3(A::AbstractArray{T,N}) where {T,N}
    kernel = array3(1, Val(N))
    filter(A, kernel)
end
```

In this example, `N` is passed as a parameter, so its "value" is known to the compiler.  Essentially,
`Val(T)` works only when `T` is either hard-coded/literal (`Val(3)`) or already specified in the
type-domain.

## The dangers of abusing multiple dispatch (aka, more on types with values-as-parameters)

Once one learns to appreciate multiple dispatch, there's an understandable tendency to go crazy
and try to use it for everything. For example, you might imagine using it to store information,
e.g.

```
struct Car{Make,Model}
    year::Int
    ...more fields...
end
```

and then dispatch on objects like `Car{:Honda,:Accord}(year, args...)`.

This might be worthwhile when the following are true:

  * You require CPU-intensive processing on each `Car`, and it becomes vastly more efficient if you
    know the `Make` and `Model` at compile time.
  * You have homogenous lists of the same type of `Car` to process, so that you can store them all
    in an `Array{Car{:Honda,:Accord},N}`.

When the latter holds, a function processing such a homogenous array can be productively specialized:
Julia knows the type of each element in advance (all objects in the container have the same concrete
type), so Julia can "look up" the correct method calls when the function is being compiled (obviating
the need to check at run-time) and thereby emit efficient code for processing the whole list.

When these do not hold, then it's likely that you'll get no benefit; worse, the resulting "combinatorial
explosion of types" will be counterproductive.  If `items[i+1]` has a different type than `item[i]`,
Julia has to look up the type at run-time, search for the appropriate method in method tables,
decide (via type intersection) which one matches, determine whether it has been JIT-compiled yet
(and do so if not), and then make the call. In essence, you're asking the full type- system and
JIT-compilation machinery to basically execute the equivalent of a switch statement or dictionary
lookup in your own code.

Some run-time benchmarks comparing (1) type dispatch, (2) dictionary lookup, and (3) a "switch"
statement can be found [on the mailing list](https://groups.google.com/forum/#!msg/julia-users/jUMu9A3QKQQ/qjgVWr7vAwAJ).

Perhaps even worse than the run-time impact is the compile-time impact: Julia will compile specialized
functions for each different `Car{Make, Model}`; if you have hundreds or thousands of such types,
then every function that accepts such an object as a parameter (from a custom `get_year` function
you might write yourself, to the generic `push!` function in Julia Base) will have hundreds
or thousands of variants compiled for it.  Each of these increases the size of the cache of compiled
code, the length of internal lists of methods, etc.  Excess enthusiasm for values-as-parameters
can easily waste enormous resources.

## Access arrays in memory order, along columns

Multidimensional arrays in Julia are stored in column-major order. This means that arrays are
stacked one column at a time. This can be verified using the `vec` function or the syntax `[:]`
as shown below (notice that the array is ordered `[1 3 2 4]`, not `[1 2 3 4]`):

```jldoctest
julia> x = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> x[:]
4-element Array{Int64,1}:
 1
 3
 2
 4
```

This convention for ordering arrays is common in many languages like Fortran, Matlab, and R (to
name a few). The alternative to column-major ordering is row-major ordering, which is the convention
adopted by C and Python (`numpy`) among other languages. Remembering the ordering of arrays can
have significant performance effects when looping over arrays. A rule of thumb to keep in mind
is that with column-major arrays, the first index changes most rapidly. Essentially this means
that looping will be faster if the inner-most loop index is the first to appear in a slice expression.

Consider the following contrived example. Imagine we wanted to write a function that accepts a
[`Vector`](@ref) and returns a square [`Matrix`](@ref) with either the rows or the columns filled with copies
of the input vector. Assume that it is not important whether rows or columns are filled with these
copies (perhaps the rest of the code can be easily adapted accordingly). We could conceivably
do this in at least four ways (in addition to the recommended call to the built-in [`repeat`](@ref)):

```julia
function copy_cols(x::Vector{T}) where T
    inds = axes(x, 1)
    out = similar(Array{T}, inds, inds)
    for i = inds
        out[:, i] = x
    end
    out
end

function copy_rows(x::Vector{T}) where T
    inds = axes(x, 1)
    out = similar(Array{T}, inds, inds)
    for i = inds
        out[i, :] = x
    end
    out
end

function copy_col_row(x::Vector{T}) where T
    inds = axes(x, 1)
    out = similar(Array{T}, inds, inds)
    for col = inds, row = inds
        out[row, col] = x[row]
    end
    out
end

function copy_row_col(x::Vector{T}) where T
    inds = axes(x, 1)
    out = similar(Array{T}, inds, inds)
    for row = inds, col = inds
        out[row, col] = x[col]
    end
    out
end
```

Now we will time each of these functions using the same random `10000` by `1` input vector:

```julia-repl
julia> x = randn(10000);

julia> fmt(f) = println(rpad(string(f)*": ", 14, ' '), @elapsed f(x))

julia> map(fmt, Any[copy_cols, copy_rows, copy_col_row, copy_row_col]);
copy_cols:    0.331706323
copy_rows:    1.799009911
copy_col_row: 0.415630047
copy_row_col: 1.721531501
```

Notice that `copy_cols` is much faster than `copy_rows`. This is expected because `copy_cols`
respects the column-based memory layout of the `Matrix` and fills it one column at a time. Additionally,
`copy_col_row` is much faster than `copy_row_col` because it follows our rule of thumb that the
first element to appear in a slice expression should be coupled with the inner-most loop.

## Pre-allocating outputs

If your function returns an `Array` or some other complex type, it may have to allocate memory.
 Unfortunately, oftentimes allocation and its converse, garbage collection, are substantial bottlenecks.

Sometimes you can circumvent the need to allocate memory on each function call by preallocating
the output.  As a trivial example, compare

```julia
function xinc(x)
    return [x, x+1, x+2]
end

function loopinc()
    y = 0
    for i = 1:10^7
        ret = xinc(i)
        y += ret[2]
    end
    y
end
```

with

```julia
function xinc!(ret::AbstractVector{T}, x::T) where T
    ret[1] = x
    ret[2] = x+1
    ret[3] = x+2
    nothing
end

function loopinc_prealloc()
    ret = Vector{Int}(undef, 3)
    y = 0
    for i = 1:10^7
        xinc!(ret, i)
        y += ret[2]
    end
    y
end
```

Timing results:

```julia-repl
julia> @time loopinc()
  0.529894 seconds (40.00 M allocations: 1.490 GiB, 12.14% gc time)
50000015000000

julia> @time loopinc_prealloc()
  0.030850 seconds (6 allocations: 288 bytes)
50000015000000
```

Preallocation has other advantages, for example by allowing the caller to control the "output"
type from an algorithm.  In the example above, we could have passed a `SubArray` rather than an
[`Array`](@ref), had we so desired.

Taken to its extreme, pre-allocation can make your code uglier, so performance measurements and
some judgment may be required. However, for "vectorized" (element-wise) functions, the convenient
syntax `x .= f.(y)` can be used for in-place operations with fused loops and no temporary arrays
(see the [dot syntax for vectorizing functions](@ref man-vectorized)).

## More dots: Fuse vectorized operations

Julia has a special [dot syntax](@ref man-vectorized) that converts
any scalar function into a "vectorized" function call, and any operator
into a "vectorized" operator, with the special property that nested
"dot calls" are *fusing*: they are combined at the syntax level into
a single loop, without allocating temporary arrays. If you use `.=` and
similar assignment operators, the result can also be stored in-place
in a pre-allocated array (see above).

In a linear-algebra context, this means that even though operations like
`vector + vector` and `vector * scalar` are defined, it can be advantageous
to instead use `vector .+ vector` and `vector .* scalar` because the
resulting loops can be fused with surrounding computations. For example,
consider the two functions:

```julia
f(x) = 3x.^2 + 4x + 7x.^3

fdot(x) = @. 3x^2 + 4x + 7x^3 # equivalent to 3 .* x.^2 .+ 4 .* x .+ 7 .* x.^3
```

Both `f` and `fdot` compute the same thing.  However, `fdot`
(defined with the help of the [`@.`](@ref @__dot__) macro) is
significantly faster when applied to an array:

```julia-repl
julia> x = rand(10^6);

julia> @time f(x);
  0.010986 seconds (18 allocations: 53.406 MiB, 11.45% gc time)

julia> @time fdot(x);
  0.003470 seconds (6 allocations: 7.630 MiB)

julia> @time f.(x);
  0.003297 seconds (30 allocations: 7.631 MiB)
```

That is, `fdot(x)` is three times faster and allocates 1/7 the
memory of `f(x)`, because each `*` and `+` operation in `f(x)` allocates
a new temporary array and executes in a separate loop. (Of course,
if you just do `f.(x)` then it is as fast as `fdot(x)` in this
example, but in many contexts it is more convenient to just sprinkle
some dots in your expressions rather than defining a separate function
for each vectorized operation.)

## Consider using views for slices

In Julia, an array "slice" expression like `array[1:5, :]` creates
a copy of that data (except on the left-hand side of an assignment,
where `array[1:5, :] = ...` assigns in-place to that portion of `array`).
If you are doing many operations on the slice, this can be good for
performance because it is more efficient to work with a smaller
contiguous copy than it would be to index into the original array.
On the other hand, if you are just doing a few simple operations on
the slice, the cost of the allocation and copy operations can be
substantial.

An alternative is to create a "view" of the array, which is
an array object (a `SubArray`) that actually references the data
of the original array in-place, without making a copy.  (If you
write to a view, it modifies the original array's data as well.)
This can be done for individual slices by calling [`view`](@ref),
or more simply for a whole expression or block of code by putting
[`@views`](@ref) in front of that expression.  For example:

```julia-repl
julia> fcopy(x) = sum(x[2:end-1])

julia> @views fview(x) = sum(x[2:end-1])

julia> x = rand(10^6);

julia> @time fcopy(x);
  0.003051 seconds (7 allocations: 7.630 MB)

julia> @time fview(x);
  0.001020 seconds (6 allocations: 224 bytes)
```

Notice both the 3× speedup and the decreased memory allocation
of the `fview` version of the function.

## Copying data is not always bad

Arrays are stored contiguously in memory, lending themselves to CPU vectorization
and fewer memory accesses due to caching. These are the same reasons that it is recommended
to access arrays in column-major order (see above). Irregular access patterns and non-contiguous views
can drastically slow down computations on arrays because of non-sequential memory access.

Copying irregularly-accessed data into a contiguous array before operating on it can result
in a large speedup, such as in the example below. Here, a matrix and a vector are being accessed at
800,000 of their randomly-shuffled indices before being multiplied. Copying the views into
plain arrays speeds the multiplication by more than a factor of 2 even with the cost of the copying operation.

```julia-repl
julia> x = randn(1_000_000);

julia> inds = shuffle(1:1_000_000)[1:800000];

julia> A = randn(50, 1_000_000);

julia> xtmp = zeros(800_000);

julia> Atmp = zeros(50, 800_000);

julia> @time sum(view(A, :, inds) * view(x, inds))
  0.640320 seconds (41 allocations: 1.391 KiB)
7253.242699002263

julia> @time begin
           copyto!(xtmp, view(x, inds))
           copyto!(Atmp, view(A, :, inds))
           sum(Atmp * xtmp)
       end
  0.261294 seconds (41 allocations: 1.391 KiB)
7253.242699002323
```
Provided there is enough memory for the copies, the cost of copying the view to an array is
far outweighed by the speed boost from doing the matrix multiplication on a contiguous array.

## Avoid string interpolation for I/O

When writing data to a file (or other I/O device), forming extra intermediate strings is a source
of overhead. Instead of:

```julia
println(file, "$a $b")
```

use:

```julia
println(file, a, " ", b)
```

The first version of the code forms a string, then writes it to the file, while the second version
writes values directly to the file. Also notice that in some cases string interpolation can be
harder to read. Consider:

```julia
println(file, "$(f(a))$(f(b))")
```

versus:

```julia
println(file, f(a), f(b))
```

## Optimize network I/O during parallel execution

When executing a remote function in parallel:

```julia
responses = Vector{Any}(nworkers())
@sync begin
    for (idx, pid) in enumerate(workers())
        @async responses[idx] = remotecall_fetch(pid, foo, args...)
    end
end
```

is faster than:

```julia
refs = Vector{Any}(nworkers())
for (idx, pid) in enumerate(workers())
    refs[idx] = @spawnat pid foo(args...)
end
responses = [fetch(r) for r in refs]
```

The former results in a single network round-trip to every worker, while the latter results in
two network calls - first by the [`@spawnat`](@ref) and the second due to the [`fetch`](@ref)
(or even a [`wait`](@ref)).
The [`fetch`](@ref)/[`wait`](@ref) is also being executed serially resulting in an overall poorer performance.

## Fix deprecation warnings

A deprecated function internally performs a lookup in order to print a relevant warning only once.
This extra lookup can cause a significant slowdown, so all uses of deprecated functions should
be modified as suggested by the warnings.

## Tweaks

These are some minor points that might help in tight inner loops.

  * Avoid unnecessary arrays. For example, instead of [`sum([x,y,z])`](@ref) use `x+y+z`.
  * Use [`abs2(z)`](@ref) instead of [`abs(z)^2`](@ref) for complex `z`. In general, try to rewrite
    code to use [`abs2`](@ref) instead of [`abs`](@ref) for complex arguments.
  * Use [`div(x,y)`](@ref) for truncating division of integers instead of [`trunc(x/y)`](@ref), [`fld(x,y)`](@ref)
    instead of [`floor(x/y)`](@ref), and [`cld(x,y)`](@ref) instead of [`ceil(x/y)`](@ref).

## [Performance Annotations](@id man-performance-annotations)

Sometimes you can enable better optimization by promising certain program properties.

  * Use `@inbounds` to eliminate array bounds checking within expressions. Be certain before doing
    this. If the subscripts are ever out of bounds, you may suffer crashes or silent corruption.
  * Use `@fastmath` to allow floating point optimizations that are correct for real numbers, but lead
    to differences for IEEE numbers. Be careful when doing this, as this may change numerical results.
    This corresponds to the `-ffast-math` option of clang.
  * Write `@simd` in front of `for` loops that are amenable to vectorization. **This feature is experimental**
    and could change or disappear in future versions of Julia.

The common idiom of using 1:n to index into an AbstractArray is not safe if the Array uses unconventional indexing,
and may cause a segmentation fault if bounds checking is turned off. Use `linearindices(x)` or `eachindex(x)`
instead (see also [offset-arrays](https://docs.julialang.org/en/latest/devdocs/offset-arrays)).

Note: While `@simd` needs to be placed directly in front of a loop, both `@inbounds` and `@fastmath`
can be applied to several statements at once, e.g. using `begin` ... `end`, or even to a whole
function.

Here is an example with both `@inbounds` and `@simd` markup:

```julia
function inner(x, y)
    s = zero(eltype(x))
    for i=eachindex(x)
        @inbounds s += x[i]*y[i]
    end
    s
end

function innersimd(x, y)
    s = zero(eltype(x))
    @simd for i=eachindex(x)
        @inbounds s += x[i]*y[i]
    end
    s
end

function timeit(n, reps)
    x = rand(Float32,n)
    y = rand(Float32,n)
    s = zero(Float64)
    time = @elapsed for j in 1:reps
        s+=inner(x,y)
    end
    println("GFlop/sec        = ",2.0*n*reps/time*1E-9)
    time = @elapsed for j in 1:reps
        s+=innersimd(x,y)
    end
    println("GFlop/sec (SIMD) = ",2.0*n*reps/time*1E-9)
end

timeit(1000,1000)
```

On a computer with a 2.4GHz Intel Core i5 processor, this produces:

```
GFlop/sec        = 1.9467069505224963
GFlop/sec (SIMD) = 17.578554163920018
```

(`GFlop/sec` measures the performance, and larger numbers are better.) The range for a `@simd for`
loop should be a one-dimensional range. A variable used for accumulating, such as `s` in the example,
is called a *reduction variable*. By using `@simd`, you are asserting several properties of the
loop:

  * It is safe to execute iterations in arbitrary or overlapping order, with special consideration
    for reduction variables.
  * Floating-point operations on reduction variables can be reordered, possibly causing different
    results than without `@simd`.
  * No iteration ever waits on another iteration to make forward progress.

A loop containing `break`, `continue`, or `@goto` will cause a compile-time error.

Using `@simd` merely gives the compiler license to vectorize. Whether it actually does so depends
on the compiler. To actually benefit from the current implementation, your loop should have the
following additional properties:

  * The loop must be an innermost loop.
  * The loop body must be straight-line code. This is why `@inbounds` is currently needed for all
    array accesses. The compiler can sometimes turn short `&&`, `||`, and `?:` expressions into straight-line
    code, if it is safe to evaluate all operands unconditionally. Consider using the [`ifelse`](@ref)
    function instead of `?:` in the loop if it is safe to do so.
  * Accesses must have a stride pattern and cannot be "gathers" (random-index reads) or "scatters"
    (random-index writes).
  * The stride should be unit stride.
  * In some simple cases, for example with 2-3 arrays accessed in a loop, the LLVM auto-vectorization
    may kick in automatically, leading to no further speedup with `@simd`.

Here is an example with all three kinds of markup. This program first calculates the finite difference
of a one-dimensional array, and then evaluates the L2-norm of the result:

```julia
function init!(u::Vector)
    n = length(u)
    dx = 1.0 / (n-1)
    @fastmath @inbounds @simd for i in 1:n #by asserting that `u` is a `Vector` we can assume it has 1-based indexing
        u[i] = sin(2pi*dx*i)
    end
end

function deriv!(u::Vector, du)
    n = length(u)
    dx = 1.0 / (n-1)
    @fastmath @inbounds du[1] = (u[2] - u[1]) / dx
    @fastmath @inbounds @simd for i in 2:n-1
        du[i] = (u[i+1] - u[i-1]) / (2*dx)
    end
    @fastmath @inbounds du[n] = (u[n] - u[n-1]) / dx
end

function norm(u::Vector)
    n = length(u)
    T = eltype(u)
    s = zero(T)
    @fastmath @inbounds @simd for i in 1:n
        s += u[i]^2
    end
    @fastmath @inbounds return sqrt(s/n)
end

function main()
    n = 2000
    u = Vector{Float64}(undef, n)
    init!(u)
    du = similar(u)

    deriv!(u, du)
    nu = norm(du)

    @time for i in 1:10^6
        deriv!(u, du)
        nu = norm(du)
    end

    println(nu)
end

main()
```

On a computer with a 2.7 GHz Intel Core i7 processor, this produces:

```
$ julia wave.jl;
elapsed time: 1.207814709 seconds (0 bytes allocated)

$ julia --math-mode=ieee wave.jl;
elapsed time: 4.487083643 seconds (0 bytes allocated)
```

Here, the option `--math-mode=ieee` disables the `@fastmath` macro, so that we can compare results.

In this case, the speedup due to `@fastmath` is a factor of about 3.7. This is unusually large
– in general, the speedup will be smaller. (In this particular example, the working set of the
benchmark is small enough to fit into the L1 cache of the processor, so that memory access latency
does not play a role, and computing time is dominated by CPU usage. In many real world programs
this is not the case.) Also, in this case this optimization does not change the result – in
general, the result will be slightly different. In some cases, especially for numerically unstable
algorithms, the result can be very different.

The annotation `@fastmath` re-arranges floating point expressions, e.g. changing the order of
evaluation, or assuming that certain special cases (inf, nan) cannot occur. In this case (and
on this particular computer), the main difference is that the expression `1 / (2*dx)` in the function
`deriv` is hoisted out of the loop (i.e. calculated outside the loop), as if one had written
`idx = 1 / (2*dx)`. In the loop, the expression `... / (2*dx)` then becomes `... * idx`, which
is much faster to evaluate. Of course, both the actual optimization that is applied by the compiler
as well as the resulting speedup depend very much on the hardware. You can examine the change
in generated code by using Julia's [`code_native`](@ref) function.

## Treat Subnormal Numbers as Zeros

Subnormal numbers, formerly called [denormal numbers](https://en.wikipedia.org/wiki/Denormal_number),
are useful in many contexts, but incur a performance penalty on some hardware. A call [`set_zero_subnormals(true)`](@ref)
grants permission for floating-point operations to treat subnormal inputs or outputs as zeros,
which may improve performance on some hardware. A call [`set_zero_subnormals(false)`](@ref) enforces
strict IEEE behavior for subnormal numbers.

Below is an example where subnormals noticeably impact performance on some hardware:

```julia
function timestep(b::Vector{T}, a::Vector{T}, Δt::T) where T
    @assert length(a)==length(b)
    n = length(b)
    b[1] = 1                            # Boundary condition
    for i=2:n-1
        b[i] = a[i] + (a[i-1] - T(2)*a[i] + a[i+1]) * Δt
    end
    b[n] = 0                            # Boundary condition
end

function heatflow(a::Vector{T}, nstep::Integer) where T
    b = similar(a)
    for t=1:div(nstep,2)                # Assume nstep is even
        timestep(b,a,T(0.1))
        timestep(a,b,T(0.1))
    end
end

heatflow(zeros(Float32,10),2)           # Force compilation
for trial=1:6
    a = zeros(Float32,1000)
    set_zero_subnormals(iseven(trial))  # Odd trials use strict IEEE arithmetic
    @time heatflow(a,1000)
end
```

This example generates many subnormal numbers because the values in `a` become an exponentially
decreasing curve, which slowly flattens out over time.

Treating subnormals as zeros should be used with caution, because doing so breaks some identities,
such as `x-y == 0` implies `x == y`:

```jldoctest
julia> x = 3f-38; y = 2f-38;

julia> set_zero_subnormals(true); (x - y, x == y)
(0.0f0, false)

julia> set_zero_subnormals(false); (x - y, x == y)
(1.0000001f-38, false)
```

In some applications, an alternative to zeroing subnormal numbers is to inject a tiny bit of noise.
 For example, instead of initializing `a` with zeros, initialize it with:

```julia
a = rand(Float32,1000) * 1.f-9
```

## [[`@code_warntype`](@ref)](@id man-code-warntype)

The macro [`@code_warntype`](@ref) (or its function variant [`code_warntype`](@ref)) can sometimes
be helpful in diagnosing type-related problems. Here's an example:

```julia
pos(x) = x < 0 ? 0 : x

function f(x)
    y = pos(x)
    sin(y*x+1)
end

julia> @code_warntype f(3.2)
Variables:
  #self#::#f
  x::Float64
  y::UNION{FLOAT64,INT64}
  fy::Float64
  #temp#@_5::UNION{FLOAT64,INT64}
  #temp#@_6::Core.MethodInstance
  #temp#@_7::Float64

Body:
  begin
      $(Expr(:inbounds, false))
      # meta: location REPL[1] pos 1
      # meta: location float.jl < 487
      fy::Float64 = (Core.typeassert)((Base.sitofp)(Float64,0)::Float64,Float64)::Float64
      # meta: pop location
      unless (Base.or_int)((Base.lt_float)(x::Float64,fy::Float64)::Bool,(Base.and_int)((Base.and_int)((Base.eq_float)(x::Float64,fy::Float64)::Bool,(Base.lt_float)(fy::Float64,9.223372036854776e18)::Bool)::Bool,(Base.slt_int)((Base.fptosi)(Int64,fy::Float64)::Int64,0)::Bool)::Bool)::Bool goto 9
      #temp#@_5::UNION{FLOAT64,INT64} = 0
      goto 11
      9:
      #temp#@_5::UNION{FLOAT64,INT64} = x::Float64
      11:
      # meta: pop location
      $(Expr(:inbounds, :pop))
      y::UNION{FLOAT64,INT64} = #temp#@_5::UNION{FLOAT64,INT64} # line 3:
      unless (y::UNION{FLOAT64,INT64} isa Int64)::ANY goto 19
      #temp#@_6::Core.MethodInstance = MethodInstance for *(::Int64, ::Float64)
      goto 28
      19:
      unless (y::UNION{FLOAT64,INT64} isa Float64)::ANY goto 23
      #temp#@_6::Core.MethodInstance = MethodInstance for *(::Float64, ::Float64)
      goto 28
      23:
      goto 25
      25:
      #temp#@_7::Float64 = (y::UNION{FLOAT64,INT64} * x::Float64)::Float64
      goto 30
      28:
      #temp#@_7::Float64 = $(Expr(:invoke, :(#temp#@_6), :(Main.*), :(y), :(x)))
      30:
      return $(Expr(:invoke, MethodInstance for sin(::Float64), :(Main.sin), :((Base.add_float)(#temp#@_7,(Base.sitofp)(Float64,1)::Float64)::Float64)))
  end::Float64
```

Interpreting the output of [`@code_warntype`](@ref), like that of its cousins [`@code_lowered`](@ref),
[`@code_typed`](@ref), [`@code_llvm`](@ref), and [`@code_native`](@ref), takes a little practice.
Your code is being presented in form that has been partially digested on its way to generating
compiled machine code.  Most of the expressions are annotated by a type, indicated by the `::T`
(where `T` might be [`Float64`](@ref), for example). The most important characteristic of [`@code_warntype`](@ref)
is that non-concrete types are displayed in red; in the above example, such output is shown in
all-caps.

The top part of the output summarizes the type information for the different variables internal
to the function. You can see that `y`, one of the variables you created, is a `Union{Int64,Float64}`,
due to the type-instability of `pos`.  There is another variable, `_var4`, which you can see also
has the same type.

The next lines represent the body of `f`. The lines starting with a number followed by a colon
(`1:`, `2:`) are labels, and represent targets for jumps (via `goto`) in your code.  Looking at
the body, you can see that `pos` has been *inlined* into `f`--everything before `2:` comes from
code defined in `pos`.

Starting at `2:`, the variable `y` is defined, and again annotated as a `Union` type.  Next, we
see that the compiler created the temporary variable `_var1` to hold the result of `y*x`. Because
a [`Float64`](@ref) times *either* an [`Int64`](@ref) or `Float64` yields a `Float64`,
all type-instability ends here. The net result is that `f(x::Float64)` will not be type-unstable
in its output, even if some of the intermediate computations are type-unstable.

How you use this information is up to you. Obviously, it would be far and away best to fix `pos`
to be type-stable: if you did so, all of the variables in `f` would be concrete, and its performance
would be optimal.  However, there are circumstances where this kind of *ephemeral* type instability
might not matter too much: for example, if `pos` is never used in isolation, the fact that `f`'s
output is type-stable (for [`Float64`](@ref) inputs) will shield later code from the propagating
effects of type instability.  This is particularly relevant in cases where fixing the type instability
is difficult or impossible: for example, currently it's not possible to infer the return type
of an anonymous function.  In such cases, the tips above (e.g., adding type annotations and/or
breaking up functions) are your best tools to contain the "damage" from type instability.
Also, note that even Julia Base has functions that are type unstable.
For example, the function [`findfirst`](@ref) returns the index into an array where a key is found,
or `nothing` if it is not found, a clear type instability. In order to make it easier to find the
type instabilities that are likely to be important, `Union`s containing either `missing` or `nothing`
are color highlighted in yellow, instead of red.

The following examples may help you interpret expressions marked as containing non-leaf types:

  * Function body ending in `end::Union{T1,T2})`

      * Interpretation: function with unstable return type
      * Suggestion: make the return value type-stable, even if you have to annotate it
  * `f(x::T)::Union{T1,T2}`

      * Interpretation: call to a type-unstable function
      * Suggestion: fix the function, or if necessary annotate the return value
  * `(top(arrayref))(A::Array{Any,1},1)::Any`

      * Interpretation: accessing elements of poorly-typed arrays
      * Suggestion: use arrays with better-defined types, or if necessary annotate the type of individual
        element accesses
  * `(top(getfield))(A::ArrayContainer{Float64},:data)::Array{Float64,N}`

      * Interpretation: getting a field that is of non-leaf type. In this case, `ArrayContainer` had a
        field `data::Array{T}`. But `Array` needs the dimension `N`, too, to be a concrete type.
      * Suggestion: use concrete types like `Array{T,3}` or `Array{T,N}`, where `N` is now a parameter
        of `ArrayContainer`
