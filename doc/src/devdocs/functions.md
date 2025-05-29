# Julia Functions


This document will explain how functions, method definitions, and method tables work.

## Method Tables

Every function in Julia is a generic function. A generic function is conceptually a single function,
but consists of many definitions, or methods. The methods of a generic function are stored in a
method table. There is one global method table (type `MethodTable`) named `Core.GlobalMethods`. Any
default operation on methods (such as calls) uses that table.

## [Function calls](@id Function-calls)

Given the call `f(x, y)`, the following steps are performed: First, a tuple type is formed,
`Tuple{typeof(f), typeof(x), typeof(y)}`. Note that the type of the function itself is the first
element. This is because the function itself participates symmetrically in method lookup with the
other arguments. This tuple type is looked up in the global method table. However, the system can
then cache the results, so these steps can be skipped later for similar lookups.

This dispatch process is performed by `jl_apply_generic`, which takes two arguments: a pointer
to an array of the values `f`, `x`, and `y`, and the number of values (in this case 3).

Throughout the system, there are two kinds of APIs that handle functions and argument lists: those
that accept the function and arguments separately, and those that accept a single argument structure.
In the first kind of API, the "arguments" part does *not* contain information about the function,
since that is passed separately. In the second kind of API, the function is the first element
of the argument structure.

For example, the following function for performing a call accepts just an `args` pointer, so the
first element of the args array will be the function to call:

```c
jl_value_t *jl_apply(jl_value_t **args, uint32_t nargs)
```

This entry point for the same functionality accepts the function separately, so the `args` array
does not contain the function:

```c
jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, int32_t nargs);
```

## Adding methods

Given the above dispatch process, conceptually all that is needed to add a new method is (1) a
tuple type, and (2) code for the body of the method. `jl_method_def` implements this operation.

## Creating generic functions

Since every object is callable, nothing special is needed to create a generic function. Therefore
`jl_new_generic_function` simply creates a new singleton (0 size) subtype of `Function` and returns
its instance. A function can have a mnemonic "display name" which is used in debug info and when
printing objects. For example the name of `Base.sin` is `sin`. By convention, the name of the
created *type* is the same as the function name, with a `#` prepended. So `typeof(sin)` is `Base.#sin`.

## Closures

A closure is simply a callable object with field names corresponding to captured variables. For
example, the following code:

```julia
function adder(x)
    return y->x+y
end
```

is lowered to (roughly):

```julia
struct ##1{T}
    x::T
end

(_::##1)(y) = _.x + y

function adder(x)
    return ##1(x)
end
```

## Constructors

A constructor call is just a call to a type, to a method defined on `Type{T}`.

## Builtins

The "builtin" functions, defined in the `Core` module, are:

```@eval
function lines(words)
    io = IOBuffer()
    n = 0
    for w in words
        if n+length(w) > 80
            print(io, '\n', w)
            n = length(w)
        elseif n == 0
            print(io, w);
            n += length(w)
        else
            print(io, ' ', w);
            n += length(w)+1
        end
    end
    takestring!(io)
end
import Markdown
[string(n) for n in names(Core;all=true)
    if getfield(Core,n) isa Core.Builtin && nameof(getfield(Core,n)) === n] |>
    lines |>
    s ->  "```\n$s\n```" |>
    Markdown.parse
```

These are mostly singleton objects all of whose types are subtypes of `Builtin`, which is a
subtype of `Function`. Their purpose is to expose entry points in the run time that use the
"jlcall" calling convention:

```c
jl_value_t *(jl_value_t*, jl_value_t**, uint32_t)
```

## Keyword arguments

Keyword arguments work by adding methods to the kwcall function. This function
is usually the "keyword argument sorter" or "keyword sorter", which then calls
the inner body of the function (defined anonymously).
Every definition in the kwsorter function has the same arguments as some definition in the normal
method table, except with a single `NamedTuple` argument prepended, which gives
the names and values of passed keyword arguments. The kwsorter's job is to move keyword arguments
into their canonical positions based on name, plus evaluate and substitute any needed default value
expressions. The result is a normal positional argument list, which is then passed to yet another
compiler-generated function.

The easiest way to understand the process is to look at how a keyword argument method definition
is lowered. The code:

```julia
function circle(center, radius; color = black, fill::Bool = true, options...)
    # draw
end
```

actually produces *three* method definitions. The first is a function that accepts all arguments
(including keyword arguments) as positional arguments, and includes the code for the method body.
It has an auto-generated name:

```julia
function #circle#1(color, fill::Bool, options, circle, center, radius)
    # draw
end
```

The second method is an ordinary definition for the original `circle` function, which handles
the case where no keyword arguments are passed:

```julia
function circle(center, radius)
    #circle#1(black, true, pairs(NamedTuple()), circle, center, radius)
end
```

This simply dispatches to the first method, passing along default values.
`pairs` is applied to the named tuple of rest arguments to provide key-value pair iteration.
Note that if the method doesn't accept rest keyword arguments then this argument
is absent.

Finally there is the kwsorter definition:

```
function (::Core.kwcall)(kws, circle, center, radius)
    if haskey(kws, :color)
        color = kws.color
    else
        color = black
    end
    # etc.

    # put remaining kwargs in `options`
    options = structdiff(kws, NamedTuple{(:color, :fill)})

    # if the method doesn't accept rest keywords, throw an error
    # unless `options` is empty

    #circle#1(color, fill, pairs(options), circle, center, radius)
end
```


## [Compiler efficiency issues](@id compiler-efficiency-issues)

Generating a new type for every function has potentially serious consequences for compiler resource
use when combined with Julia's "specialize on all arguments by default" design. Indeed, the initial
implementation of this design suffered from much longer build and test times, higher memory use,
and a system image nearly 2x larger than the baseline. In a naive implementation, the problem
is bad enough to make the system nearly unusable. Several significant optimizations were needed
to make the design practical.

The first issue is excessive specialization of functions for different values of function-valued
arguments. Many functions simply "pass through" an argument to somewhere else, e.g. to another
function or to a storage location. Such functions do not need to be specialized for every closure
that might be passed in. Fortunately this case is easy to distinguish by simply considering whether
a function *calls* one of its arguments (i.e. the argument appears in "head position" somewhere).
Performance-critical higher-order functions like `map` certainly call their argument function
and so will still be specialized as expected. This optimization is implemented by recording which
arguments are called during the `analyze-variables` pass in the front end. When `cache_method`
sees an argument in the `Function` type hierarchy passed to a slot declared as `Any` or `Function`,
it behaves as if the `@nospecialize` annotation were applied. This heuristic seems to be extremely
effective in practice.

The next issue concerns the structure of method tables. Empirical studies show that the vast
majority of dynamically-dispatched calls involve one or two arguments. In turn, many of these cases
can be resolved by considering only the first argument. (Aside: proponents of single dispatch would
not be surprised by this at all. However, this argument means "multiple dispatch is easy to optimize
in practice", and that we should therefore use it, *not* "we should use single dispatch"!). So the
method table and cache splits up on the structure based on a left-to-right decision tree so allow
efficient nearest-neighbor searches.

The front end generates type declarations for all closures. Initially, this was implemented by
generating normal type declarations. However, this produced an extremely large number of constructors,
all of which were trivial (simply passing all arguments through to [`new`](@ref)). Since methods are partially
ordered, inserting all of these methods is O(n²), plus there are just too many of them to keep
around. This was optimized by generating `struct_type` expressions directly (bypassing default
constructor generation), and using `new` directly to create closure instances. Not the prettiest
thing ever, but you do what you gotta do.

The next problem was the `@test` macro, which generated a 0-argument closure for each test case.
This is not really necessary, since each test case is simply run once in place. Therefore, `@test`
was modified to expand to a try-catch block that records the test result (true, false, or exception
raised) and calls the test suite handler on it.
