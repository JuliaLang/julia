# Julia Functions

This document will explain how functions, method definitions, and method tables work.

## Method Tables

Every function in Julia is a generic function. A generic function is conceptually a single function,
but consists of many definitions, or methods. The methods of a generic function are stored in
a method table. Method tables (type `MethodTable`) are associated with `TypeName`s. A `TypeName`
describes a family of parameterized types. For example `Complex{Float32}` and `Complex{Float64}`
share the same `Complex` type name object.

All objects in Julia are potentially callable, because every object has a type, which in turn
has a `TypeName`.

## Function calls

Given the call `f(x,y)`, the following steps are performed: first, the method table to use is
accessed as `typeof(f).name.mt`. Second, an argument tuple type is formed, `Tuple{typeof(f), typeof(x), typeof(y)}`.
Note that the type of the function itself is the first element. This is because the type might
have parameters, and so needs to take part in dispatch. This tuple type is looked up in the method
table.

This dispatch process is performed by `jl_apply_generic`, which takes two arguments: a pointer
to an array of the values f, x, and y, and the number of values (in this case 3).

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
`jl_first_argument_datatype` is called to extract the relevant method table from what would be
the type of the first argument. This is much more complicated than the corresponding procedure
during dispatch, since the argument tuple type might be abstract. For example, we can define:

```julia
(::Union{Foo{Int},Foo{Int8}})(x) = 0
```

which works since all possible matching methods would belong to the same method table.

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

A constructor call is just a call to a type. The type of most types is `DataType`, so the method
table for `DataType` contains most constructor definitions. One wrinkle is the fallback definition
that makes all types callable via `convert`:

```julia
(::Type{T})(args...) where {T} = convert(T, args...)::T
```

In this definition the function type is abstract, which is not normally supported. To make this
work, all subtypes of `Type` (`Type`, `UnionAll`, `Union`, and `DataType`) currently share
a method table via special arrangement.

## Builtins

The "builtin" functions, defined in the `Core` module, are:

```
=== typeof sizeof <: isa typeassert throw tuple getfield setfield! fieldtype
nfields isdefined arrayref arrayset arraysize applicable invoke apply_type _apply
_expr svec
```

These are all singleton objects whose types are subtypes of `Builtin`, which is a subtype of
`Function`. Their purpose is to expose entry points in the run time that use the "jlcall" calling
convention:

```c
jl_value_t *(jl_value_t*, jl_value_t**, uint32_t)
```

The method tables of builtins are empty. Instead, they have a single catch-all method cache entry
(`Tuple{Vararg{Any}}`) whose jlcall fptr points to the correct function. This is kind of a hack
but works reasonably well.

## Keyword arguments

Keyword arguments work by associating a special, hidden function object with each method table
that has definitions with keyword arguments. This function is called the "keyword argument sorter"
or "keyword sorter", or "kwsorter", and is stored in the `kwsorter` field of `MethodTable` objects.
Every definition in the kwsorter function has the same arguments as some definition in the normal
method table, except with a single `Array` argument prepended. This array contains alternating
symbols and values that represent the passed keyword arguments. The kwsorter's job is to move
keyword arguments into their canonical positions based on name, plus evaluate and substite any
needed default value expressions. The result is a normal positional argument list, which is then
passed to yet another function.

The easiest way to understand the process is to look at how a keyword argument method definition
is lowered. The code:

```julia
function circle(center, radius; color = black, fill::Bool = true, options...)
    # draw
end
```

actually produces *three* method definitions. The first is a function that accepts all arguments
(including keywords) as positional arguments, and includes the code for the method body. It has
an auto-generated name:

```julia
function #circle#1(color, fill::Bool, options, circle, center, radius)
    # draw
end
```

The second method is an ordinary definition for the original `circle` function, which handles
the case where no keyword arguments are passed:

```julia
function circle(center, radius)
    #circle#1(black, true, Any[], circle, center, radius)
end
```

This simply dispatches to the first method, passing along default values. Finally there is the
kwsorter definition:

```
function (::Core.kwftype(typeof(circle)))(kw::Array, circle, center, radius)
    options = Any[]
    color = arg associated with :color, or black if not found
    fill = arg associated with :fill, or true if not found
    # push remaining elements of kw into options array
    #circle#1(color, fill, options, circle, center, radius)
end
```

The front end generates code to loop over the `kw` array and pick out arguments in the right order,
evaluating default expressions when an argument is not found.

The function `Core.kwftype(t)` fetches (and creates, if necessary) the field `t.name.mt.kwsorter`.

This design has the feature that call sites that don't use keyword arguments require no special
handling; everything works as if they were not part of the language at all. Call sites that do
use keyword arguments are dispatched directly to the called function's kwsorter. For example the
call:

```julia
circle((0,0), 1.0, color = red; other...)
```

is lowered to:

```julia
kwfunc(circle)(Any[:color,red,other...], circle, (0,0), 1.0)
```

The unpacking procedure represented here as `other...` actually further unpacks each *element*
of `other`, expecting each one to contain two values (a symbol and a value). `kwfunc` (also in
`Core`) fetches the kwsorter for the called function. Notice that the original `circle` function
is passed through, to handle closures.

## Compiler efficiency issues

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

The next issue concerns the structure of method cache hash tables. Empirical studies show that
the vast majority of dynamically-dispatched calls involve one or two arguments. In turn, many
of these cases can be resolved by considering only the first argument. (Aside: proponents of single
dispatch would not be surprised by this at all. However, this argument means "multiple dispatch
is easy to optimize in practice", and that we should therefore use it, *not* "we should use single
dispatch"!) So the method cache uses the type of the first argument as its primary key. Note,
however, that this corresponds to the *second* element of the tuple type for a function call (the
first element being the type of the function itself). Typically, type variation in head position
is extremely low -- indeed, the majority of functions belong to singleton types with no parameters.
However, this is not the case for constructors, where a single method table holds constructors
for every type. Therefore the `Type` method table is special-cased to use the *first* tuple type
element instead of the second.

The front end generates type declarations for all closures. Initially, this was implemented by
generating normal type declarations. However, this produced an extremely large number of constructors,
all of which were trivial (simply passing all arguments through to [`new`](@ref)). Since methods are partially
ordered, inserting all of these methods is O(n^2), plus there are just too many of them to keep
around. This was optimized by generating `struct_type` expressions directly (bypassing default
constructor generation), and using `new` directly to create closure instances. Not the prettiest
thing ever, but you do what you gotta do.

The next problem was the `@test` macro, which generated a 0-argument closure for each test case.
This is not really necessary, since each test case is simply run once in place. Therefore I modified
`@test` to expand to a try-catch block that records the test result (true, false, or exception
raised) and calls the test suite handler on it.
