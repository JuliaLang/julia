# [The World Age mechanism](@id man-world-age)

!!! note
    World age is an advanced concept. For the vast majority of Julia users, the world age
    mechanism operates invisibly in the background. This documentation is intended for the
    few users who may encounter world-age related issues or error messages.

!!! compat "Julia 1.12"
    Prior to Julia 1.12, the world age mechanism did not apply to changes to the global binding table.
    The documentation in this chapter is specific to Julia 1.12+.

!!! warning
    This manual chapter uses internal functions to introspect world age and runtime data structures
    as an explanatory aid. In general, unless otherwise noted the world age mechanism is not a stable
    interface and should be interacted with in packages through stable APIs (e.g. `invokelatest`) only.
    In particular, do not assume that world ages are always integers or that they have a linear order.

## World age in general

The "world age counter" is a monotonically increasing counter that is incremented for every
change to the global method table or the global binding table (e.g. through method definition,
type definition, `import`/`using` declaration, creation of (typed) globals or definition of constants).

The current value of the global world age counter can be retrieved using the (internal) function [`Base.get_world_counter`](@ref).

```julia-repl
julia> Base.get_world_counter()
0x0000000000009632

julia> const x = 1

julia> Base.get_world_counter()
0x0000000000009633
```

In addition, each [`Task`](@ref) stores a local world age that determines which modifications to
the global binding and method tables are currently visible to the running task. The world age of
the running task will never exceed the global world age counter, but may run arbitrarily behind it.
In general the term "current world age" refers to the local world age of the currently running task.
The current world age may be retrieved using the (internal) function [`Base.tls_world_age`](@ref)

```julia-repl
julia> function f end
f (generic function with 0 methods)

julia> begin
           @show (Int(Base.get_world_counter()), Int(Base.tls_world_age()))
           Core.eval(@__MODULE__, :(f() = 1))
           @show (Int(Base.get_world_counter()), Int(Base.tls_world_age()))
           f()
       end
(Int(Base.get_world_counter()), Int(Base.tls_world_age())) = (38452, 38452)
(Int(Base.get_world_counter()), Int(Base.tls_world_age())) = (38453, 38452)
ERROR: MethodError: no method matching f()
The applicable method may be too new: running in current world age 38452, while global world is 38453.

Closest candidates are:
  f() (method too new to be called from this world context.)
   @ Main REPL[2]:3

Stacktrace:
 [1] top-level scope
   @ REPL[2]:5

julia> (f(), Int(Base.tls_world_age()))
(1, 38453)
```

Here the definition of the method `f` raised the global world counter, but the current world
age did not change. As a result, the definition of `f` was not visible in the currently
executing task and a [`MethodError`](@ref) resulted.

!!! note
    The method error printing provided additional information that `f()` is available in a newer world age.
    This information is added by the error display, not the task that threw the `MethodError`.
    The thrown `MethodError` is identical whether or not a matching definition of `f()` exists
    in a newer world age.

However, note that the definition of `f()` was subsequently available at the next REPL prompt, because
the current task's world age had been raised. In general, certain syntactic constructs (in particular most definitions)
will raise the current task's world age to the latest global world age, thus making all changes
(both from the current task and any concurrently executing other tasks) visible. The following statements
raise the current world age:

1. An explicit invocation of `Core.@latestworld`
2. The start of every top-level statement
3. The start of every REPL prompt
4. Any type or struct definition
5. Any method definition
6. Any constant declaration
7. Any global variable declaration (but not a global variable assignment)
8. Any `using`, `import`, `export` or `public` statement
9. Certain other macros like [`@eval`](@ref) (depends on the macro implementation)

Note, however, that the current task's world age may only ever be permanently incremented at
top level. As a general rule, using any of the above statements in non-top-level scope is a syntax error:

```julia-repl
julia> f() = Core.@latestworld
ERROR: syntax: World age increment not at top level
Stacktrace:
 [1] top-level scope
   @ REPL[5]:1
```

When it isn't (for example for `@eval`), the world age side effect is ignored.

As a result of these rules, Julia may assume that the world age does not change
within the execution of an ordinary function.

```julia
function my_function()
    before = Base.tls_world_age()
    # Any arbitrary code
    after = Base.tls_world_age()
    @assert before === after # always true
end
```

This is the key invariant that allows Julia to optimize based on the current state
of its global data structures, while still having the well-defined ability to change
these data structures.

## Temporarily raising the world age using `invokelatest`

As described above, it is not possible to permanently raise the world age for the remainder of
a `Task`'s execution unless the task is executing top-level statements. However, it is possible to
temporarily change the world age in a scoped manner using `invokelatest`:

```jldoctest
julia> function f end
f (generic function with 0 methods)

julia> begin
           Core.eval(@__MODULE__, :(f() = 1))
           invokelatest(f)
       end
1
```

`invokelatest` will temporarily raise the current task's world age to the latest global world age (at
entry to `invokelatest`) and execute the provided function. Note that the world age will return
to its prior value upon exit from `invokelatest`.

## World age and const struct redefinitions

The semantics described above for method redefinition also apply to redefinition of constants:

```jldoctest
julia> const x = 1
1

julia> get_const() = x
get_const (generic function with 1 method)

julia> begin
           @show get_const()
           Core.eval(@__MODULE__, :(const x = 2))
           @show get_const()
           Core.@latestworld
           @show get_const()
       end
get_const() = 1
get_const() = 1
get_const() = 2
2
```

However, for the avoidance of doubt, they do not apply to ordinary assignment to global variables, which becomes visible immediately:
```jldoctest
julia> global y = 1
1

julia> get_global() = y
get_global (generic function with 1 method)

julia> begin
           @show get_global()
           Core.eval(@__MODULE__, :(y = 2))
           @show get_global()
       end
get_global() = 1
get_global() = 2
2
```

One particular special case of constant reassignment is the redefinition of struct types:

```jldoctest; filter = r"\@world\(MyStruct, \d+\:\d+\)"
julia> struct MyStruct
           x::Int
       end

julia> const one_field = MyStruct(1)
MyStruct(1)

julia> struct MyStruct
           x::Int
           y::Float64
       end

julia> const two_field = MyStruct(1, 2.0)
MyStruct(1, 2.0)

julia> one_field
@world(MyStruct, 38452:38455)(1)

julia> two_field
MyStruct(1, 2.0)
```

Internally the two definitions of `MyStruct` are entirely separate types. However,
after the new `MyStruct` type is defined, there is no longer any default binding
for the original definition of `MyStruct`. To nevertheless facilitate access to
these types, the special [`@world`](@ref) macro may be used to access the meaning
of a name in a previous world. However, this facility is intended for introspection
only and in particular note that world age numbers are not stable across precompilation
and should in general be treated opaquely.

### Binding partition introspection

In certain cases, it can be helpful to introspect the system's understanding of what
a binding means in any particular world age. The default display printing of `Core.Binding`
provides a helpful summary (e.g. on the `MyStruct` example from above):

```julia-repl
julia> convert(Core.Binding, GlobalRef(@__MODULE__, :MyStruct))
Binding Main.MyStruct
   38456:∞ - constant binding to MyStruct
   38452:38455 - constant binding to @world(MyStruct, 38452:38455)
   38451:38451 - backdated constant binding to @world(MyStruct, 38452:38455)
   0:38450 - backdated constant binding to @world(MyStruct, 38452:38455)
```

## World age and `using`/`import`

Bindings provided via `using` and `import` also operate via the world age mechanism.
Binding resolution is a stateless function of the `import` and `using` definitions
visible in the current world age. For example:

```julia-repl
julia> module M1; const x = 1; export x; end

julia> module M2; const x = 2; export x; end

julia> using .M1

julia> x
1

julia> using .M2

julia> x
ERROR: UndefVarError: `x` not defined in `Main`
Hint: It looks like two or more modules export different bindings with this name, resulting in ambiguity. Try explicitly importing it from a particular module, or qualifying the name with the module it should come from.

julia> convert(Core.Binding, GlobalRef(@__MODULE__, :x))
Binding Main.x
   38458:∞ - ambiguous binding - guard entry
   38457:38457 - implicit `using` resolved to constant 1
```

## World age capture

Certain language features capture the current task's world age. Perhaps the most common of
these is creation of new tasks. Newly created tasks will inherit the creating task's local
world age at creation time and will retain said world age (unless explicitly raised) even
if the originating tasks raises its world age:

```julia-repl
julia> const x = 1

julia> t = @task (wait(); println("Running now"); x);

julia> const x = 2

julia> schedule(t);
Running now

julia> x
2

julia> fetch(t)
1
```

In addition to tasks, opaque closures also capture their world age at creation. See [`Base.Experimental.@opaque`](@ref).

```@docs
Base.@world
Base.get_world_counter
Base.tls_world_age
Base.invoke_in_world
Base.Experimental.@opaque
```
