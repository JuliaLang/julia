# [Scoped Values](@id scoped-values)

Scoped values provide an implementation of dynamic scoping in Julia.

!!! note "Lexical scoping vs dynamic scoping"
    [Lexical scoping](@ref scope-of-variables) is the default behavior in Julia.
    Under lexical scoping the scope of a variable is determined by the lexical
    (textual) structure of a program.
    Under dynamic scoping a variable is bound to the most recent assigned value
    during the program's execution.

The state of a scoped value is dependent on the execution path of the program.
This means that for a scoped value you may observe multiple different values
concurrently.

!!! compat "Julia 1.11"
    Scoped values were introduced in Julia 1.11. In Julia 1.8+ a compatible
    implementation is available from the package ScopedValues.jl.

In its simplest form you can create a [`ScopedValue`](@ref) with a
default value and then use [`with`](@ref Base.with) or [`@with`](@ref) to
enter a new dynamic scope.

The new scope will inherit all values from the parent scope
(and recursively from all outer scopes) with the provided scoped
value taking priority over previous definitions.

Let's first look at an example of **lexical** scope:

A `let` statements begins a new lexical scope within which the outer definition
of `x` is shadowed by it's inner definition.

```julia
x = 1
let x = 5
    @show x # 5
end
@show x # 1
```

Since Julia uses lexical scope the variable `x` is bound within the function `f`
to the global scope and entering a `let` scope does not change the value `f`
observes.

```julia
x = 1
f() = @show x
let x = 5
    f() # 1
end
f() # 1
```

Now using a `ScopedValue` we can use **dynamic** scoping.

```julia
x = ScopedValue(1)
f() = @show x[]
with(x=>5) do
    f() # 5
end
f() # 1
```

Not that the observed value of the `ScopedValue` is dependent on the execution
path of the program.

It often makes sense to use a `const` variable to point to a scoped value,
and you can set the value of multiple `ScopedValue`s with one call to `with`.


```julia
const scoped_val = ScopedValue(1)
const scoped_val2 = ScopedValue(0)

# Enter a new dynamic scope and set value
@show scoped_val[] # 1
@show scoped_val2[] # 0
with(scoped_val => 2) do
    @show scoped_val[] # 2
    @show scoped_val2[] # 0
    with(scoped_val => 3, scoped_val2 => 5) do
        @show scoped_val[] # 3
        @show scoped_val2[] # 5
    end
    @show scoped_val[] # 2
    @show scoped_val2[] # 0
end
@show scoped_val[] # 1
@show scoped_val2[] # 0
```

Since `with` requires a closure or a function and creates another call-frame,
it can sometimes be beneficial to use the macro form.

```julia
const STATE = ScopedValue{Union{Nothing, State}}()
with_state(f, state::State) = @with(STATE => state, f())
```

!!! note
    Dynamic scopes are inherited by [`Task`](@ref)s, at the moment of task creation. Dynamic scopes are **not** propagated through `Distributed.jl` operations.

In the example below we open a new dynamic scope before launching a task.
The parent task and the two child tasks observe independent values of the
same scoped value at the same time.

```julia
import Base.Threads: @spawn
const scoped_val = ScopedValue(1)
@sync begin
    with(scoped_val => 2)
        @spawn @show scoped_val[] # 2
    end
    with(scoped_val => 3)
        @spawn @show scoped_val[] # 3
    end
    @show scoped_val[] # 1
end
```

Scoped values are constant throughout a scope, but you can store mutable
state in a scoped value. Just keep in mind that the usual caveats
for global variables apply in the context of concurrent programming.

Care is also required when storing references to mutable state in scoped
values. You might want to explicitly [unshare mutable state](@ref unshare_mutable_state)
when entering a new dynamic scope.

```julia
import Base.Threads: @spawn
const sval_dict = ScopedValue(Dict())

# Example of using a mutable value wrongly
@sync begin
    # `Dict` is not thread-safe the usage below is invalid
    @spawn (sval_dict[][:a] = 3)
    @spawn (sval_dict[][:b] = 3)
end

@sync begin
    # If we instead pass a unique dictionary to each
    # task we can access the dictonaries race free.
    with(sval_dict => Dict()) do
        @spawn (sval_dict[][:a] = 3)
    end
    with(sval_dict => Dict()) do
        @spawn (sval_dict[][:b] = 3)
    end
end
```

## Example

In the example below we use a scoped value to implement a permission check in
a web-application. After determining the permissions of the request,
a new dynamic scope is entered and the scoped value `LEVEL` is set.
Other parts of the application can query the scoped value and will receive
the appropriate value. Other alternatives like task-local storage and global variables
are not well suited for this kind of propagation; our only alternative would have
been to thread a value through the entire call-chain.

```julia
const LEVEL = ScopedValue(:GUEST)

function serve(request, response)
    level = isAdmin(request) ? :ADMIN : :GUEST
    with(LEVEL => level) do
        Threads.@spawn handle(request, respone)
    end
end

function open(connection::Database)
    level = LEVEL[]
    if level !== :ADMIN
        error("Access disallowed")
    end
    # ... open connection
end

function handle(request, response)
    # ...
    open(Database(#=...=#))
    # ...
end
```

## Idioms
### [Unshare mutable state](@id unshare_mutable_state)

```julia
import Base.Threads: @spawn
const sval_dict = ScopedValue(Dict())

# If you want to add new values to the dict, instead of replacing
# it, unshare the values explicitly. In this example we use `merge`
# to unshare the state of the dictonary in parent scope.
@sync begin
    with(sval_dict => merge(sval_dict[], Dict(:a => 10))) do
        @spawn @show sval_dict[][:a]
    end
    @spawn sval_dict[][:a] = 3 # Not a race since they are unshared.
end
```

### [Local caching](@id local_caching)

Since lookup of a scoped variable is linear in scope depth, it can be beneficial
for a library at an API boundary to cache the state of the scoped value.

```julia
const DEVICE = ScopedValue(:CPU)

function solve_problem(args...)
    # Cache current device, by putting it
    # on top of the scope stack.
    @with DEVICE => DEVICE[] begin
        # call functions that use `DEVICE[]` repeatedly.
    end
```

### Scoped values as globals

In order to access the value of a scoped value, the scoped value itself has to
be in (lexical) scope. This means most often you likely want to use scoped values
as constant globals.

```julia
const sval = ScopedValue(1)
```

Indeed one can think of scoped values as hidden function arguments.

This does not preclude their use as non-globals.

```julia
import Base.Threads: @spawn
function main()
    role = ScopedValue(:client)

    function launch()
        #...
        role[]
    end

    @with role => :server @spawn launch()
    launch()
end
```

But it might have been simpler to just directly pass the function argument
in these cases.

### Very many ScopedValues

If you find yourself creating many `ScopedValue`'s for one given module,
it may be better to use a dedicated struct to hold them.

```julia
Base.@kwdef struct Configuration
    color::Bool = false
    verbose::Bool = false
end

const CONFIG = ScopedValue(Configuration())

@with CONFIG => Configuration(CONFIG[], color=true) begin
    @show CONFIG[].color # true
    @show CONFIG[].verbose # false
end
```

## API docs

```@docs
Base.ScopedValues.ScopedValue
Base.ScopedValues.with
Base.ScopedValues.@with
```

## Implementation notes and performance

`Scope`s form a linked-list that must be searched to find the most recent
assignment of the scoped value. In essence `Scope` is an immutable dictionary
from `ScopeValue` to current value. Scope entry is `O(1)` and cheap but
accessing a scoped value has a cost of `O(n)` where `n` is the scope depth.

[Local caching](@ref local_caching) allows the developer to mitigate this cost
if frequent access to a particular scoped value is required.
The `Scope` object itself is not user-facing and may be replaced in a future
version of Julia.

## Design inspiration

This design was heavily inspired by [JEPS-429](https://openjdk.org/jeps/429),
which in turn was inspired by dynamically scoped free variables in many Lisp dialects. In particular Interlisp-D and it's deep binding strategy.

A prior design discussed was context variables ala [PEPS-567](https://peps.python.org/pep-0567/) and implemented in Julia as [ContextVariablesX.jl](https://github.com/tkf/ContextVariablesX.jl).
