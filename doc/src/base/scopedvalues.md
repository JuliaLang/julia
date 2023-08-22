# Scoped Values

Scoped values provide an implementation of dynamic scoping in Julia.
Dynamic scope means that the state of the value is dependent on the execution path
of the program. This means that for a scoped value you may observe
multiple different values at the same time.

!!! compat "Julia 1.11"
    Scoped values were introduced in Julia 1.11. In Julia 1.8+ a compatible
    implementation is available from the package ScopedValues.jl.

In its simplest form you can create a [`ScopedValue`](@ref) with a
default value and then use [`scoped`](@ref) or [`@scoped`](@ref) to
enter a new dynamic scope.
The new scope will inherit all values from the parent scope
(and recursively from all outer scopes) with the provided scoped
value taking priority over previous definitions.

```julia
const scoped_val = ScopedValue(1)
const scoped_val2 = ScopedValue(0)

# Enter a new dynamic scope and set value
@show scoped_val[] # 1
@show scoped_val2[] # 0
scoped(scoped_val => 2) do
    @show scoped_val[] # 2
    @show scoped_val2[] # 0
    scoped(scoped_val => 3, scoped_val2 => 5) do
        @show scoped_val[] # 3
        @show scoped_val2[] # 5
    end
    @show scoped_val[] # 2
    @show scoped_val2[] # 0
end
@show scoped_val[] # 1
@show scoped_val2[] # 0
```

Since `scoped` requires a closure or a function and creates another call-frame,
it can sometimes be beneficial to use the macro form.

```julia
const STATE = ScopedValue{Union{Nothing, State}}()
with_state(f, state::State) = @scoped(STATE => state, f())
```

!!! note
    Dynamic scopes are propagated through [`Task`](@ref)s.

In the example below we open a new dynamic scope before launching a task.
The parent task and the two child tasks observe independent values of the
same scoped value at the same time.

```julia
import Base.Threads: @spawn
const scoped_val = ScopedValue(1)
@sync begin
    scoped(scoped_val => 2)
        @spawn @show scoped_val[] # 2
    end
    scoped(scoped_val => 3)
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
    scoped(sval_dict => Dict())
        @spawn (sval_dict[][:a] = 3)
    end
    scoped(sval_dict => Dict())
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
    scoped(LEVEL => level) do
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
### [Unshare mutable state]((@id unshare_mutable_state))

```julia
import Base.Threads: @spawn
const sval_dict = ScopedValue(Dict())

# If you want to add new values to the dict, instead of replacing
# it, unshare the values explicitly. In this example we use `merge`
# to unshare the state of the dictonary in parent scope.
@sync begin
    scoped(sval_dict => merge(sval_dict, Dict(:a => 10))) do
        @spawn @show sval_dict[][:a]
    end
    @spawn sval_dict[][:a] = 3 # Not a race since they are unshared.
end
```

### Local caching

Since lookup of a scoped variable is linear in scope depth, it can be beneficial
for a library at an API boundary to cache the state of the scoped value.

```julia
const DEVICE = ScopedValue(:CPU)

function solve_problem(args...)
    # Cache current device
    @scoped DEVICE => DEVICE[] begin
        # call functions that use `DEVICE[]` repeatedly.
    end
```

## API docs

```@docs
Base.ScopedValues.ScopedValue
Base.ScopedValues.scoped
Base.ScopedValues.@scoped
```
