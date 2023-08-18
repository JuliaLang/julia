# Scoped Values

Scoped values provide an implementation of dynamical scoping in Julia.
In particular dynamical scopes are propagated through [Task](@ref)s.

!!! compat "Julia 1.11"
    Scoped values were introduced in Julia 1.11. In Julia 1.7+ a compatible
    implementation is available from the package ScopedValues.jl.

In it's simplest form you can create a [`ScopedValue`](@ref) with a
default value and then use [`scoped`](@ref) to enter a new dynamical
scope. The new scope will inherit all values from the parent scope
(and recursivly from all outer scopes) with the the provided scoped
value taking priority over previous definitions.

```julia
const scoped_val = ScopedValue(1)
const scoped_val2 = ScopedValue(2)

# Enter a new dynamic scope and set value
scoped(scoped_val => 2) do
    @show scoped_val[] # 2
    @show scoped_val2[] # 1
    scoped(scoped_val => 3, scoped_val2 => 5) do
        @show scoped_val[] # 3
        @show scoped_val2[] # 5
    end
end
```

Scoped values are constant throughout a scope, but you can store mutable
state in a scoped value. Just keep in mind that the usual caveats
for global variables apply in the context of concurrent programming.

```julia
const scoped_dict = ScopedValue(Dict())

# Important we are using `merge` to "unshare" the mutable values
# across the different views of the same scoped value.
scoped(svar_dict => merge(svar_dict, Dict(:a => 10))) do
    @show svar_dict[][:a]
end
```

Care is also required when storing references to mutable state in scoped
values. You might want to explicitly unshare when entering a new dynamic scope.


In the example below we use a scoped value to implement a permission check in
a web-application. After determining the permissions of the request.
A new dynamic scope is entered and the scoped value `LEVEL` is set.
Other parts of the application can now query the scoped value and will receive
the appropriate value. Other alternatives like task-local storage and global variables,
are not well suited for this kind of propagation and our only alternative would have
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
    open(Database(#=...=#))
    # ...
end
```

## API docs

```@docs
Base.ScopedValues.ScopedValue
Base.ScopedValues.scoped
```
