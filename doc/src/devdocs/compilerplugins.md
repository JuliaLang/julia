# Compiler plugins

!!! warning
    Compiler plugins are an unstable feature and depend on compiler internals.
    Users of compiler plugins are encouraged to actively keep track of Julia upstream,
    and to contribute necessary enhancements.

Compiler plugins use the `AbstractInterpreter` interface to customize Julia's high-level compiler,
and the `AbstractCompiler` interface to make the results from a foreign abstract interpreter executable.

Each task has a private field `compiler` that is like a scoped value to set and propagate the compiler plugin
across a task-graph. It can be accessed through `Core.Compiler.current_compiler()` and set through `Base.invoke_within`.

Instances of `AbstractCompiler` are used a cache lookup tokens and are compared with `jl_egal`.

!!! note
    The native compiler of Julia uses `nothing` as a compiler instance and cache token. Switching back to the native compiler
    can thus be done with `Base.invoke_within(nothing, f, args...)`.

## Interface

```@docs
Core.Compiler.AbstractCompiler
Core.Compiler.current_compiler
Core.Compiler.abstract_interpreter
Core.Compiler.compiler_world
Base.invoke_within
```

!!! important
    While the abstract interpreter interface can be used independently of compiler plugins, if it is used for a compiler plugin.
    `CC.cache_owner(interp::AbstractInterpreter)` must return as a cache token the `AbstractCompiler` instance that was used
    to construct the `AbstractInterpreter` with `Core.Compiler.abstract_interpreter`.

## Intermediate representations and compiler pipeline

### Pre-inference IR
### Post-inference IR


## Freezing compiler world-ages

It is often undesirable for compiler code to be invalidated by user code. Julia's `Core.Compiler`
as executed by `jl_type_infer` is run in a frozen world-age (`jl_typeinf_world`). Compiler plugins
are not reachable from that world-age and need to be executed in a newer one.

The API [`Core.Compiler.compiler_world`](@ref) can be used to return a frozen world-age.

```julia
const CC = Core.Compiler

struct MyCompiler <: CC.AbstractCompiler end
CC.abstract_interpreter(compiler::MyCompiler, world::UInt) = # ...

const COMPILER_WORLD = Ref{UInt}(0)

function __init__()
    COMPILER_WORLD[] = Base.get_world_counter()
end

CC.compiler_world(::MyCompiler) = COMPILER_WORLD[]
```

By default, `compiler_world` returns `get_world_counter()`, thus `invoke_in_world(compiler_world(...), ...)`
being equivalent to `invokelatest`. This default definition can be useful during development.

## Runtime executed implicit functions

The Julia runtime executes implicit functions like finalizers and `__init__` functions.
Currently, these are all executed within the native compiler.

!!! warning
    This is currently not fully implemented and might cause spooky action at a distance.

## Accidental recursive instrumentation

## Future work

- Compiler standard library
- Cross-compiler inference
  - Inlining?

## Examples

### Tracing

### Broadcast fusion