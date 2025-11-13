The Julia compiler is not part of the public interface of Julia's `Core`, and may change in non-breaking Julia releases. However, since several community packages do interface with the compiler, this file lists breaking changes introduced to the compiler, to aid in adapting to those changes.

# v1.12

## Changes to the `IRCode` type

The `linetable` field of `IRCode` has been removed. The same information is now available in a different format in the `debuginfo` field. TODO: Add a high level explanation of the differences and how to adapt.

Related to the above, the `verify_linetable` function that previously took in a `Vector{Core.LineInfoNode}` now wants a `DebugInfoStream`, and also a second argument `nstmts::Int64` for the number of statements.

`IRCode` now has a new field called `valid_worlds`. TODO Explain what it does.

## `InstructionStream` constructors have gotten stricter

The constructor for `InstructionStream` now requires the arguments to be of exactly the correct types. The fields haven't changed, but for instance you need to cast the second argument called `type` to be a `Vector{Any}` instead of being able to pass in something like a `Vector{Type}` and relying on an automatic cast.

## `CodeInstance`s rather than `MethodInstance`s

Something in the `IRCode` type has changed to internally use `CodeInstance` rather than directly `MethodInstance`. For instance, this
```julia
f(a) = println(a)
ir = Base.code_ircode_by_type(Tuple{typeof(f), Int})[1][1]
println(ir.stmts[1].data.stmt[1].args[1])
```
on 1.11 is `MethodInstance for println(::Int64)` but on 1.12 `CodeInstance for MethodInstance for println(::Int64)`.

TODO Explain this whole thing properly.

## `SpecInfo` replaces `MethodInfo`

TODO Explain what the difference is and where this comes up.

## Changes to `OpaqueClosure`s

TODO Explain what's changed.
