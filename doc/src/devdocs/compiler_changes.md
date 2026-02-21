# History of changes to the compiler

The Julia compiler is not part of the public interface of Julia's `Core`, and may change in non-breaking Julia releases. This page lists breaking changes introduced to the compiler to aid in adapting to those changes, intended for packages that do interface with the compiler. It is maintained on a best-effort basis and may be incomplete.

## v1.14

### `get_inference_cache` now returns `InferenceCache` instead of `Vector{InferenceResult}`

Custom `AbstractInterpreter` implementations must now return an `InferenceCache` from `get_inference_cache` instead of a `Vector{InferenceResult}`. The `InferenceCache` type maintains an index for fast lookups by `MethodInstance`, improving inference performance for large compilation workloads.

To migrate, change:
```julia
struct MyInterpreter <: AbstractInterpreter
    inf_cache::Vector{InferenceResult}
end
MyInterpreter() = MyInterpreter(InferenceResult[])
get_inference_cache(interp::MyInterpreter) = interp.inf_cache
```

To:
```julia
struct MyInterpreter <: AbstractInterpreter
    inf_cache::InferenceCache
end
MyInterpreter() = MyInterpreter(InferenceCache())
get_inference_cache(interp::MyInterpreter) = interp.inf_cache
```

The `InferenceCache` type supports `push!`, iteration, and indexing for compatibility with common usage patterns.

## v1.12

### Changes to the `IRCode` type

The `linetable` field of `IRCode` has been removed, and the source line information is now available in a different format in the `debuginfo` field. ([#52415](https://github.com/JuliaLang/julia/pull/52415))

Related to the above, the `verify_linetable` function changed its signature from `verify_linetable(::Vector{Core.LineInfoNode})` to `verify_linetable(::DebugInfoStream, nstmts::Int64)`, the second argument being the number of statements.

`IRCode` now has a `valid_worlds` field, to explicitly communicate which worlds are
to be considered for type inference and optimization purposes. As 1.12 introduced world-age
partitioned bindings, this information is key to properly infer through global bindings.
An upper bound that is not constrained enough may lead to a failure to accurately infer and optimize these.

### `InstructionStream` constructors have gotten stricter

The constructor for `InstructionStream` now requires the arguments to be of exactly the correct types. The fields haven't changed, but for instance you need to cast the second argument called `type` to be a `Vector{Any}` instead of being able to pass in something like a `Vector{Type}` and relying on an automatic cast.

### `CodeInstance`s replacing certain uses of `MethodInstance`s

`CodeInstance` has gradually replaced `MethodInstance` in more and more places, over several
extensive PRs. `CodeInstance` holds the cached results of inference, and usually end up stored in
the `.cache` field of their parent `MethodInstance`s (with the `.next` field of `CodeInstance`
allowing a linked list to be formed by holding further `CodeInstance`s).

One notable change is that the first argument to `invoke` expressions is now a `CodeInstance`
instead of a `MethodInstance`.

The parent `MethodInstance` of a `CodeInstance` may be accessed with `Compiler.get_ci_mi(code_instance)`.

### `SpecInfo` replaces `MethodInfo`

`MethodInfo` was renamed to `SpecInfo`, and two fields `nargs::Int` and `isva::Bool` have been prepended to it, indicating how many arguments a specialization takes, and whether its signature is variadic (of the form `f(x...)`). ([#55976](https://github.com/JuliaLang/julia/pull/55976))

### Changes to `OpaqueClosure`s

`OpaqueClosure` now requires the first argument type of its source `IRCode` to be the type of
its environment (passed through `env...`), such as `Tuple{Int, Float64}` or `Tuple{}` for no captures. ([#54458](https://github.com/JuliaLang/julia/pull/54458))
