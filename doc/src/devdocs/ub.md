# Undefined Behavior

In programming language design, it is prudent to separate the concepts of a language's semantics, and the behavior of a language's implementation. A language's semantics define the allowable set of *observable* behaviors (including defining what it means to be observable). A correct implementation will ensure that the actual behavior of executing a program has observable behavior that is allowable according to the language semantics. This is often referred to as the **as-if** rule in other languages.

To illustrate the distinction, consider a statement like `print(Ref(1).x)`. The language semantics may specify that the observable behavior of this statement is that the value `1` is printed to `stdout`. However, whether or not the object `Ref` is actually allocated may not be semantically observable (even though it may be implicitly observable by looking at memory use, number of allocations, generated code, etc.). Because of this, the implementation is allowed to replace this statement with `print(1)`, which preserves all semantically observable behaviors.

Additionally, the allowable behaviors for a given program are not unique. For example, the `@fastmath` macro gives wide semantic latitude for floating point math rearrangements and two subsequent invocations of the same operation inside of that macro, even on the same values, are not semantically required to produce the same answer. The situation is similar for asynchronous operations, random number generation, etc.

*Undefined Behavior* (UB) occurs when a julia program semantically performs an operation that is assumed to never happen. In such a situation, the language semantics do not constrain the behavior of the implementation, so any behavior of the program is allowable, including crashes, memory corruption, incorrect behavior, etc. As such, it is very important to avoid writing programs that semantically execute undefined behavior.

Note that this explicitly applies to *semantically executed* undefined behavior. While julia's compiler is allowed to and does aggressively perform speculative execution of pure functions. Since the execution point is not semantically observable (though again indirectly observable through execution performance), this is allowable by the as-if rule. As such, speculative execution is inhibited unless the code in question is proven to be
free of undefined behavior.

The presence of undefined behavior is modeled as part of julia's effect system using the `:noub` effect bit. See the documentation for [`Base.@assume_effects`](@ref) for more information on querying the compiler's effect model or overriding it for specific situations (e.g. where a dynamic check precludes potential UB from ever actually being reached).

## List of sources of undefined behavior

The following is a list of sources of undefined behavior,
though it should currently not be considered exhaustive:

- Replacement of `const` values. Note that in interactive mode the compiler will issue a warning for this and some care is taken to minimize the impact as a user convenience, but the behavior is not defined.
- Various modifications of global state during precompile. Where possible, this is detected and yields an error, but detection is incomplete.
- Incorrect implementation of a `Core.OptimizedGenerics` interface [1]
- Any invocation of undefined behavior in FFI code (e.g. `ccall`, `llvmcall`) according to the semantics of the respective language
- Incorrect signature types in `ccall` or `cfunction`, even if those signatures happen to yield the correct ABI on a particular platform
- Incorrect use of annotations like `@inbounds`, `@assume_effects` in violation of their requirements [^1]
- Retention of pointers to GC-tracked objects outside of a `@GC.preserve` region
- Memory modification of GC-tracked objects without appropriate write barriers from outside of julia (e.g. in native calls, debuggers, etc.)
- Violations of the memory model using `unsafe_` operations (e.g. `unsafe_load` of an invalid pointer, pointer provenance violations, etc)
- Violations of TBAA guarantees (e.g. using `unsafe_wrap`)
- Mutation of data promised to be immutable (e.g. in `Base.StringVector`)
- Data races
- Modification of julia-internal mutable state (e.g. task schedulers, data types, etc.)
- A value other than `false` (`reinterpret(UInt8, b) == 0x00`) or `true` (`reinterpret(UInt8, b) == 0x01`) for a `Bool` `b`.
- Invoking undefined behavior via compiler intrinsics.

[^1] Incorrect use here may be UB, even if not semantically executed, please see the specific documentation of the feature.

## Implementation-defined behavior
Some behavior is technically forbidden by the semantics of the language, but required in certain parts of the implementation and thus allowed as long as implementation-defined constraints are obeyed. Nevertheless, these constructs should be avoided when possible, as the implementation-defined constraints may not be stable across julia versions.

- Construction of objects using `eval(Expr(:new))` rather than their constructors

## Special cases explicitly NOT undefined behavior

- As of Julia 1.11, access to undefined bits types is no longer undefined behavior. It is still allowed to return an arbitrary value of the bits type, but the value returned must be the same for every access and use thereof is not undefined behavior. In LLVM terminology, the value is `freeze undef`, not `undef`.

- As of Julia 1.12, loops that do not make forward progress are no longer considered undefined behavior.

- Signed integer overflow is not undefined behavior. See also the manual section on [Overflow Behavior](https://docs.julialang.org/en/v1/manual/integers-and-floating-point-numbers/#Overflow-behavior).

- Revival of objects inside finalizers is permitted though discouraged.
