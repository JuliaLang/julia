# [Core.Builtins](@id lib-builtins)

The following builtin functions are considered unstable, but provide the basic
definitions for what defines the abilities and behaviors of a Julia
program. They are typically accessed through a higher level generic API.

## Raw access to memory

```@docs
Core.Intrinsics.pointerref
Core.Intrinsics.pointerset
Core.Intrinsics.atomic_pointerref
Core.Intrinsics.atomic_pointerset
Core.Intrinsics.atomic_pointerswap
Core.Intrinsics.atomic_pointermodify
Core.Intrinsics.atomic_pointerreplace
```

## Managed memory

```@docs
Core.memorynew
Core.memoryrefnew
Core.memoryrefoffset
Core.memoryrefget
Core.memoryrefset!
Core.memoryref_isassigned
Core.memoryrefswap!
Core.memoryrefmodify!
Core.memoryrefreplace!
Core.memoryrefsetonce!
```

## Module bindings

Core.get_binding_type

## Other

```@docs
Core.IntrinsicFunction
Core.Intrinsics
Core.IR
```
