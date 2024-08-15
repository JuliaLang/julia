# [Core.Builtins](@id lib-builtins)

## Builtin Function APIs

The following Builtin function APIs are considered unstable, but provide the basic
definitions for what defines the abilities and behaviors of a Julia program. They are
typically accessed through a higher level generic API.

```@docs
Core.memoryrefnew
Core.memoryrefoffset
Core.memoryrefget
Core.memoryrefset!
Core.memoryref_isassigned
Core.memoryrefswap!
Core.memoryrefmodify!
Core.memoryrefreplace!
Core.memoryrefsetonce!
Core.Intrinsics.atomic_pointerref
Core.Intrinsics.atomic_pointerset
Core.Intrinsics.atomic_pointerswap
Core.Intrinsics.atomic_pointermodify
Core.Intrinsics.atomic_pointerreplace
Core.get_binding_type
Core.IntrinsicFunction
Core.Intrinsics
Core.IR
```
