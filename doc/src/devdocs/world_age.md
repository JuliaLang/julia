# World Age Semantics in Julia

## Global World Age

The global world age is a monotonically increasing counter that increments whenever a dispatch-impacting event occurs, such as:
  - `Method` registration (`Method` addition or deletion)
  - `Binding` modification (re-defining a function or constant)

A `world` represents a snapshot of the dispatch state at a given point in time.

## Localized World Ages

A "localized world age" is similar, but it only tracks changes to specific portions of the method table.

Each `Core.MethodTable` has a `local_age` field that counts dispatch-affecting events specific to that function. For example, a method registration for `read(::Foo)` affects the global world age, as well as the local age of `read`, but it would not be counted as part of the local age of `write`.

Similar to the global world age, Julia can use these ages to refer to a "snapshot" of portions of the method table and avoid unnecessary validation when loading packages. During precompilation, Julia records the local age of each MethodTable it queries. If the latest local age matches the pre-compiled "snapshot", the compiler can skip revalidation for that function, improving load times.

Loading guarantees that all of the dispatch-affecting events that happened during pre-compilation are applied to the dispatch table again at run-time, so that any modifications from other loaded packages (outside of our pre-compilation dependencies) would trigger a strictly larger local age.
