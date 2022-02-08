# [Profiling](@id lib-profiling)

## CPU Profiling

```@docs
Profile.@profile
```

The methods in `Profile` are not exported and need to be called e.g. as `Profile.print()`.

```@docs
Profile.clear
Profile.print
Profile.init
Profile.fetch
Profile.retrieve
Profile.callers
Profile.clear_malloc_data
```

## Memory profiling

Note: The current implementation of the Allocations Profiler _does not
capture types for all allocations._ Allocations for which the profiler
could not capture the type are represented as having type
`Profile.Allocs.UnknownType`.

You can read more about the missing types and the plan to improve this, here:
https://github.com/JuliaLang/julia/issues/43688.

```@docs
Profile.Allocs.@profile
```

The methods in `Profile.Allocs` are not exported and need to be called e.g. as `Profile.Allocs.fetch()`.

```@docs
Profile.Allocs.clear
Profile.Allocs.fetch
Profile.Allocs.start
Profile.Allocs.stop
```
