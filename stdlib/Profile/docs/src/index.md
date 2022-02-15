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
