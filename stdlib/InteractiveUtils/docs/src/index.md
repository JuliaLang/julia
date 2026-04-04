```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/InteractiveUtils/docs/src/index.md"
```

# [Interactive Utilities](@id man-interactive-utils)

The `InteractiveUtils` module provides utilities for interactive use of Julia,
such as code introspection and clipboard access.
It is intended for interactive work and is loaded automatically in [interactive mode](@ref command-line-interface).

```@docs
InteractiveUtils.apropos
InteractiveUtils.varinfo
InteractiveUtils.versioninfo
InteractiveUtils.methodswith
InteractiveUtils.subtypes
InteractiveUtils.supertypes
InteractiveUtils.edit(::AbstractString, ::Integer)
InteractiveUtils.edit(::Any)
InteractiveUtils.@edit
InteractiveUtils.define_editor
InteractiveUtils.less(::AbstractString)
InteractiveUtils.less(::Any)
InteractiveUtils.@less
InteractiveUtils.@which
InteractiveUtils.@functionloc
InteractiveUtils.@code_lowered
InteractiveUtils.@code_typed
InteractiveUtils.code_warntype
InteractiveUtils.@code_warntype
InteractiveUtils.code_llvm
InteractiveUtils.@code_llvm
InteractiveUtils.code_native
InteractiveUtils.@code_native
InteractiveUtils.@time_imports
InteractiveUtils.@trace_compile
InteractiveUtils.@trace_dispatch
InteractiveUtils.clipboard
```
