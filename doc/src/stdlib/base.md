# Essentials

## Introduction

The Julia standard library contains a range of functions and macros appropriate for performing
scientific and numerical computing, but is also as broad as those of many general purpose programming
languages.  Additional functionality is available from a growing collection of available packages.
Functions are grouped by topic below.

Some general notes:

  * Except for functions in built-in modules (`Pkg`, `Collections`, `Test`
    and `Profile`), all functions documented here are directly available for use in programs.
  * To use module functions, use `import Module` to import the module, and `Module.fn(x)` to use the
    functions.
  * Alternatively, `using Module` will import all exported `Module` functions into the current namespace.
  * By convention, function names ending with an exclamation point (`!`) modify their arguments.
    Some functions have both modifying (e.g., `sort!`) and non-modifying (`sort`) versions.

## Getting Around

```@docs
Base.exit
Base.quit
Base.atexit
Base.atreplinit
Base.isinteractive
Base.whos
Base.summarysize
Base.edit(::AbstractString, ::Integer)
Base.edit(::Any)
Base.@edit
Base.less(::AbstractString)
Base.less(::Any)
Base.@less
Base.clipboard(::Any)
Base.clipboard()
Base.reload
Base.require
Base.compilecache
Base.__precompile__
Base.include
Base.include_string
Base.include_dependency
Base.Docs.apropos
Base.which(::Any, ::Any)
Base.which(::Symbol)
Base.@which
Base.methods
Base.methodswith
Base.@show
Base.versioninfo
Base.workspace
ans
```

## All Objects

```@docs
Core.:(===)
Core.isa
Base.isequal
Base.isless
Base.isless(::Nullable, ::Nullable)
Base.ifelse
Base.lexcmp
Base.lexless
Core.typeof
Core.tuple
Base.ntuple
Base.object_id
Base.hash
Base.finalizer
Base.finalize
Base.copy
Base.deepcopy
Core.isdefined
Base.@isdefined
Base.convert
Base.promote
Base.oftype
Base.widen
Base.identity
```

## Dealing with Types

```@docs
Base.supertype
Core.:(<:)
Base.:(>:)
Base.subtypes
Base.typemin
Base.typemax
Base.realmin
Base.realmax
Base.maxintfloat
Base.sizeof(::Type)
Base.eps(::Type{<:AbstractFloat})
Base.eps(::AbstractFloat)
Base.promote_type
Base.promote_rule
Core.getfield
Core.setfield!
Base.fieldoffset
Core.fieldtype
Base.isimmutable
Base.isbits
Base.isconcrete
Base.typejoin
Base.typeintersect
Base.instances
```

## Special Types

```@docs
Core.Void
Core.Any
Base.Enums.@enum
Core.Union
Union{}
Core.UnionAll
Core.Tuple
Base.Val
Core.Vararg
```

## Generic Functions

```@docs
Core.Function
Base.method_exists
Core.applicable
Core.invoke
Base.invokelatest
Base.:(|>)
Base.:(∘)
Base.equalto
```

## Syntax

```@docs
Core.eval
Base.@eval
Base.evalfile
Base.esc
Base.@inbounds
Base.@boundscheck
Base.@inline
Base.@noinline
Base.@nospecialize
Base.gensym
Base.@gensym
Base.@goto
Base.@label
Base.@polly
Base.parse(::AbstractString, ::Int)
Base.parse(::AbstractString)
```

## Nullables

```@docs
Base.Nullable
Base.get(::Nullable, ::Any)
Base.isnull
Base.unsafe_get
```

## System

```@docs
Base.run
Base.spawn
Base.DevNull
Base.success
Base.process_running
Base.process_exited
Base.kill(::Base.Process, ::Integer)
Base.Sys.set_process_title
Base.Sys.get_process_title
Base.readandwrite
Base.ignorestatus
Base.detach
Base.Cmd
Base.setenv
Base.withenv
Base.pipeline(::Any, ::Any, ::Any, ::Any...)
Base.pipeline(::Base.AbstractCmd)
Base.Libc.gethostname
Base.getipaddr
Base.Libc.getpid
Base.Libc.time()
Base.time_ns
Base.@time
Base.@timev
Base.@timed
Base.@elapsed
Base.@allocated
Base.EnvHash
Base.ENV
Base.Sys.isunix
Base.Sys.isapple
Base.Sys.islinux
Base.Sys.isbsd
Base.Sys.iswindows
Base.Sys.windows_version
Base.@static
```

## Errors

```@docs
Base.error
Core.throw
Base.rethrow
Base.backtrace
Base.catch_backtrace
Base.assert
Base.@assert
Base.ArgumentError
Base.AssertionError
Core.BoundsError
Base.DimensionMismatch
Core.DivideError
Core.DomainError
Base.EOFError
Core.ErrorException
Core.InexactError
Core.InterruptException
Base.KeyError
Base.LoadError
Base.MethodError
Base.NullException
Core.OutOfMemoryError
Core.ReadOnlyMemoryError
Core.OverflowError
Base.ParseError
Base.ProcessExitedException
Core.StackOverflowError
Base.SystemError
Core.TypeError
Core.UndefRefError
Core.UndefVarError
Base.InitError
Base.retry
Base.ExponentialBackOff
```

## Events

```@docs
Base.Timer(::Function, ::Real, ::Real)
Base.Timer
Base.AsyncCondition
Base.AsyncCondition(::Function)
```

## Reflection

```@docs
Base.module_name
Base.module_parent
Base.@__MODULE__
Base.fullname
Base.names
Core.nfields
Base.fieldnames
Base.fieldname
Base.fieldcount
Base.datatype_module
Base.datatype_name
Base.isconst
Base.function_name
Base.function_module(::Function)
Base.function_module(::Any, ::Any)
Base.functionloc(::Any, ::Any)
Base.functionloc(::Method)
Base.@functionloc
```

## Internals

```@docs
Base.gc
Base.gc_enable
Base.macroexpand
Base.@macroexpand
Base.@macroexpand1
Base.expand
Base.code_lowered
Base.@code_lowered
Base.code_typed
Base.@code_typed
Base.code_warntype
Base.@code_warntype
Base.code_llvm
Base.@code_llvm
Base.code_native
Base.@code_native
Base.precompile
```
