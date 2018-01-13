# Essentials

## Introduction

Julia Base contains a range of functions and macros appropriate for performing
scientific and numerical computing, but is also as broad as those of many general purpose programming
languages.  Additional functionality is available from a growing collection of available packages.
Functions are grouped by topic below.

Some general notes:

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
Base.isinteractive
Base.varinfo
Base.summarysize
Base.edit(::AbstractString, ::Integer)
Base.edit(::Any)
Base.@edit
Base.less(::AbstractString)
Base.less(::Any)
Base.@less
Base.clipboard(::Any)
Base.clipboard()
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
ans
```

## Keywords

```@docs
module
export
import
using
baremodule
function
macro
return
do
begin
end
let
if
for
while
break
continue
try
finally
quote
local
global
const
struct
mutable struct
abstract type
primitive type
...
;
```

## Base Modules
```@docs
Base.Docs
Base.Iterators
Base.LibGit2
Base.Libc
Base.Markdown
Base.Meta
Base.Pkg
Base.StackTraces
Base.Sys
Base.Threads
```

## All Objects

```@docs
Core.:(===)
Core.isa
Base.isequal
Base.isless
Base.ifelse
Core.typeassert
Core.typeof
Core.tuple
Base.ntuple
Base.objectid
Base.hash
Base.finalizer
Base.finalize
Base.copy
Base.deepcopy
Base.getproperty
Base.setproperty!
Base.propertynames
Core.getfield
Core.setfield!
Core.isdefined
Base.@isdefined
Base.convert
Base.promote
Base.oftype
Base.widen
Base.identity
```

## Properties of Types

### Type relations

```@docs
Base.supertype
Core.:(<:)
Base.:(>:)
Base.subtypes
Base.typejoin
Base.typeintersect
Base.promote_type
Base.promote_rule
Base.isdispatchtuple
```

### Declared structure

```@docs
Base.isimmutable
Base.isabstracttype
Base.isprimitivetype
Base.isstructtype
Base.datatype_name
Base.fieldnames
Base.fieldname
```

### Memory layout

```@docs
Base.sizeof(::Type)
Base.isconcretetype
Base.isbits
Core.fieldtype
Base.fieldcount
Base.fieldoffset
Base.datatype_alignment
Base.datatype_haspadding
Base.datatype_pointerfree
```

### Special values

```@docs
Base.typemin
Base.typemax
Base.realmin
Base.realmax
Base.maxintfloat
Base.eps(::Type{<:AbstractFloat})
Base.eps(::AbstractFloat)
Base.instances
```

## Special Types

```@docs
Core.Any
Core.Union
Union{}
Core.UnionAll
Core.Tuple
Base.Val
Core.Vararg
Core.Nothing
Base.Some
Base.Enums.@enum
```

## Generic Functions

```@docs
Core.Function
Base.hasmethod
Core.applicable
Core.invoke
Base.invokelatest
new
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
```

## Missing Values
```@docs
Base.Missing
Base.missing
Base.coalesce
Base.ismissing
Base.skipmissing
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
Base.EnvDict
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
Base.MissingException
Core.OutOfMemoryError
Core.ReadOnlyMemoryError
Core.OverflowError
Base.ParseError
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
Base.parentmodule
Base.@__MODULE__
Base.fullname
Base.names
Core.nfields
Base.isconst
Base.function_name
Base.functionloc(::Any, ::Any)
Base.functionloc(::Method)
Base.@functionloc
```

## Internals

```@docs
Base.GC.gc
Base.GC.enable
Meta.lower
Meta.@lower
Meta.parse(::AbstractString, ::Int)
Meta.parse(::AbstractString)
Base.macroexpand
Base.@macroexpand
Base.@macroexpand1
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
