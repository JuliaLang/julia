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
Base.atexit
Base.isinteractive
Base.summarysize
Base.require
Base.compilecache
Base.__precompile__
Base.include
Base.MainInclude.include
Base.include_string
Base.include_dependency
Base.which(::Any, ::Any)
Base.methods
Base.@show
ans
```

## Keywords

This is the list of reserved keywords in Julia:
`baremodule`, `begin`, `break`, `catch`, `const`, `continue`, `do`,
`else`, `elseif`, `end`, `export`, `false`, `finally`, `for`, `function`,
`global`, `if`, `import`, `let`, `local`, `macro`, `module`, `quote`,
`return`, `struct`, `true`, `try`, `using`, `while`.
Those keywords are not allowed to be used as variable names.

The following two-word sequences are reserved:
`abstract type`, `mutable struct`, `primitive type`.
However, you can create variables with names:
`abstract`, `mutable`, `primitive` and `type`.

Finally,`where` is parsed as an infix operator for writing parametric method
and type definitions. Also `in` and `isa` are parsed as infix operators.
Creation of a variable named `where`, `in` or `isa` is allowed though.

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
where
...
;
=
```

## Standard Modules
```@docs
Main
Core
Base
```

## Base Submodules
```@docs
Base.Broadcast
Base.Docs
Base.Iterators
Base.Libc
Base.Meta
Base.StackTraces
Base.Sys
Base.Threads
Base.GC
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
Base.hasproperty
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
Base.typejoin
Base.typeintersect
Base.promote_type
Base.promote_rule
Base.isdispatchtuple
```

### Declared structure

```@docs
Base.ismutable
Base.isimmutable
Base.isabstracttype
Base.isprimitivetype
Base.issingletontype
Base.isstructtype
Base.nameof(::DataType)
Base.fieldnames
Base.fieldname
Base.hasfield
```

### Memory layout

```@docs
Base.sizeof(::Type)
Base.isconcretetype
Base.isbits
Base.isbitstype
Core.fieldtype
Base.fieldtypes
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
Base.floatmin
Base.floatmax
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
Core.NamedTuple
Base.@NamedTuple
Base.Val
Core.Vararg
Core.Nothing
Base.isnothing
Base.Some
Base.something
Base.Enums.Enum
Base.Enums.@enum
Core.Expr
Core.Symbol
Core.Symbol(x...)
Core.Module
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
```

## Syntax

```@docs
Core.eval
Base.MainInclude.eval
Base.@eval
Base.evalfile
Base.esc
Base.@inbounds
Base.@boundscheck
Base.@propagate_inbounds
Base.@inline
Base.@noinline
Base.@nospecialize
Base.@specialize
Base.gensym
Base.@gensym
var"name"
Base.@goto
Base.@label
Base.@simd
Base.@polly
Base.@generated
Base.@pure
Base.@deprecate
```

## Missing Values
```@docs
Base.Missing
Base.missing
Base.coalesce
Base.ismissing
Base.skipmissing
Base.nonmissingtype
```

## System

```@docs
Base.run
Base.devnull
Base.success
Base.process_running
Base.process_exited
Base.kill(::Base.Process, ::Integer)
Base.Sys.set_process_title
Base.Sys.get_process_title
Base.ignorestatus
Base.detach
Base.Cmd
Base.setenv
Base.withenv
Base.pipeline(::Any, ::Any, ::Any, ::Any...)
Base.pipeline(::Base.AbstractCmd)
Base.Libc.gethostname
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
Base.Sys.isfreebsd
Base.Sys.isopenbsd
Base.Sys.isnetbsd
Base.Sys.isdragonfly
Base.Sys.iswindows
Base.Sys.windows_version
Base.Sys.free_memory
Base.Sys.total_memory
Base.@static
```

## Versioning

```@docs
Base.VersionNumber
Base.@v_str
```

## Errors

```@docs
Base.error
Core.throw
Base.rethrow
Base.backtrace
Base.catch_backtrace
Base.catch_stack
Base.@assert
Base.register_error_hint
Base.show_error_hints
Base.ArgumentError
Base.AssertionError
Core.BoundsError
Base.CompositeException
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
Base.ProcessFailedException
Core.StackOverflowError
Base.SystemError
Core.TypeError
Core.UndefKeywordError
Core.UndefRefError
Core.UndefVarError
Base.StringIndexError
Base.InitError
Base.retry
Base.ExponentialBackOff
```

## Events

```@docs
Base.Timer(::Function, ::Real)
Base.Timer
Base.AsyncCondition
Base.AsyncCondition(::Function)
```

## Reflection

```@docs
Base.nameof(::Module)
Base.parentmodule
Base.pathof(::Module)
Base.moduleroot
Base.@__MODULE__
Base.fullname
Base.names
Core.nfields
Base.isconst
Base.nameof(::Function)
Base.functionloc(::Any, ::Any)
Base.functionloc(::Method)
```

## Internals

```@docs
Base.GC.gc
Base.GC.enable
Base.GC.@preserve
Base.GC.safepoint
Meta.lower
Meta.@lower
Meta.parse(::AbstractString, ::Int)
Meta.parse(::AbstractString)
Meta.ParseError
Core.QuoteNode
Base.macroexpand
Base.@macroexpand
Base.@macroexpand1
Base.code_lowered
Base.code_typed
Base.precompile
```

## Meta
```@docs
Meta.quot
Meta.isexpr
Meta.show_sexpr
```
