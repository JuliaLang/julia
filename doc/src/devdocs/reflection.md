# Reflection and introspection

Julia provides a variety of runtime reflection capabilities.

## Module bindings

The exported names for a `Module` are available using [`names(m::Module)`](@ref), which will return
an array of [`Symbol`](@ref) elements representing the exported bindings. `names(m::Module, true)`
returns symbols for all bindings in `m`, regardless of export status.

## DataType fields

The names of `DataType` fields may be interrogated using [`fieldnames()`](@ref). For example,
given the following type, `fieldnames(Point)` returns an arrays of [`Symbol`](@ref) elements representing
the field names:

```julia
julia> type Point
           x::Int
           y
       end

julia> fieldnames(Point)
2-element Array{Symbol,1}:
 :x
 :y
```

The type of each field in a `Point` object is stored in the `types` field of the `Point` variable
itself:

```julia
julia> Point.types
svec(Int64,Any)
```

While `x` is annotated as an `Int`, `y` was unannotated in the type definition, therefore `y`
defaults to the `Any` type.

Types are themselves represented as a structure called `DataType`:

```julia
julia> typeof(Point)
DataType
```

Note that `fieldnames(DataType)` gives the names for each field of `DataType` itself, and one
of these fields is the `types` field observed in the example above.

## Subtypes

The *direct* subtypes of any `DataType` may be listed using [`subtypes()`](@ref). For example,
the abstract `DataType``AbstractFloat` has four (concrete) subtypes:

```julia
julia> subtypes(AbstractFloat)
4-element Array{DataType,1}:
 BigFloat
 Float16
 Float32
 Float64
```

Any abstract subtype will also be included in this list, but further subtypes thereof will not;
recursive application of [`subtypes()`](@ref) may be used to inspect the full type tree.

## DataType layout

The internal representation of a `DataType` is critically important when interfacing with C code
and several functions are available to inspect these details. [`isbits(T::DataType)`](@ref) returns
true if `T` is stored with C-compatible alignment. [`fieldoffset(T::DataType, i::Integer)`](@ref)
returns the (byte) offset for field *i* relative to the start of the type.

## Function methods

The methods of any generic function may be listed using [`methods()`](@ref). The method dispatch
table may be searched for methods accepting a given type using [`methodswith()`](@ref).

## Expansion and lowering

As discussed in the [Metaprogramming](@ref) section, the [`macroexpand()`](@ref) function gives
the unquoted and interpolated expression (`Expr`) form for a given macro. To use `macroexpand`,
`quote` the expression block itself (otherwise, the macro will be evaluated and the result will
be passed instead!). For example:

```julia
julia> macroexpand( :(@edit println("")) )
:((Base.edit)(println,(Base.typesof)("")))
```

The functions `Base.Meta.show_sexpr()` and [`dump()`](@ref) are used to display S-expr style views
and depth-nested detail views for any expression.

Finally, the [`expand()`](@ref) function gives the `lowered` form of any expression and is of
particular interest for understanding both macros and top-level statements such as function declarations
and variable assignments:

```julia
julia> expand( :(f() = 1) )
:(begin
        $(Expr(:method, :f))
        $(Expr(:method, :f, :((Core.svec)((Core.svec)((Core.Typeof)(f)),(Core.svec)())), CodeInfo(:(begin  # none, line 1:
        return 1
    end)), false))
        return f
    end)
```

## Intermediate and compiled representations

Inspecting the lowered form for functions requires selection of the specific method to display,
because generic functions may have many methods with different type signatures. For this purpose,
method-specific code-lowering is available using [`code_lowered(f::Function, (Argtypes...))`](@ref),
and the type-inferred form is available using [`code_typed(f::Function, (Argtypes...))`](@ref).
[`code_warntype(f::Function, (Argtypes...))`](@ref) adds highlighting to the output of [`code_typed()`](@ref)
(see [`@code_warntype`](@ref)).

Closer to the machine, the LLVM intermediate representation of a function may be printed using
by [`code_llvm(f::Function, (Argtypes...))`](@ref), and finally the compiled machine code is available
using [`code_native(f::Function, (Argtypes...))`](@ref) (this will trigger JIT compilation/code
generation for any function which has not previously been called).

For convenience, there are macro versions of the above functions which take standard function
calls and expand argument types automatically:

```julia
julia> @code_llvm +(1,1)

; Function Attrs: sspreq
define i64 @"julia_+_130862"(i64, i64) #0 {
top:
    %2 = add i64 %1, %0, !dbg !8
    ret i64 %2, !dbg !8
}
```

(likewise `@code_typed`, `@code_warntype`, `@code_lowered`, and `@code_native`)
