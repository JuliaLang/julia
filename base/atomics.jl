# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core.Intrinsics: llvmcall

import Base: setindex!, getindex, unsafe_convert
import Base.Sys: ARCH, WORD_SIZE

export
    Atomic,
    atomic_cas!,
    atomic_xchg!,
    atomic_add!, atomic_sub!,
    atomic_and!, atomic_nand!, atomic_or!, atomic_xor!,
    atomic_max!, atomic_min!,
    atomic_fence

# Disable 128-bit types on 32-bit Intel sytems due to LLVM problems;
# see <https://github.com/JuliaLang/julia/issues/14818> (fixed on LLVM 3.9)
# 128-bit atomics do not exist on AArch32.
if (VersionNumber(Base.libllvm_version) < v"3.9-" && ARCH === :i686) ||
        startswith(string(ARCH), "arm")
    const inttypes = (Int8, Int16, Int32, Int64,
                      UInt8, UInt16, UInt32, UInt64)
else
    const inttypes = (Int8, Int16, Int32, Int64, Int128,
                      UInt8, UInt16, UInt32, UInt64, UInt128)
end
const floattypes = (Float16, Float32, Float64)
# TODO: Support Bool, Ptr
const atomictypes = (inttypes..., floattypes...)
const IntTypes = Union{inttypes...}
const FloatTypes = Union{floattypes...}
const AtomicTypes = Union{atomictypes...}

"""
    Threads.Atomic{T}

Holds a reference to an object of type `T`, ensuring that it is only
accessed atomically, i.e. in a thread-safe manner.

Only certain "simple" types can be used atomically, namely the
primitive integer and float-point types. These are `Int8`...`Int128`,
`UInt8`...`UInt128`, and `Float16`...`Float64`.

New atomic objects can be created from a non-atomic values; if none is
specified, the atomic object is initialized with zero.

Atomic objects can be accessed using the `[]` notation:

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> x[] = 1
1

julia> x[]
1
```

Atomic operations use an `atomic_` prefix, such as `atomic_add!`,
`atomic_xchg!`, etc.
"""
mutable struct Atomic{T<:AtomicTypes}
    value::T
    Atomic{T}() where {T<:AtomicTypes} = new(zero(T))
    Atomic{T}(value) where {T<:AtomicTypes} = new(value)
end

Atomic() = Atomic{Int}()

"""
    Threads.atomic_cas!{T}(x::Atomic{T}, cmp::T, newval::T)

Atomically compare-and-set `x`

Atomically compares the value in `x` with `cmp`. If equal, write
`newval` to `x`. Otherwise, leaves `x` unmodified. Returns the old
value in `x`. By comparing the returned value to `cmp` (via `===`) one
knows whether `x` was modified and now holds the new value `newval`.

For further details, see LLVM's `cmpxchg` instruction.

This function can be used to implement transactional semantics. Before
the transaction, one records the value in `x`. After the transaction,
the new value is stored only if `x` has not been modified in the mean
time.

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_cas!(x, 4, 2);

julia> x
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_cas!(x, 3, 2);

julia> x
Base.Threads.Atomic{Int64}(2)
```
"""
function atomic_cas! end

"""
    Threads.atomic_xchg!{T}(x::Atomic{T}, newval::T)

Atomically exchange the value in `x`

Atomically exchanges the value in `x` with `newval`. Returns the **old**
value.

For further details, see LLVM's `atomicrmw xchg` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_xchg!(x, 2)
3

julia> x[]
2
```
"""
function atomic_xchg! end

"""
    Threads.atomic_add!{T}(x::Atomic{T}, val::T)

Atomically add `val` to `x`

Performs `x[] += val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw add` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_add!(x, 2)
3

julia> x[]
5
```
"""
function atomic_add! end

"""
    Threads.atomic_sub!{T}(x::Atomic{T}, val::T)

Atomically subtract `val` from `x`

Performs `x[] -= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw sub` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_sub!(x, 2)
3

julia> x[]
1
```
"""
function atomic_sub! end

"""
    Threads.atomic_and!{T}(x::Atomic{T}, val::T)

Atomically bitwise-and `x` with `val`

Performs `x[] &= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw and` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_and!(x, 2)
3

julia> x[]
2
```
"""
function atomic_and! end

"""
    Threads.atomic_nand!{T}(x::Atomic{T}, val::T)

Atomically bitwise-nand (not-and) `x` with `val`

Performs `x[] = ~(x[] & val)` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw nand` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_nand!(x, 2)
3

julia> x[]
-3
```
"""
function atomic_nand! end

"""
    Threads.atomic_or!{T}(x::Atomic{T}, val::T)

Atomically bitwise-or `x` with `val`

Performs `x[] |= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw or` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(5)
Base.Threads.Atomic{Int64}(5)

julia> Threads.atomic_or!(x, 7)
5

julia> x[]
7
```
"""
function atomic_or! end

"""
    Threads.atomic_xor!{T}(x::Atomic{T}, val::T)

Atomically bitwise-xor (exclusive-or) `x` with `val`

Performs `x[] \$= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw xor` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(5)
Base.Threads.Atomic{Int64}(5)

julia> Threads.atomic_xor!(x, 7)
5

julia> x[]
2
```
"""
function atomic_xor! end

"""
    Threads.atomic_max!{T}(x::Atomic{T}, val::T)

Atomically store the maximum of `x` and `val` in `x`

Performs `x[] = max(x[], val)` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw max` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(5)
Base.Threads.Atomic{Int64}(5)

julia> Threads.atomic_max!(x, 7)
5

julia> x[]
7
```
"""
function atomic_max! end

"""
    Threads.atomic_min!{T}(x::Atomic{T}, val::T)

Atomically store the minimum of `x` and `val` in `x`

Performs `x[] = min(x[], val)` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw min` instruction.

```jldoctest
julia> x = Threads.Atomic{Int}(7)
Base.Threads.Atomic{Int64}(7)

julia> Threads.atomic_min!(x, 5)
7

julia> x[]
5
```
"""
function atomic_min! end

unsafe_convert(::Type{Ptr{T}}, x::Atomic{T}) where {T} = convert(Ptr{T}, pointer_from_objref(x))
setindex!(x::Atomic{T}, v) where {T} = setindex!(x, convert(T, v))

const llvmtypes = Dict(
    Bool => "i1",
    Int8 => "i8", UInt8 => "i8",
    Int16 => "i16", UInt16 => "i16",
    Int32 => "i32", UInt32 => "i32",
    Int64 => "i64", UInt64 => "i64",
    Int128 => "i128", UInt128 => "i128",
    Float16 => "i16", # half
    Float32 => "float",
    Float64 => "double",
)
inttype(::Type{T}) where {T<:Integer} = T
inttype(::Type{Float16}) = Int16
inttype(::Type{Float32}) = Int32
inttype(::Type{Float64}) = Int64


alignment(::Type{T}) where {T} = ccall(:jl_alignment, Cint, (Csize_t,), sizeof(T))

# All atomic operations have acquire and/or release semantics, depending on
# whether the load or store values. Most of the time, this is what one wants
# anyway, and it's only moderately expensive on most hardware.
for typ in atomictypes
    lt = llvmtypes[typ]
    ilt = llvmtypes[inttype(typ)]
    rt = VersionNumber(Base.libllvm_version) >= v"3.6" ? "$lt, $lt*" : "$lt*"
    irt = VersionNumber(Base.libllvm_version) >= v"3.6" ? "$ilt, $ilt*" : "$ilt*"
    if VersionNumber(Base.libllvm_version) >= v"3.8"
        @eval getindex(x::Atomic{$typ}) =
            llvmcall($"""
                     %rv = load atomic $rt %0 acquire, align $(alignment(typ))
                     ret $lt %rv
                     """, $typ, Tuple{Ptr{$typ}}, unsafe_convert(Ptr{$typ}, x))
        @eval setindex!(x::Atomic{$typ}, v::$typ) =
            llvmcall($"""
                     store atomic $lt %1, $lt* %0 release, align $(alignment(typ))
                     ret void
                     """, Void, Tuple{Ptr{$typ},$typ}, unsafe_convert(Ptr{$typ}, x), v)
    else
        if typ <: Integer
            @eval getindex(x::Atomic{$typ}) =
                llvmcall($"""
                         %rv = load atomic $rt %0 acquire, align $(alignment(typ))
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ}}, unsafe_convert(Ptr{$typ}, x))
            @eval setindex!(x::Atomic{$typ}, v::$typ) =
                llvmcall($"""
                         store atomic $lt %1, $lt* %0 release, align $(alignment(typ))
                         ret void
                         """, Void, Tuple{Ptr{$typ},$typ}, unsafe_convert(Ptr{$typ}, x), v)
        else
            @eval getindex(x::Atomic{$typ}) =
                llvmcall($"""
                         %iptr = bitcast $lt* %0 to $ilt*
                         %irv = load atomic $irt %iptr acquire, align $(alignment(typ))
                         %rv = bitcast $ilt %irv to $lt
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ}}, unsafe_convert(Ptr{$typ}, x))
            @eval setindex!(x::Atomic{$typ}, v::$typ) =
                llvmcall($"""
                         %iptr = bitcast $lt* %0 to $ilt*
                         %ival = bitcast $lt %1 to $ilt
                         store atomic $ilt %ival, $ilt* %iptr release, align $(alignment(typ))
                         ret void
                         """, Void, Tuple{Ptr{$typ},$typ}, unsafe_convert(Ptr{$typ}, x), v)
        end
    end
    # Note: atomic_cas! succeeded (i.e. it stored "new") if and only if the result is "cmp"
    if VersionNumber(Base.libllvm_version) >= v"3.5"
        if typ <: Integer
            @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
                llvmcall($"""
                         %rs = cmpxchg $lt* %0, $lt %1, $lt %2 acq_rel acquire
                         %rv = extractvalue { $lt, i1 } %rs, 0
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ},$typ,$typ},
                         unsafe_convert(Ptr{$typ}, x), cmp, new)
        else
            @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
                llvmcall($"""
                         %iptr = bitcast $lt* %0 to $ilt*
                         %icmp = bitcast $lt %1 to $ilt
                         %inew = bitcast $lt %2 to $ilt
                         %irs = cmpxchg $ilt* %iptr, $ilt %icmp, $ilt %inew acq_rel acquire
                         %irv = extractvalue { $ilt, i1 } %irs, 0
                         %rv = bitcast $ilt %irv to $lt
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ},$typ,$typ},
                         unsafe_convert(Ptr{$typ}, x), cmp, new)
        end
    else
        if typ <: Integer
            @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
                llvmcall($"""
                         %rv = cmpxchg $lt* %0, $lt %1, $lt %2 acq_rel
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ},$typ,$typ},
                         unsafe_convert(Ptr{$typ}, x), cmp, new)
        else
            @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
                llvmcall($"""
                         %iptr = bitcast $lt* %0 to $ilt*
                         %icmp = bitcast $lt %1 to $ilt
                         %inew = bitcast $lt %2 to $ilt
                         %irv = cmpxchg $ilt* %iptr, $ilt %icmp, $ilt %inew acq_rel
                         %rv = bitcast $ilt %irv to $lt
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ},$typ,$typ},
                         unsafe_convert(Ptr{$typ}, x), cmp, new)
        end
    end
    for rmwop in [:xchg, :add, :sub, :and, :nand, :or, :xor, :max, :min]
        rmw = string(rmwop)
        fn = Symbol("atomic_", rmw, "!")
        if (rmw == "max" || rmw == "min") && typ <: Unsigned
            # LLVM distinguishes signedness in the operation, not the integer type.
            rmw = "u" * rmw
        end
        if typ <: Integer
            @eval $fn(x::Atomic{$typ}, v::$typ) =
                llvmcall($"""
                         %rv = atomicrmw $rmw $lt* %0, $lt %1 acq_rel
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ}, $typ}, unsafe_convert(Ptr{$typ}, x), v)
        else
            rmwop == :xchg || continue
            @eval $fn(x::Atomic{$typ}, v::$typ) =
                llvmcall($"""
                         %iptr = bitcast $lt* %0 to $ilt*
                         %ival = bitcast $lt %1 to $ilt
                         %irv = atomicrmw $rmw $ilt* %iptr, $ilt %ival acq_rel
                         %rv = bitcast $ilt %irv to $lt
                         ret $lt %rv
                         """, $typ, Tuple{Ptr{$typ}, $typ}, unsafe_convert(Ptr{$typ}, x), v)
        end
    end
end

# Provide atomic floating-point operations via atomic_cas!
const opnames = Dict{Symbol, Symbol}(:+ => :add, :- => :sub)
for op in [:+, :-, :max, :min]
    opname = get(opnames, op, op)
    @eval function $(Symbol("atomic_", opname, "!"))(var::Atomic{T}, val::T) where T<:FloatTypes
        IT = inttype(T)
        old = var[]
        while true
            new = $op(old, val)
            cmp = old
            old = atomic_cas!(var, cmp, new)
            reinterpret(IT, old) == reinterpret(IT, cmp) && return new
            # Temporary solution before we have gc transition support in codegen.
            ccall(:jl_gc_safepoint, Void, ())
        end
    end
end

"""
    Threads.atomic_fence()

Insert a sequential-consistency memory fence

Inserts a memory fence with sequentially-consistent ordering
semantics. There are algorithms where this is needed, i.e. where an
acquire/release ordering is insufficient.

This is likely a very expensive operation. Given that all other atomic
operations in Julia already have acquire/release semantics, explicit
fences should not be necessary in most cases.

For further details, see LLVM's `fence` instruction.
"""
atomic_fence() = llvmcall("""
                          fence seq_cst
                          ret void
                          """, Void, Tuple{})
