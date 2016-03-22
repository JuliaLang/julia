# This file is a part of Julia. License is MIT: http://julialang.org/license

using Core.Intrinsics: llvmcall

import Base: setindex!, getindex, unsafe_convert

export
    Atomic,
    atomic_cas!,
    atomic_xchg!,
    atomic_add!, atomic_sub!,
    atomic_and!, atomic_nand!, atomic_or!, atomic_xor!,
    atomic_max!, atomic_min!,
    atomic_fence

atomicintsmap = Dict(Int8   => "i8",   UInt8   => "i8",
                     Int16  => "i16",  UInt16  => "i16",
                     Int32  => "i32",  UInt32  => "i32",
                     Int64  => "i64",  UInt64  => "i64",
                     Int128 => "i128", UInt128 => "i128")
AtomicInts = Union{keys(atomicintsmap)...}

type Atomic{T<:AtomicInts}
    value::T
    Atomic() = new(zero(T))
    Atomic(value) = new(value)
end

Atomic() = Atomic{Int}()

unsafe_convert{T}(::Type{Ptr{T}}, x::Atomic{T}) = convert(Ptr{T}, pointer_from_objref(x))
setindex!{T}(x::Atomic{T}, v) = setindex!(x, convert(T, v))

# All atomic operations have acquire and/or release semantics, depending on
# whether the load or store values. Most of the time, this is what one wants
# anyway, and it's only moderately expensive on most hardware.
for (typ, lt) in atomicintsmap
    rt = VersionNumber(Base.libllvm_version) >= v"3.6" ? "$lt, $lt*" : "$lt*"
    @eval getindex(x::Atomic{$typ}) =
        llvmcall($"""
                %rv = load atomic $rt %0 acquire, align $WORD_SIZE
                ret $lt %rv
            """, $typ, Tuple{Ptr{$typ}}, unsafe_convert(Ptr{$typ}, x))
    @eval setindex!(x::Atomic{$typ}, v::$typ) =
        llvmcall($"""
                store atomic $lt %1, $lt* %0 release, align $WORD_SIZE
                ret void
            """, Void, Tuple{Ptr{$typ},$typ}, unsafe_convert(Ptr{$typ}, x), v)
    if VersionNumber(Base.libllvm_version) >= v"3.5"
        @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
            llvmcall($"""
                     %rv = cmpxchg $lt* %0, $lt %1, $lt %2 acq_rel monotonic
                     %bv = extractvalue { $lt, i1 } %rv, 1
                     ret i1 %bv
                     """, Bool, Tuple{Ptr{$typ},$typ,$typ},
                     unsafe_convert(Ptr{$typ}, x), cmp, new)
    else
        @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
            llvmcall($"""
                     %rv = cmpxchg $lt* %0, $lt %1, $lt %2 acq_rel
                     %bv = icmp eq $lt %1, %rv
                     ret i1 %bv
                     """, Bool, Tuple{Ptr{$typ},$typ,$typ},
                     unsafe_convert(Ptr{$typ}, x), cmp, new)
    end
    for rmwop in [:xchg, :add, :sub, :and, :nand, :or, :xor, :max, :min]
        rmw = string(rmwop)
        fn = symbol("atomic_", rmw, "!")
        if (rmw == "max" || rmw == "min") && typ <: Unsigned
            # LLVM distinguishes signedness in the operation, not the integer type.
            rmw = "u" * rmw
        end
        @eval $fn(x::Atomic{$typ}, v::$typ) =
            llvmcall($"""
                    %rv = atomicrmw $rmw $lt* %0, $lt %1 acq_rel
                    ret $lt %rv
                """, $typ, Tuple{Ptr{$typ}, $typ}, unsafe_convert(Ptr{$typ}, x), v)
    end
end

# Use sequential consistency for a memory fence. There are algorithms where this
# is needed (where an acquire/release ordering is insufficient). This is likely
# a very expensive operation. Given that all other atomic operations have
# already acquire/release semantics, explicit fences should not be necessary in
# most cases.
atomic_fence() = llvmcall("""
                          fence seq_cst
                          ret void
                          """, Void, Tuple{})
