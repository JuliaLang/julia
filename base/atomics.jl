# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Intrinsics: llvmcall

import Base: setindex!, getindex

export
    Atomic,
    atomic_cas!,
    atomic_xchg!,
    atomic_add!, atomic_sub!,
    atomic_and!, atomic_nand!, atomic_or!, atomic_xor!,
    atomic_max!, atomic_min!

type Atomic{T<:Integer}
    value::T
    Atomic() = new(zero(T))
    Atomic(value) = new(value)
end

Atomic() = Atomic{Int}()

atomicintsmap = Dict(Int8   => "i8",   UInt8   => "i8",
                     Int16  => "i16",  UInt16  => "i16",
                     Int32  => "i32",  UInt32  => "i32",
                     Int64  => "i64",  UInt64  => "i64",
                     Int128 => "i128", UInt128 => "i128")

unsafe_convert{T}(::Type{Ptr{T}}, x::Atomic{T}) = convert(Ptr{T}, pointer_from_objref(x))
setindex!{T}(x::Atomic{T}, v) = setindex!(x, convert(T, v))

for (typ, lt) in atomicintsmap
    rt = VersionNumber(Base.libllvm_version) >= v"3.6" ? "$lt, $lt*" : "$lt*"
    @eval getindex(x::Atomic{$typ}) =
        llvmcall($"""
                %rv = load atomic volatile $rt %0 monotonic, align $WORD_SIZE
                ret $lt %rv
            """, $typ, Tuple{Ptr{$typ}}, unsafe_convert(Ptr{$typ}, x))
    @eval setindex!(x::Atomic{$typ}, v::$typ) =
        llvmcall($"""
                store atomic volatile $lt %1, $lt* %0 monotonic, align $WORD_SIZE
                ret void
            """, Void, Tuple{Ptr{$typ},$typ}, unsafe_convert(Ptr{$typ}, x), v)
    @eval atomic_cas!(x::Atomic{$typ}, cmp::$typ, new::$typ) =
        llvmcall($"""
                %rv = cmpxchg $lt* %0, $lt %1, $lt %2 acq_rel monotonic
                %bv = extractvalue { $lt, i1 } %rv, 1
                ret i1 %bv
            """, Bool, Tuple{Ptr{$typ},$typ,$typ}, unsafe_convert(Ptr{$typ}, x), cmp, new)
    for rmwop in [:xchg, :add, :sub, :and, :nand, :or, :xor, :max, :min]
        rmw = string(rmwop)
        fn = symbol("atomic_", rmw, "!")
        if (rmw == "max" || rmw == "min") && super(typ) == Unsigned
            # LLVM distinguishes signedness in the operation, not the integer type.
            rmw = "u" * rmw
        end
        @eval $fn(x::Atomic{$typ}, v::$typ) =
            llvmcall($"""
                    %rv = atomicrmw volatile $rmw $lt* %0, $lt %1 acquire
                    ret $lt %rv
                """, $typ, Tuple{Ptr{$typ}, $typ}, unsafe_convert(Ptr{$typ}, x), v)
    end
end

