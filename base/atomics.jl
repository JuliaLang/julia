# Copyright (c) 2015, Intel Corporation
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Intel Corporation nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

using Base.Intrinsics: llvmcall

import Base: setindex!, getindex

export
    Atomic,
    atomic_cas!,
    atomic_xchg!,
    atomic_add!, atomic_sub!,
    atomic_and!, atomic_nand!, atomic_or!, atomic_xor!,
    atomic_max!, atomic_min!, atomic_umax!, atomic_umin!

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
    @eval getindex(x::Atomic{$typ}) =
        llvmcall($"""
                %rv = load atomic volatile $lt, $lt* %0 monotonic, align $WORD_SIZE
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
    for rmwop in [:xchg, :add, :sub, :and, :nand, :or, :xor, :max, :min, :umax, :umin]
        rmw = string(rmwop)
        fn = symbol("atomic_", rmw, "!")
        @eval $fn(x::Atomic{$typ}, v::$typ) =
            llvmcall($"""
                    %rv = atomicrmw volatile $rmw $lt* %0, $lt %1 acquire
                    ret $lt %rv
                """, $typ, Tuple{Ptr{$typ}, $typ}, unsafe_convert(Ptr{$typ}, x), v)
    end
end

