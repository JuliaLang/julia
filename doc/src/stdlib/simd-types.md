# SIMD Support

Type `VecElement{T}` is intended for building libraries of SIMD operations. Practical use of it
requires using `llvmcall`. The type is defined as:

```julia
immutable VecElement{T}
    value::T
end
```

It has a special compilation rule: a homogeneous tuple of `VecElement{T}` maps to an LLVM `vector`
type when `T` is a bitstype and the tuple length is in the set {2-6,8-10,16}.

At `-O3`, the compiler *might* automatically vectorize operations on such tuples. For example,
the following program, when compiled with `julia -O3` generates two SIMD addition instructions
(`addps`) on x86 systems:

```julia
typealias m128 NTuple{4,VecElement{Float32}}

function add(a::m128, b::m128)
    (VecElement(a[1].value+b[1].value),
     VecElement(a[2].value+b[2].value),
     VecElement(a[3].value+b[3].value),
     VecElement(a[4].value+b[4].value))
end

triple(c::m128) = add(add(c,c),c)

code_native(triple,(m128,))
```

However, since the automatic vectorization cannot be relied upon, future use will mostly be via
libraries that use `llvmcall`.
