# SIMD Support

Type `VecElement{T}` is intended for building libraries of SIMD operations. The type is defined as:

```julia
struct VecElement{T}
    value::T
end
```

It has a special compilation rule: a homogeneous tuple of `VecElement{T}` maps to an LLVM `vector`
type when `T` is a primitive bits type.

At `-O3`, the compiler *might* automatically vectorize operations on such tuples. For example,
the following program, when compiled with `julia -O3` generates two SIMD addition instructions
(`addps`) on x86 systems:

```julia
const m128 = NTuple{4,VecElement{Float32}}

function add(a::m128, b::m128)
    (VecElement(a[1].value+b[1].value),
     VecElement(a[2].value+b[2].value),
     VecElement(a[3].value+b[3].value),
     VecElement(a[4].value+b[4].value))
end

triple(c::m128) = add(add(c,c),c)

code_native(triple,(m128,))
```

However, since the automatic vectorization cannot be relied upon, libraries should instead
use the elementwise intrinsics in `Core.Intrinsics` (such as `add_int`, `mul_float`, `slt_int`,
`sext_int` or `bitcast`), which accept such vectors and compile to LLVM vector instructions:

```julia
add(a::m128, b::m128) = Core.Intrinsics.add_float(a, b)
```

These intrinsics act lane-wise, so all vector arguments must have the same number of lanes.
Comparisons return a vector of `Bool` (an `NTuple{N,VecElement{Bool}}`), conversions such as
`sext_int` take a vector target type with the same number of lanes, and `bitcast` requires only
that the total size in bits matches. The checked arithmetic intrinsics do not support vectors.
Operations not covered by the intrinsics (for example, shuffles) still require `llvmcall`.
