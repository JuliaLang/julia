## hashing a single value ##

hash(x::Any) = hash(x, zero(UInt))
hash(w::WeakRef, h::UInt) = hash(w.value, h)

## hashing general objects ##

hash(x::ANY, h::UInt) = hash(object_id(x), h)

## core data hashing functions ##

function hash_64_64(n::UInt64)
    local a::UInt64 = n
    a = ~a + a << 21
    a =  a $ a >> 24
    a =  a + a << 3 + a << 8
    a =  a $ a >> 14
    a =  a + a << 2 + a << 4
    a =  a $ a >> 28
    a =  a + a << 31
    return a
end

function hash_64_32(n::UInt64)
    local a::UInt64 = n
    a = ~a + a << 18
    a =  a $ a >> 31
    a =  a * 21
    a =  a $ a >> 11
    a =  a + a << 6
    a =  a $ a >> 22
    return a % UInt32
end

function hash_32_32(n::UInt32)
    local a::UInt32 = n
    a = a + 0x7ed55d16 + a << 12
    a = a $ 0xc761c23c $ a >> 19
    a = a + 0x165667b1 + a << 5
    a = a + 0xd3a2646c $ a << 9
    a = a + 0xfd7046c5 + a << 3
    a = a $ 0xb55a4f09 $ a >> 16
    return a
end

if UInt === UInt64
    hash_uint64(x::UInt64) = hash_64_64(x)
    hash_uint(x::UInt)     = hash_64_64(x)
else
    hash_uint64(x::UInt64) = hash_64_32(x)
    hash_uint(x::UInt)     = hash_32_32(x)
end

## hashing small, built-in numeric types ##

hx(a::UInt64, b::Float64, h::UInt) = hash_uint64((3a + reinterpret(UInt64,b)) - h)
const hx_NaN = hx(uint64(0), NaN, uint(0  ))

hash(x::UInt64,  h::UInt) = hx(x, float64(x), h)
hash(x::Int64,   h::UInt) = hx(reinterpret(UInt64,abs(x)), float64(x), h)
hash(x::Float64, h::UInt) = isnan(x) ? (hx_NaN $ h) : hx(box(UInt64,fptoui(unbox(Float64,abs(x)))), x, h)

hash(x::Union(Bool,Char,Int8,UInt8,Int16,UInt16,Int32,UInt32), h::UInt) = hash(int64(x), h)
hash(x::Float32, h::UInt) = hash(float64(x), h)

## hashing complex numbers ##

if UInt === UInt64
    const h_imag = 0x32a7a07f3e7cd1f9
else
    const h_imag = 0x3e7cd1f9
end
const hash_0_imag = hash(0, h_imag)

function hash(z::Complex, h::UInt)
    # TODO: with default argument specialization, this would be better:
    # hash(real(z), h $ hash(imag(z), h $ h_imag) $ hash(0, h $ h_imag))
    hash(real(z), h $ hash(imag(z), h_imag) $ hash_0_imag)
end

## symbol & expression hashing ##

hash(x::Symbol, h::UInt) = hash(object_id(x), h)
if UInt === UInt64
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
else
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x96d26dc6))
end
