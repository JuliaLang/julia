## hashing a single value ##

hash(x::Any) = hash(x, zero(Uint))

## core data hashing functions ##

function hash_uint(n::Uint64)
    local a::Uint64 = n
    a = ~a + a << 21
    a =  a $ a >> 24
    a =  a + a << 3 + a << 8
    a =  a $ a >> 14
    a =  a + a << 2 + a << 4
    a =  a $ a >> 28
    a =  a + a << 31
    return a
end

function hash_uint(n::Uint32)
    local a::Uint32 = n
    a = a + 0x7ed55d16 + a << 12
    a = a $ 0xc761c23c $ a >> 19
    a = a + 0x165667b1 + a << 5
    a = a + 0xd3a2646c $ a << 9
    a = a + 0xfd7046c5 + a << 3
    a = a $ 0xb55a4f09 $ a >> 16
    return a
end

## hashing small, built-in numeric types ##

hx(a::Uint64, b::Float64, h::Uint) = hash_uint((3a + reinterpret(Uint64,b)) - h)

hash(x::Uint64,  h::Uint) = hx(x, float64(x), h)
hash(x::Int64,   h::Uint) = hx(reinterpret(Uint64,x), float64(x), h)
hash(x::Float64, h::Uint) = hx(box(Uint64,fptosi(unbox(Float64,x))), ifelse(isnan(x), NaN, x), h)

hash(x::Union(Int8,Uint8,Int16,Uint16,Int32,Uint32), h::Uint) = hash(int64(x), h)
hash(x::Float32, h::Uint) = hash(float64(x), h)

## hashing complex numbers ##

const h_imag = 0x32a7a07f3e7cd1f9
const hash_0_imag = hash(0, h_imag)

function hash(z::Complex, h::Uint)
    # TODO: with default argument specialization, this would be better:
    # hash(real(z), h $ hash(imag(z), h $ h_imag) $ hash(0, h $ h_imag))
    hash(real(z), h $ hash(imag(z), h_imag) $ hash_0_imag)
end

## special hashing for booleans and characters ##

hash(x::Bool, h::Uint) = hash(int(x), h + 0x4cd135a1755139a5)
hash(x::Char, h::Uint) = hash(int(x), h + 0x10f989ff0f886f11)

## symbol & expression hashing ##

hash(x::Symbol, h::Uint) = hash(object_id(x), h)
hash(x::Expr, h::Uint) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
