# commented-out definitions are implemented in C

#type Any <: Any
#type Type{T}

#type ...{T}
#Tuple = (Any...)

#struct Symbol
#    #opaque
#end

#struct TypeName
#    name::Symbol
#end

print(tn::TypeName) = print(tn.name)

#struct TagKind <: Type
#    name::TypeName
#    super::Type
#    parameters::Tuple
#end

#struct StructKind <: TagKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#    names::Tuple
#    types::Tuple
#end

#struct BitsKind <: TagKind
#    #name::TypeName
#    #super::Type
#    #parameters::Tuple
#end

#struct FuncKind <: Type
#    from::Type
#    to::Type
#end

#struct UnionKind <: Type
#    types::Tuple
#end

#Bottom = Union()

#struct TypeVar
#    name::Symbol
#    lb::Type
#    ub::Type
#end

#struct TypeConstructor
#    parameters::Tuple
#    body
#end

#type Tensor{T, N}

#struct Array{T, N} <: Tensor{T, N}
#    dims::NTuple{N, Int32}
#end

#struct Expr
#    head::Symbol
#    args::Array{Any,1}
#    type::Any
#end

#struct LambdaStaticData
#    ast::Expr
#    sparams::Tuple
#    tfunc
#    name::Symbol
#end

#bitstype {32|64} Ptr{T}

type String

typealias Scalar{T} Tensor{T, 0}

type Number{T} <: Scalar{T}
type Real{T} <: Number{T}
type Int{T} <: Real{T}
type Float{T} <: Real{T}

bitstype 8  Bool <: Scalar{Bool}
bitstype 32 Char <: Scalar{Char}

bitstype 8  Int8 <: Int{Int8}
bitstype 8  Uint8 <: Int{Uint8}
bitstype 16 Int16 <: Int{Int16}
bitstype 16 Uint16 <: Int{Uint16}
bitstype 32 Int32 <: Int{Int32}
bitstype 32 Uint32 <: Int{Uint32}
bitstype 64 Int64 <: Int{Int64}
bitstype 64 Uint64 <: Int{Uint64}

bitstype 32 Float32 <: Float{Float32}
bitstype 64 Float64 <: Float{Float64}

struct ArrayString <: String
    data::Array{Uint8, 1}
end
