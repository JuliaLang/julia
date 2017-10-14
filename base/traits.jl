# This file is a part of Julia. License is MIT: https://julialang.org/license

## numeric/object traits
# trait for objects that have an ordering
abstract type TypeOrder end
struct HasOrder <: TypeOrder end
struct Unordered <: TypeOrder end

TypeOrder(instance) = TypeOrder(typeof(instance))
TypeOrder(::Type{<:Real}) = HasOrder()
TypeOrder(::Type{<:Any}) = Unordered()

# trait for objects that support arithmetic
abstract type TypeArithmetic end
struct ArithmeticRounds <: TypeArithmetic end     # least significant bits can be lost
struct ArithmeticOverflows <: TypeArithmetic end  #  most significant bits can be lost
struct ArithmeticUnknown <: TypeArithmetic end

TypeArithmetic(instance) = TypeArithmetic(typeof(instance))
TypeArithmetic(::Type{<:AbstractFloat}) = ArithmeticRounds()
TypeArithmetic(::Type{<:Integer}) = ArithmeticOverflows()
TypeArithmetic(::Type{<:Any}) = ArithmeticUnknown()
