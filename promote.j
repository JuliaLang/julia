## promotion mechanism ##

promote_type{T}(::Type{T}, ::Type{T}) = T

function promote_type{T,S}(::Type{T}, ::Type{S})
    # print("promote_type: ",T,", ",S,"\n")
    if method_exists(promote_rule,(T,S))
        return promote_rule(T,S)
    elseif method_exists(promote_rule,(S,T))
        return promote_rule(S,T)
    else
        error(strcat("no promotion exists for ",string(T)," and ",string(S)))
    end
end

promote() = ()
promote(x) = (x,)
function promote{T,S}(x::T, y::S)
    # print("promote: ",T,", ",S,"\n")
    #R = promote_type(T,S)
    # print("= ", R,"\n")
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
function promote{T,S,U}(x::T, y::S, z::U)
    R = promote_type(promote_type(T,S), U)
    convert((R...), (x, y, z))
end
function promote{T,S}(x::T, y::S, zs...)
    R = promote_type(T,S)
    for z = zs
        R = promote_type(R,typeof(z))
    end
    convert((R...), tuple(x,y,zs...))
end

## integer promotions ##

promote_rule(::Type{Int16}, ::Type{Int8} ) = Int16
promote_rule(::Type{Int32}, ::Type{Int8} ) = Int32
promote_rule(::Type{Int32}, ::Type{Int16}) = Int32
promote_rule(::Type{Int64}, ::Type{Int8} ) = Int64
promote_rule(::Type{Int64}, ::Type{Int16}) = Int64
promote_rule(::Type{Int64}, ::Type{Int32}) = Int64

promote_rule(::Type{Uint16}, ::Type{Uint8} ) = Uint16
promote_rule(::Type{Uint32}, ::Type{Uint8} ) = Uint32
promote_rule(::Type{Uint32}, ::Type{Uint16}) = Uint32
promote_rule(::Type{Uint64}, ::Type{Uint8} ) = Uint64
promote_rule(::Type{Uint64}, ::Type{Uint16}) = Uint64
promote_rule(::Type{Uint64}, ::Type{Uint32}) = Uint64

promote_rule(::Type{Int16}, ::Type{Uint8} ) = Int16
promote_rule(::Type{Int32}, ::Type{Uint8} ) = Int32
promote_rule(::Type{Int32}, ::Type{Uint16}) = Int32
promote_rule(::Type{Int64}, ::Type{Uint8} ) = Int64
promote_rule(::Type{Int64}, ::Type{Uint16}) = Int64
promote_rule(::Type{Int64}, ::Type{Uint32}) = Int64

promote_rule(::Type{Char}, ::Type{Int8})   = Int32
promote_rule(::Type{Char}, ::Type{Uint8})  = Int32
promote_rule(::Type{Char}, ::Type{Int16})  = Int32
promote_rule(::Type{Char}, ::Type{Uint16}) = Int32
promote_rule(::Type{Char}, ::Type{Int32})  = Int32
promote_rule(::Type{Char}, ::Type{Uint32}) = Uint32
promote_rule(::Type{Char}, ::Type{Int64})  = Int64
promote_rule(::Type{Char}, ::Type{Uint32}) = Uint64

## floating point promotions ##

promote_rule(::Type{Float64}, ::Type{Float32} ) = Float64

promote_rule(::Type{Float32}, ::Type{Int8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Int16}) = Float32
promote_rule(::Type{Float32}, ::Type{Int32}) = Float64
promote_rule(::Type{Float32}, ::Type{Int64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float64}, ::Type{Int8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Int16}) = Float64
promote_rule(::Type{Float64}, ::Type{Int32}) = Float64
promote_rule(::Type{Float64}, ::Type{Int64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float32}, ::Type{Uint8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Uint16}) = Float32
promote_rule(::Type{Float32}, ::Type{Uint32}) = Float64
promote_rule(::Type{Float32}, ::Type{Uint64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float64}, ::Type{Uint8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Uint16}) = Float64
promote_rule(::Type{Float64}, ::Type{Uint32}) = Float64
promote_rule(::Type{Float64}, ::Type{Uint64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float32}, ::Type{Char}) = Float32
promote_rule(::Type{Float64}, ::Type{Char}) = Float64

## promotion in arithmetic ##

(+)(x::Number, y::Number) = (+)(promote(x,y)...)
(*)(x::Number, y::Number) = (*)(promote(x,y)...)
(-)(x::Number, y::Number) = (-)(promote(x,y)...)
(/)(x::Number, y::Number) = (/)(promote(x,y)...)

## promotion in comparisons ##

(<) (x::Real, y::Real)     = (<)(promote(x,y)...)
(==)(x::Number, y::Number) = (==)(promote(x,y)...)

# these are defined for the fundamental < and == so that if a method is
# not found for e.g. <=, it is translated to < and == first, then promotion
# is handled after.

## integer-specific promotions ##

(%)(x::Int, y::Int) = (%)(promote(x,y)...)
div(x::Int, y::Int) = div(promote(x,y)...)

(&)(x::Int...) = (&)(promote(x...)...)
(|)(x::Int...) = (|)(promote(x...)...)
($)(x::Int...) = ($)(promote(x...)...)

## promotion catch-alls for undefined operations ##

no_op_err(name, T) = error(strcat(name," not defined for ",string(T)))
(+){T<:Number}(x::T, y::T) = no_op_err("+", T)
(*){T<:Number}(x::T, y::T) = no_op_err("*", T)
(-){T<:Number}(x::T, y::T) = no_op_err("-", T)
(/){T<:Number}(x::T, y::T) = no_op_err("/", T)
(<){T<:Real}  (x::T, y::T) = no_op_err("<", T)
(==){T<:Number}(x::T, y::T) = no_op_err("==", T)

(%){T<:Int}(x::T, y::T) = no_op_err("%", T)
div{T<:Int}(x::T, y::T) = no_op_err("div", T)
(&){T<:Int}(x::T, y::T) = no_op_err("&", T)
(|){T<:Int}(x::T, y::T) = no_op_err("|", T)
($){T<:Int}(x::T, y::T) = no_op_err("$", T)
