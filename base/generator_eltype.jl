function eltype(::Type{Generator{A, typeof(identity)}}) where {A}
    eltype(A)
end

function eltype(::Type{Generator{A, Fix1{typeof(getindex), B}}}) where {A, B}
    if B <: Type
        # a user may overload `getindex(user_type)` to return a non-`Vector` `AbstractVector`
        Any
    elseif (eltype(A) == keytype(B)) || ((eltype(A) <: Integer) && (keytype(B) <: Integer))
        valtype(B)
    else
        Any
    end
end

function eltype(::Type{Generator{A, Fix1{typeof(getfield), B}}} where {A}) where {B}
    typejoin(fieldtypes(B)...)
end
