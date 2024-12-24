function eltype(::Type{Generator{A, typeof(identity)}}) where {A}
    eltype(A)
end

function IteratorEltype(::Type{Generator{A, typeof(identity)}}) where {A}
    IteratorEltype(A)
end

function eltype(::Type{Generator{A, Fix1{typeof(getindex), B}}} where {A}) where {B}
    if B <: Type
        # a user may overload `getindex(user_type)` to return a non-`Vector` `AbstractVector`
        Any
    else
        eltype(B)
    end
end

function IteratorEltype(::Type{Generator{A, Fix1{typeof(getindex), B}}} where {A}) where {B}
    if B <: Type
        EltypeUnknown()
    else
        IteratorEltype(B)
    end
end

function eltype(::Type{Generator{A, Fix1{typeof(getfield), B}}} where {A}) where {B}
    typejoin(fieldtypes(B)...)
end
