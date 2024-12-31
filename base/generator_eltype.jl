function eltype(::Type{Generator{A, typeof(identity)}}) where {A}
    eltype(A)
end

function eltype(::Type{Generator{A, Fix1{typeof(getindex), B}}}) where {A, B}
    if B <: Type
        # TODO: theoretically we could be more precise here and return a subtype
        # of `AbstractVector`. The problem is that several packages do dubious
        # punning of `getindex`. See
        # https://github.com/mcabbott/AxisKeys.jl/issues/163
        Any
    else
        # TODO: add special-casing for the case when `B <: AbstractArray`?
        #       It could make sense to do this because arrays support multiple
        #       index types, not all of which are necessarily covered by
        #       `keytype`.
        if eltype(A) == keytype(B)
            valtype(B)
        else
            Any
        end
    end
end

function eltype(::Type{Generator{A, Fix1{typeof(getfield), B}}} where {A}) where {B}
    typejoin(fieldtypes(B)...)
end
