immutable Transpose{C,A}
    data::A
end
call{C,T<:AbstractArray}(::Type{Transpose{C}}, A::T) = Transpose{C,T}(A)

transpose(A::AbstractArray) = Transpose{false}(A)
ctranspose(A::AbstractArray) = Transpose{true}(A)

transpose(T::Transpose{false}) = T.data
ctranspose(T::Transpose{true}) = T.data

transpose(T::Transpose{true}) = error("cannot take transpose of a ctranspose for now")
ctranspose(T::Transpose{false}) = error("cannot take ctranspose of a transpose for now")
