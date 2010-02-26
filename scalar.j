sign(x::Scalar) = (x < 0 ? -1 : (x > 0 ? +1 : 0))
signbit(x::Scalar) = (x < 0 ? -1 : +1)
signbit(x::Float) = (x < 0 ? -1 : (x > 0 ? 1 : (1.0/x < 0 ? -1 : +1)))

conj(x::Scalar) = x
transpose(x::Scalar) = x
ctranspose(x::Scalar) = conj(transpose(x))
