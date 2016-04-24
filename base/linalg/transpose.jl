immutable Transpose{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    conjugated::Bool
end

# temp definitions while Transpose is not subtype of AbstractMatrix
# Base.eltype{T}(A::Transpose{T}) = T

# end temp

Base.size(A::Transpose) = size(A.data, 2), size(A.data, 1)
function Base.size(A::Transpose, i::Integer)
    if i <= 0
        error("arraysize: dimension out of range")
    elseif i <= 2
        return size(A.data, 3 - i)
    else
        return 1
    end
end

@inline function Base.getindex(A::Transpose, i::Integer, j::Integer)
    aji = A.data[j,i]
    return ifelse(A.conjugated, conj(aji), aji)
end

Base.similar(A::Transpose, T::Type, d::Dims) = similar(A.data, T, d)

ctranspose(A::AbstractMatrix) = Transpose{eltype(A), typeof(A)}(A, true)
function ctranspose(A::Transpose)
    if A.conjugated
        return A.data
    else
        return map(conj, A.data)
    end
end
transpose(A::AbstractMatrix) = Transpose{eltype(A), typeof(A)}(A, false)
function transpose(A::Transpose)
    if A.conjugated
        return map(conj, A.data)
    else
        return A
    end
end

transpose(x::AbstractVector) = [ transpose(v) for i=1, v in x ]
ctranspose{T}(x::AbstractVector{T}) = T[ ctranspose(v) for i=1, v in x ] #Fixme comprehension


# From operators.jl

# should probably be map(conj, transpose(x)) but map seems to assume that elements are iterable
# ctranspose(x) = conj(x)


# ctranspose(a::AbstractArray) = error("ctranspose not implemented for $(typeof(a)). Consider adding parentheses, e.g. A*(B*C') instead of A*B*C' to avoid explicit calculation of the transposed matrix.")
# transpose(a::AbstractArray) = error("transpose not implemented for $(typeof(a)). Consider adding parentheses, e.g. A*(B*C.') instead of A*B*C' to avoid explicit calculation of the transposed matrix.")


# transposed divide
# TODO! Probably reenable when lrdiv uses Transpose
# Ac_rdiv_B(a,b)  = ctranspose(a)/b
# A_rdiv_Bc(a,b)  = a/ctranspose(b)
# Ac_rdiv_Bc(a,b) = ctranspose(a)/ctranspose(b)
# At_rdiv_B(a,b)  = transpose(a)/b
# A_rdiv_Bt(a,b)  = a/transpose(b)
# At_rdiv_Bt(a,b) = transpose(a)/transpose(b)

# Ac_ldiv_B(a,b)  = ctranspose(a)\b
# A_ldiv_Bc(a,b)  = a\ctranspose(b)
# Ac_ldiv_Bc(a,b) = ctranspose(a)\ctranspose(b)
# At_ldiv_B(a,b)  = transpose(a)\b
# A_ldiv_Bt(a,b)  = a\transpose(b)
# At_ldiv_Bt(a,b) = At_ldiv_B(a,transpose(b))
# Ac_ldiv_Bt(a,b) = Ac_ldiv_B(a,transpose(b))

# TODO! Remove when lrdiv uses Transpose
Ac_rdiv_Bc(a::Number,b::Number) = conj(a)/conj(b)

# """
#     Ac_ldiv_B(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ`` \\ ``B``.
# """
# Ac_ldiv_B

# """
#     Ac_rdiv_B(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ / B``.
# """
# Ac_rdiv_B

# """
#     At_rdiv_Bt(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ / Bᵀ``.
# """
# At_rdiv_Bt

# """
#     A_ldiv_Bt(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``A`` \\ ``Bᵀ``.
# """
# A_ldiv_Bt

# """
#     Ac_ldiv_Bc(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ`` \\ ``Bᴴ``.
# """
# Ac_ldiv_Bc

# """
#     At_ldiv_Bt(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ`` \\ ``Bᵀ``.
# """
# At_ldiv_Bt

# """
#     At_ldiv_B(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ`` \\ ``B``.
# """
# At_ldiv_B

# """
#     A_ldiv_Bc(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``A`` \\ ``Bᴴ``.
# """
# A_ldiv_Bc

# """
#     A_rdiv_Bc(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``A / Bᴴ``.
# """
# A_rdiv_Bc

# """
#     A_rdiv_Bt(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``A / Bᵀ``.
# """
# A_rdiv_Bt

# """
#     Ac_rdiv_Bc(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ / Bᴴ``.
# """
# Ac_rdiv_Bc

# """
#     At_rdiv_B(A, B)

# For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ / B``.
# """
# At_rdiv_B


const transposebaselength=64

function transpose!(B::AbstractMatrix,A::AbstractMatrix)
    m, n = size(A)
    size(B,1) == n && size(B,2) == m || throw(DimensionMismatch("transpose"))

    if m*n<=4*transposebaselength
        @inbounds begin
            for j = 1:n #Fixme iter
                for i = 1:m #Fixme iter
                    B[j,i] = transpose(A[i,j])
                end
            end
        end
    else
        transposeblock!(B,A,m,n,0,0)
    end
    return B
end
function transpose!(B::AbstractVector, A::AbstractMatrix)
    length(B) == length(A) && size(A,1) == 1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transpose!(B::AbstractMatrix, A::AbstractVector)
    length(B) == length(A) && size(B,1) == 1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transposeblock!(B::AbstractMatrix,A::AbstractMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n) #Fixme iter
                for i = offseti+(1:m) #Fixme iter
                    B[j,i] = transpose(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        transposeblock!(B,A,newm,n,offseti,offsetj)
        transposeblock!(B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        transposeblock!(B,A,m,newn,offseti,offsetj)
        transposeblock!(B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end
function ctranspose!(B::AbstractMatrix,A::AbstractMatrix)
    m, n = size(A)
    size(B,1) == n && size(B,2) == m || throw(DimensionMismatch("transpose"))

    if m*n<=4*transposebaselength
        @inbounds begin
            for j = 1:n #Fixme iter
                for i = 1:m #Fixme iter
                    B[j,i] = ctranspose(A[i,j])
                end
            end
        end
    else
        ctransposeblock!(B,A,m,n,0,0)
    end
    return B
end
function ctranspose!(B::AbstractVector, A::AbstractMatrix)
    length(B) == length(A) && size(A,1) == 1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctranspose!(B::AbstractMatrix, A::AbstractVector)
    length(B) == length(A) && size(B,1) == 1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctransposeblock!(B::AbstractMatrix,A::AbstractMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n) #Fixme iter
                for i = offseti+(1:m) #Fixme iter
                    B[j,i] = ctranspose(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        ctransposeblock!(B,A,newm,n,offseti,offsetj)
        ctransposeblock!(B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        ctransposeblock!(B,A,m,newn,offseti,offsetj)
        ctransposeblock!(B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end
function ccopy!(B, A)
    for (i,j) = zip(eachindex(B),eachindex(A))
        B[i] = ctranspose(A[j])
    end
end

function copy_transpose!{R,S}(B::AbstractVecOrMat{R}, ir_dest::Range{Int}, jr_dest::Range{Int},
                              A::AbstractVecOrMat{S}, ir_src::Range{Int}, jr_src::Range{Int})
    if length(ir_dest) != length(jr_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(jr_src)," and ",length(ir_dest),")")))
    end
    if length(jr_dest) != length(ir_src)
        throw(ArgumentError(string("source and destination must have same size (got ",
                                   length(ir_src)," and ",length(jr_dest),")")))
    end
    @boundscheck checkbounds(B, ir_dest, jr_dest)
    @boundscheck checkbounds(A, ir_src, jr_src)
    idest = first(ir_dest)
    for jsrc in jr_src
        jdest = first(jr_dest)
        for isrc in ir_src
            B[idest,jdest] = A[isrc,jsrc]
            jdest += step(jr_dest)
        end
        idest += step(ir_dest)
    end
    return B
end

function convert{T,S}(::Type{Matrix}, A::Transpose{T,S})
    B = Array(T, size(A))
    if A.conjugated
        return ctranspose!(B, A.data)
    else
        return transpose!(B, A.data)
    end
end
function convert{T}(::Type{AbstractMatrix{T}}, A::Transpose)
    B = convert(AbstractMatrix{T}, A.data)
    return Transpose{T, typeof(B)}(B, A.conjugated)
end

convert(::Type{BitMatrix}, B::Transpose{Bool,BitMatrix}) = Base._transpose(B.data)

"""
    ctranspose!(dest,src)

Conjugate transpose array `src` and store the result in the preallocated array `dest`, which
should have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
ctranspose!

"""
    transpose!(dest,src)

Transpose array `src` and store the result in the preallocated array `dest`, which should
have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition is
supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
transpose!

"""
    ctranspose(A)

The conjugate transposition operator (`'`).
"""
ctranspose

"""
    transpose(A)

The transposition operator (`.'`).
"""
transpose