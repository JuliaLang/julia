#### Specialized matrix types ####

## Bidiagonal matrices


type Bidiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T} # diagonal
    ev::Vector{T} # sub/super diagonal
    isupper::Bool # is upper bidiagonal (true) or lower (false)
    function Bidiagonal{T}(dv::Vector{T}, ev::Vector{T}, isupper::Bool)
        if length(ev)!=length(dv)-1 error("dimension mismatch") end
        new(dv, ev, isupper)
    end
end

Bidiagonal{T<:BlasFloat}(dv::Vector{T}, ev::Vector{T}, isupper::Bool)=Bidiagonal{T}(copy(dv), copy(ev), isupper)
Bidiagonal{T}(dv::Vector{T}, ev::Vector{T}) = error("Did you want an upper or lower Bidiagonal? Try again with an additional true (upper) or false (lower) argument.")


#Convert from BLAS uplo flag to boolean internal
function Bidiagonal{T<:BlasFloat}(dv::Vector{T}, ev::Vector{T}, uplo::BlasChar)
    if uplo=='U'
        isupper = true
    elseif uplo=='L'
        isupper = false
    else
        error(string("Bidiagonal can only be upper 'U' or lower 'L' but you said '", uplo, "'"))
    end
    Bidiagonal{T}(copy(dv), copy(ev), isupper)
end

function Bidiagonal{Td<:Number,Te<:Number}(dv::Vector{Td}, ev::Vector{Te}, isupper::Bool)
    T = promote(Td,Te)
    Bidiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev), isupper)
end

Bidiagonal(A::AbstractMatrix, isupper::Bool)=Bidiagonal(diag(A), diag(A, isupper?1:-1), isupper)

#Converting from Bidiagonal to dense Matrix
full{T}(M::Bidiagonal{T}) = convert(Matrix{T}, M)
convert{T}(::Type{Matrix{T}}, A::Bidiagonal{T})=diagm(A.dv) + diagm(A.ev, A.isupper?1:-1)
promote_rule{T}(::Type{Matrix{T}}, ::Type{Bidiagonal{T}})=Matrix{T}
promote_rule{T,S}(::Type{Matrix{T}}, ::Type{Bidiagonal{S}})=Matrix{promote_type(T,S)}

#Converting from Bidiagonal to Tridiagonal
Tridiagonal{T}(M::Bidiagonal{T}) = convert(Tridiagonal{T}, M)
function convert{T}(::Type{Tridiagonal{T}}, A::Bidiagonal{T})
    z = zeros(T, size(A)[1]-1)
    A.isupper ? Tridiagonal(A.ev, A.dv, z) : Tridiagonal(z, A.dv, A.ev)
end
promote_rule{T}(::Type{Tridiagonal{T}}, ::Type{Bidiagonal{T}})=Tridiagonal{T}
promote_rule{T,S}(::Type{Tridiagonal{T}}, ::Type{Bidiagonal{S}})=Tridiagonal{promote_type(T,S)}


function show(io::IO, M::Bidiagonal)
    println(io, summary(M), ":")
    print(io, " diag:")
    print_matrix(io, (M.dv)')
    print(io, M.isupper?"\nsuper:":"\n  sub:")
    print_matrix(io, (M.ev)')
end

size(M::Bidiagonal) = (length(M.dv), length(M.dv))
size(M::Bidiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(M.dv) : 1)

#Elementary operations
copy(M::Bidiagonal) = Bidiagonal(copy(M.dv), copy(M.ev), copy(M.isupper))
round(M::Bidiagonal) = Bidiagonal(round(M.dv), round(M.ev), M.isupper)
iround(M::Bidiagonal) = Bidiagonal(iround(M.dv), iround(M.ev), M.isupper)

conj(M::Bidiagonal) = Bidiagonal(conj(M.dv), conj(M.ev), M.isupper)
transpose(M::Bidiagonal) = Bidiagonal(M.dv, M.ev, !M.isupper)
ctranspose(M::Bidiagonal) = Bidiagonal(conj(M.dv), conj(M.ev), !M.isupper)

function +(A::Bidiagonal, B::Bidiagonal)
    if A.isupper==B.isupper
        Bidiagonal(A.dv+B.dv, A.ev+B.ev, A.isupper)
    else #return tridiagonal
        if A.isupper #&& !B.isupper
            Tridiagonal(B.ev,A.dv+B.dv,A.ev)
        else
            Tridiagonal(A.ev,A.dv+B.dv,B.ev)
        end
    end
end

function -(A::Bidiagonal, B::Bidiagonal)
    if A.isupper==B.isupper
        Bidiagonal(A.dv-B.dv, A.ev-B.ev, A.isupper)
    else #return tridiagonal
        if A.isupper #&& !B.isupper
            Tridiagonal(-B.ev,A.dv-B.dv,A.ev)
        else
            Tridiagonal(A.ev,A.dv-B.dv,-B.ev)
        end
    end
end

-(A::Bidiagonal)=Bidiagonal(-A.dv,-A.ev)
#XXX Returns dense matrix but really should be banded
*(A::Bidiagonal, B::Bidiagonal) = full(A)*full(B)
*(A::Bidiagonal, B::Number) = Bidiagonal(A.dv*B, A.ev*B, A.isupper)
*(B::Number, A::Bidiagonal) = A*B
/(A::Bidiagonal, B::Number) = Bidiagonal(A.dv/B, A.ev/B, A.isupper)
==(A::Bidiagonal, B::Bidiagonal) = (A.dv==B.dv) && (A.ev==B.ev) && (A.isupper==B.isupper)

*(A::SymTridiagonal, B::Bidiagonal) = full(A)*full(B)
*(A::Bidiagonal, B::SymTridiagonal) = full(A)*full(B)
*(A::Tridiagonal, B::Bidiagonal) = full(A)*full(B)
*(A::Bidiagonal, B::Tridiagonal) = full(A)*full(B)

# solver uses tridiagonal gtsv! 
function \{T<:BlasFloat}(M::Bidiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        z = zeros(size(M, 1) - 1)
        if M.isupper
            return LAPACK.gtsv!(z, copy(M.dv), copy(M.ev), copy(rhs))
        else
            return LAPACK.gtsv!(copy(M.ev), copy(M.dv), z, copy(rhs))
        end
    end
    solve(M, rhs)  # use the Julia "fallback"
end

#Wrap bdsdc to compute singular values and vectors
svdvals{T<:Real}(M::Bidiagonal{T})=LAPACK.bdsdc!(M.isupper?'U':'L', 'N', copy(M.dv), copy(M.ev))
svd    {T<:Real}(M::Bidiagonal{T})=LAPACK.bdsdc!(M.isupper?'U':'L', 'I', copy(M.dv), copy(M.ev))

