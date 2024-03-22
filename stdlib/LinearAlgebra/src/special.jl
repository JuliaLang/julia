# This file is a part of Julia. License is MIT: https://julialang.org/license

# Methods operating on different special matrix types

# Interconversion between special matrix types

# conversions from Diagonal to other special matrix types
Bidiagonal(A::Diagonal) = Bidiagonal(A.diag, fill!(similar(A.diag, length(A.diag)-1), 0), :U)
SymTridiagonal(A::Diagonal) = SymTridiagonal(A.diag, fill!(similar(A.diag, length(A.diag)-1), 0))
Tridiagonal(A::Diagonal) = Tridiagonal(fill!(similar(A.diag, length(A.diag)-1), 0), A.diag,
                                       fill!(similar(A.diag, length(A.diag)-1), 0))

# conversions from Bidiagonal to other special matrix types
Diagonal(A::Bidiagonal) = Diagonal(A.dv)
SymTridiagonal(A::Bidiagonal) =
    iszero(A.ev) ? SymTridiagonal(A.dv, A.ev) :
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
Tridiagonal(A::Bidiagonal) =
    Tridiagonal(A.uplo == 'U' ? fill!(similar(A.ev), 0) : A.ev, A.dv,
                A.uplo == 'U' ? A.ev : fill!(similar(A.ev), 0))

# conversions from SymTridiagonal to other special matrix types
Diagonal(A::SymTridiagonal) = Diagonal(A.dv)

# These can fail when ev has the same length as dv
# TODO: Revisit when a good solution for #42477 is found
Bidiagonal(A::SymTridiagonal) =
    iszero(A.ev) ? Bidiagonal(A.dv, A.ev, :U) :
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
Tridiagonal(A::SymTridiagonal) =
    Tridiagonal(copy(A.ev), A.dv, A.ev)

# conversions from Tridiagonal to other special matrix types
Diagonal(A::Tridiagonal) = Diagonal(A.d)
Bidiagonal(A::Tridiagonal) =
    iszero(A.dl) ? Bidiagonal(A.d, A.du, :U) :
    iszero(A.du) ? Bidiagonal(A.d, A.dl, :L) :
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))

# conversions from AbstractTriangular to special matrix types
Bidiagonal(A::AbstractTriangular) =
    isbanded(A, 0, 1) ? Bidiagonal(diag(A, 0), diag(A,  1), :U) : # is upper bidiagonal
    isbanded(A, -1, 0) ? Bidiagonal(diag(A, 0), diag(A, -1), :L) : # is lower bidiagonal
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))

_lucopy(A::Bidiagonal, T) = copymutable_oftype(Tridiagonal(A), T)
_lucopy(A::Diagonal, T)   = copymutable_oftype(Tridiagonal(A), T)
function _lucopy(A::SymTridiagonal, T)
    du = copy_similar(_evview(A), T)
    dl = copy.(transpose.(du))
    d  = copy_similar(A.dv, T)
    return Tridiagonal(dl, d, du)
end

const ConvertibleSpecialMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,AbstractTriangular}
const PossibleTriangularMatrix = Union{Diagonal, Bidiagonal, AbstractTriangular}

convert(::Type{T}, m::ConvertibleSpecialMatrix) where {T<:Diagonal}       = m isa T ? m :
    isdiag(m) ? T(m)::T : throw(ArgumentError("matrix cannot be represented as Diagonal"))
convert(::Type{T}, m::ConvertibleSpecialMatrix) where {T<:SymTridiagonal} = m isa T ? m :
    issymmetric(m) && isbanded(m, -1, 1) ? T(m)::T : throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
convert(::Type{T}, m::ConvertibleSpecialMatrix) where {T<:Tridiagonal}    = m isa T ? m :
    isbanded(m, -1, 1) ? T(m)::T : throw(ArgumentError("matrix cannot be represented as Tridiagonal"))

convert(::Type{T}, m::Union{LowerTriangular,UnitLowerTriangular}) where {T<:LowerTriangular} = m isa T ? m : T(m)::T
convert(::Type{T}, m::Union{UpperTriangular,UnitUpperTriangular}) where {T<:UpperTriangular} = m isa T ? m : T(m)::T

convert(::Type{T}, m::PossibleTriangularMatrix) where {T<:LowerTriangular} = m isa T ? m :
    istril(m) ? T(m)::T : throw(ArgumentError("matrix cannot be represented as LowerTriangular"))
convert(::Type{T}, m::PossibleTriangularMatrix) where {T<:UpperTriangular} = m isa T ? m :
    istriu(m) ? T(m)::T : throw(ArgumentError("matrix cannot be represented as UpperTriangular"))

# Constructs two method definitions taking into account (assumed) commutativity
# e.g. @commutative f(x::S, y::T) where {S,T} = x+y is the same is defining
#     f(x::S, y::T) where {S,T} = x+y
#     f(y::T, x::S) where {S,T} = f(x, y)
macro commutative(myexpr)
    @assert Base.is_function_def(myexpr) # Make sure it is a function definition
    y = copy(myexpr.args[1].args[2:end])
    reverse!(y)
    reversed_call = Expr(:(=), Expr(:call,myexpr.args[1].args[1],y...), myexpr.args[1])
    esc(Expr(:block, myexpr, reversed_call))
end

for op in (:+, :-)
    for (matrixtype, uplo, converttype) in ((:UpperTriangular, 'U', :UpperTriangular),
                                            (:UnitUpperTriangular, 'U', :UpperTriangular),
                                            (:LowerTriangular, 'L', :LowerTriangular),
                                            (:UnitLowerTriangular, 'L', :LowerTriangular))
        @eval begin
            function ($op)(A::$matrixtype, B::Bidiagonal)
                if B.uplo == $uplo
                    ($op)(A, convert($converttype, B))
                else
                    ($op).(A, B)
                end
            end

            function ($op)(A::Bidiagonal, B::$matrixtype)
                if A.uplo == $uplo
                    ($op)(convert($converttype, A), B)
                else
                    ($op).(A, B)
                end
            end
        end
    end
end

# disambiguation between triangular and banded matrices, banded ones "dominate"
_mul!(C::AbstractMatrix, A::AbstractTriangular, B::BandedMatrix, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))
_mul!(C::AbstractMatrix, A::BandedMatrix, B::AbstractTriangular, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))

function *(H::UpperHessenberg, B::Bidiagonal)
    T = promote_op(matprod, eltype(H), eltype(B))
    A = mul!(similar(H, T, size(H)), H, B)
    return B.uplo == 'U' ? UpperHessenberg(A) : A
end
function *(B::Bidiagonal, H::UpperHessenberg)
    T = promote_op(matprod, eltype(B), eltype(H))
    A = mul!(similar(H, T, size(H)), B, H)
    return B.uplo == 'U' ? UpperHessenberg(A) : A
end

function /(H::UpperHessenberg, B::Bidiagonal)
    T = typeof(oneunit(eltype(H))/oneunit(eltype(B)))
    A = _rdiv!(similar(H, T, size(H)), H, B)
    return B.uplo == 'U' ? UpperHessenberg(A) : A
end

function \(B::Bidiagonal, H::UpperHessenberg)
    T = typeof(oneunit(eltype(B))\oneunit(eltype(H)))
    A = ldiv!(similar(H, T, size(H)), B, H)
    return B.uplo == 'U' ? UpperHessenberg(A) : A
end

# specialized +/- for structured matrices. If these are removed, it falls
# back to broadcasting which has ~2-10x speed regressions.
# For the other structure matrix pairs, broadcasting works well.

# For structured matrix types with different non-zero diagonals the underlying
# representations must be promoted to the same type.
# For example, in Diagonal + Bidiagonal only the main diagonal is touched so
# the off diagonal could be a different type after the operation resulting in
# an error. See issue #28994

@commutative function (+)(A::Bidiagonal, B::Diagonal)
    newdv = A.dv + B.diag
    Bidiagonal(newdv, typeof(newdv)(A.ev), A.uplo)
end

function (-)(A::Bidiagonal, B::Diagonal)
    newdv = A.dv - B.diag
    Bidiagonal(newdv, typeof(newdv)(A.ev), A.uplo)
end

function (-)(A::Diagonal, B::Bidiagonal)
    newdv = A.diag - B.dv
    Bidiagonal(newdv, typeof(newdv)(-B.ev), B.uplo)
end

@commutative function (+)(A::Diagonal, B::SymTridiagonal)
    newdv = A.diag + B.dv
    SymTridiagonal(A.diag + B.dv, typeof(newdv)(B.ev))
end

function (-)(A::Diagonal, B::SymTridiagonal)
    newdv = A.diag - B.dv
    SymTridiagonal(newdv, typeof(newdv)(-B.ev))
end

function (-)(A::SymTridiagonal, B::Diagonal)
    newdv = A.dv - B.diag
    SymTridiagonal(newdv, typeof(newdv)(A.ev))
end

# this set doesn't have the aforementioned problem

@commutative (+)(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl+_evview(B), A.d+B.dv, A.du+_evview(B))
-(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl-_evview(B), A.d-B.dv, A.du-_evview(B))
-(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(_evview(A)-B.dl, A.dv-B.d, _evview(A)-B.du)

@commutative function (+)(A::Diagonal, B::Tridiagonal)
    newdv = A.diag + B.d
    Tridiagonal(typeof(newdv)(B.dl), newdv, typeof(newdv)(B.du))
end

function (-)(A::Diagonal, B::Tridiagonal)
    newdv = A.diag - B.d
    Tridiagonal(typeof(newdv)(-B.dl), newdv, typeof(newdv)(-B.du))
end

function (-)(A::Tridiagonal, B::Diagonal)
    newdv = A.d - B.diag
    Tridiagonal(typeof(newdv)(A.dl), newdv, typeof(newdv)(A.du))
end

@commutative function (+)(A::Bidiagonal, B::Tridiagonal)
    newdv = A.dv + B.d
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(B.dl), newdv, A.ev+B.du) : (A.ev+B.dl, newdv, typeof(newdv)(B.du)))...)
end

function (-)(A::Bidiagonal, B::Tridiagonal)
    newdv = A.dv - B.d
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-B.dl), newdv, A.ev-B.du) : (A.ev-B.dl, newdv, typeof(newdv)(-B.du)))...)
end

function (-)(A::Tridiagonal, B::Bidiagonal)
    newdv = A.d - B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(A.dl), newdv, A.du-B.ev) : (A.dl-B.ev, newdv, typeof(newdv)(A.du)))...)
end

@commutative function (+)(A::Bidiagonal, B::SymTridiagonal)
    newdv = A.dv + B.dv
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(_evview(B)), A.dv+B.dv, A.ev+_evview(B)) : (A.ev+_evview(B), A.dv+B.dv, typeof(newdv)(_evview(B))))...)
end

function (-)(A::Bidiagonal, B::SymTridiagonal)
    newdv = A.dv - B.dv
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-_evview(B)), newdv, A.ev-_evview(B)) : (A.ev-_evview(B), newdv, typeof(newdv)(-_evview(B))))...)
end

function (-)(A::SymTridiagonal, B::Bidiagonal)
    newdv = A.dv - B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(_evview(A)), newdv, _evview(A)-B.ev) : (_evview(A)-B.ev, newdv, typeof(newdv)(_evview(A))))...)
end

@commutative function (+)(A::Tridiagonal, B::UniformScaling)
    newd = A.d .+ Ref(B)
    Tridiagonal(typeof(newd)(A.dl), newd, typeof(newd)(A.du))
end

@commutative function (+)(A::SymTridiagonal, B::UniformScaling)
    newdv = A.dv .+ Ref(B)
    SymTridiagonal(newdv, typeof(newdv)(A.ev))
end

@commutative function (+)(A::Bidiagonal, B::UniformScaling)
    newdv = A.dv .+ Ref(B)
    Bidiagonal(newdv, typeof(newdv)(A.ev), A.uplo)
end

@commutative function (+)(A::Diagonal, B::UniformScaling)
    Diagonal(A.diag .+ Ref(B))
end

# StructuredMatrix - UniformScaling = StructuredMatrix + (-UniformScaling) =>
# no need to define reversed order
function (-)(A::UniformScaling, B::Tridiagonal)
    d = Ref(A) .- B.d
    Tridiagonal(convert(typeof(d), -B.dl), d, convert(typeof(d), -B.du))
end
function (-)(A::UniformScaling, B::SymTridiagonal)
    dv = Ref(A) .- B.dv
    SymTridiagonal(dv, convert(typeof(dv), -B.ev))
end
function (-)(A::UniformScaling, B::Bidiagonal)
    dv = Ref(A) .- B.dv
    Bidiagonal(dv, convert(typeof(dv), -B.ev), B.uplo)
end
function (-)(A::UniformScaling, B::Diagonal)
    Diagonal(Ref(A) .- B.diag)
end

## Diagonal construction from UniformScaling
Diagonal{T}(s::UniformScaling, m::Integer) where {T} = Diagonal{T}(fill(T(s.λ), m))
Diagonal(s::UniformScaling, m::Integer) = Diagonal{eltype(s)}(s, m)

Base.muladd(A::Union{Diagonal, UniformScaling}, B::Union{Diagonal, UniformScaling}, z::Union{Diagonal, UniformScaling}) =
    Diagonal(_diag_or_value(A) .* _diag_or_value(B) .+ _diag_or_value(z))

_diag_or_value(A::Diagonal) = A.diag
_diag_or_value(A::UniformScaling) = A.λ

# fill[stored]! methods
fillstored!(A::Diagonal, x) = (fill!(A.diag, x); A)
fillstored!(A::Bidiagonal, x) = (fill!(A.dv, x); fill!(A.ev, x); A)
fillstored!(A::Tridiagonal, x) = (fill!(A.dl, x); fill!(A.d, x); fill!(A.du, x); A)
fillstored!(A::SymTridiagonal, x) = (fill!(A.dv, x); fill!(A.ev, x); A)

_small_enough(A::Union{Diagonal, Bidiagonal}) = size(A, 1) <= 1
_small_enough(A::Tridiagonal) = size(A, 1) <= 2
_small_enough(A::SymTridiagonal) = size(A, 1) <= 2

function fill!(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal}, x)
    xT = convert(eltype(A), x)
    (iszero(xT) || _small_enough(A)) && return fillstored!(A, xT)
    throw(ArgumentError("array of type $(typeof(A)) and size $(size(A)) can
    not be filled with $x, since some of its entries are constrained."))
end

one(D::Diagonal) = Diagonal(one.(D.diag))
one(A::Bidiagonal{T}) where T = Bidiagonal(fill!(similar(A.dv, typeof(one(T))), one(T)), fill!(similar(A.ev, typeof(one(T))), zero(one(T))), A.uplo)
one(A::Tridiagonal{T}) where T = Tridiagonal(fill!(similar(A.du, typeof(one(T))), zero(one(T))), fill!(similar(A.d, typeof(one(T))), one(T)), fill!(similar(A.dl, typeof(one(T))), zero(one(T))))
one(A::SymTridiagonal{T}) where T = SymTridiagonal(fill!(similar(A.dv, typeof(one(T))), one(T)), fill!(similar(A.ev, typeof(one(T))), zero(one(T))))
for t in (:LowerTriangular, :UnitLowerTriangular, :UpperTriangular, :UnitUpperTriangular)
    @eval one(A::$t) = $t(one(parent(A)))
    @eval oneunit(A::$t) = $t(oneunit(parent(A)))
end

zero(D::Diagonal) = Diagonal(zero.(D.diag))
oneunit(D::Diagonal) = Diagonal(oneunit.(D.diag))

isdiag(A::HermOrSym{<:Any,<:Diagonal}) = isdiag(parent(A))
dot(x::AbstractVector, A::RealHermSymComplexSym{<:Real,<:Diagonal}, y::AbstractVector) =
    dot(x, A.data, y)

# equals and approx equals methods for structured matrices
# SymTridiagonal == Tridiagonal is already defined in tridiag.jl

==(A::Diagonal, B::Bidiagonal) = iszero(B.ev) && A.diag == B.dv
==(A::Diagonal, B::SymTridiagonal) = iszero(_evview(B)) && A.diag == B.dv
==(B::Bidiagonal, A::Diagonal) = A == B
==(A::Diagonal, B::Tridiagonal) = iszero(B.dl) && iszero(B.du) && A.diag == B.d
==(B::Tridiagonal, A::Diagonal) = A == B

function ==(A::Bidiagonal, B::Tridiagonal)
    if A.uplo == 'U'
        return iszero(B.dl) && A.dv == B.d && A.ev == B.du
    else
        return iszero(B.du) && A.dv == B.d && A.ev == B.dl
    end
end
==(B::Tridiagonal, A::Bidiagonal) = A == B

==(A::Bidiagonal, B::SymTridiagonal) = iszero(_evview(B)) && iszero(A.ev) && A.dv == B.dv
==(B::SymTridiagonal, A::Bidiagonal) = A == B

# TODO: remove these deprecations (used by SparseArrays in the past)
const _DenseConcatGroup = Union{}
const _SpecialArrays = Union{}

promote_to_array_type(::Tuple) = Matrix

# promote_to_arrays(n,k, T, A...) promotes any UniformScaling matrices
# in A to matrices of type T and sizes given by n[k:end].  n is an array
# so that the same promotion code can be used for hvcat.  We pass the type T
# so that we can re-use this code for sparse-matrix hcat etcetera.
promote_to_arrays_(n::Int, ::Type, a::Number) = a
promote_to_arrays_(n::Int, ::Type{Matrix}, J::UniformScaling{T}) where {T} = Matrix(J, n, n)
promote_to_arrays_(n::Int, ::Type, A::AbstractArray) = A
promote_to_arrays_(n::Int, ::Type, A::AbstractQ) = collect(A)
promote_to_arrays(n,k, ::Type) = ()
promote_to_arrays(n,k, ::Type{T}, A) where {T} = (promote_to_arrays_(n[k], T, A),)
promote_to_arrays(n,k, ::Type{T}, A, B) where {T} =
    (promote_to_arrays_(n[k], T, A), promote_to_arrays_(n[k+1], T, B))
promote_to_arrays(n,k, ::Type{T}, A, B, C) where {T} =
    (promote_to_arrays_(n[k], T, A), promote_to_arrays_(n[k+1], T, B), promote_to_arrays_(n[k+2], T, C))
promote_to_arrays(n,k, ::Type{T}, A, B, Cs...) where {T} =
    (promote_to_arrays_(n[k], T, A), promote_to_arrays_(n[k+1], T, B), promote_to_arrays(n,k+2, T, Cs...)...)

_us2number(A) = A
_us2number(J::UniformScaling) = J.λ

for (f, _f, dim, name) in ((:hcat, :_hcat, 1, "rows"), (:vcat, :_vcat, 2, "cols"))
    @eval begin
        @inline $f(A::Union{AbstractArray,AbstractQ,UniformScaling}...) = $_f(A...)
        # if there's a Number present, J::UniformScaling must be 1x1-dimensional
        @inline $f(A::Union{AbstractArray,AbstractQ,UniformScaling,Number}...) = $f(map(_us2number, A)...)
        function $_f(A::Union{AbstractArray,AbstractQ,UniformScaling,Number}...; array_type = promote_to_array_type(A))
            n = -1
            for a in A
                if !isa(a, UniformScaling)
                    require_one_based_indexing(a)
                    na = size(a,$dim)
                    n >= 0 && n != na &&
                        throw(DimensionMismatch(string("number of ", $name,
                            " of each array must match (got ", n, " and ", na, ")")))
                    n = na
                end
            end
            n == -1 && throw(ArgumentError($("$f of only UniformScaling objects cannot determine the matrix size")))
            return cat(promote_to_arrays(fill(n, length(A)), 1, array_type, A...)..., dims=Val(3-$dim))
        end
    end
end

hvcat(rows::Tuple{Vararg{Int}}, A::Union{AbstractArray,AbstractQ,UniformScaling}...) = _hvcat(rows, A...)
hvcat(rows::Tuple{Vararg{Int}}, A::Union{AbstractArray,AbstractQ,UniformScaling,Number}...) = _hvcat(rows, A...)
function _hvcat(rows::Tuple{Vararg{Int}}, A::Union{AbstractArray,AbstractQ,UniformScaling,Number}...; array_type = promote_to_array_type(A))
    require_one_based_indexing(A...)
    nr = length(rows)
    sum(rows) == length(A) || throw(ArgumentError("mismatch between row sizes and number of arguments"))
    n = fill(-1, length(A))
    needcols = false # whether we also need to infer some sizes from the column count
    j = 0
    for i = 1:nr # infer UniformScaling sizes from row counts, if possible:
        ni = -1 # number of rows in this block-row, -1 indicates unknown
        for k = 1:rows[i]
            if !isa(A[j+k], UniformScaling)
                na = size(A[j+k], 1)
                ni >= 0 && ni != na &&
                    throw(DimensionMismatch("mismatch in number of rows"))
                ni = na
            end
        end
        if ni >= 0
            for k = 1:rows[i]
                n[j+k] = ni
            end
        else # row consisted only of UniformScaling objects
            needcols = true
        end
        j += rows[i]
    end
    if needcols # some sizes still unknown, try to infer from column count
        nc = -1
        j = 0
        for i = 1:nr
            nci = 0
            rows[i] > 0 && n[j+1] == -1 && (j += rows[i]; continue)
            for k = 1:rows[i]
                nci += isa(A[j+k], UniformScaling) ? n[j+k] : size(A[j+k], 2)
            end
            nc >= 0 && nc != nci && throw(DimensionMismatch("mismatch in number of columns"))
            nc = nci
            j += rows[i]
        end
        nc == -1 && throw(ArgumentError("sizes of UniformScalings could not be inferred"))
        j = 0
        for i = 1:nr
            if rows[i] > 0 && n[j+1] == -1 # this row consists entirely of UniformScalings
                nci, r = divrem(nc, rows[i])
                r != 0 && throw(DimensionMismatch("indivisible UniformScaling sizes"))
                for k = 1:rows[i]
                    n[j+k] = nci
                end
            end
            j += rows[i]
        end
    end
    Amat = promote_to_arrays(n, 1, array_type, A...)
    # We have two methods for promote_to_array_type, one returning Matrix and
    # another one returning SparseMatrixCSC (in SparseArrays.jl). In the dense
    # case, we cannot call hvcat for the promoted UniformScalings because this
    # causes a stack overflow. In the sparse case, however, we cannot call
    # typed_hvcat because we need a sparse output.
    if array_type == Matrix
        return typed_hvcat(promote_eltype(Amat...), rows, Amat...)
    else
        return hvcat(rows, Amat...)
    end
end

# factorizations
function cholesky(S::RealHermSymComplexHerm{<:Real,<:SymTridiagonal}, ::NoPivot = NoPivot(); check::Bool = true)
    T = choltype(eltype(S))
    B = Bidiagonal{T}(diag(S, 0), diag(S, S.uplo == 'U' ? 1 : -1), sym_uplo(S.uplo))
    cholesky!(Hermitian(B, sym_uplo(S.uplo)), NoPivot(); check = check)
end
