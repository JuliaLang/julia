# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using SuiteSparse


# define types used for testing
import Base: setindex!, getindex, size, transpose
struct TestCSC{Tv,Ti} <: AbstractSparseMatrixCSC{Tv,Ti}
  m::Int
  n::Int
  colptr::Vector{Ti}
  rowval::Vector{Ti}
  nzval::Vector{Tv}
  # for simplicity, have an underlying matrix and alias the above arrays
  _A::SparseMatrixCSC{Tv,Ti}
end

function TestCSC(A::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}

  A2 = copy(A)
  return TestCSC{Tv,Ti}(A2.m, A2.n, A2.colptr, A2.rowval, A2.nzval, A2)
end

function size(A::TestCSC)
  return (A.m, A.n)
end

function setindex!(A::TestCSC, value, key...)
  setindex!(A._A, value, key...)
end

function getindex(A::TestCSC, key...)
  getindex(A._A, key...)
end

function transpose(A::TestCSC{Tv,Ti}) where {Tv,Ti}

  A2 = transpose(A._A)
  convert(TestCSC{Tv,Ti}, A2)
end

#=
# TODO: implement in Base
import Base:nnz
nnz(A::TestCSC) = nnz(A._A)
=#

if Base.USE_GPL_LIBS
   include("umfpack.jl")
   include("cholmod.jl")
   include("spqr.jl")
end
