# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

@deprecate mul!(C, A, B, α, β) addmul!(C, A, B, α, β)
