# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

# Tests for groupslices, groupinds, firstinds, lastinds
A = [1 2 3 ; 4 5 6 ; 7 8 9]
B = [11 12 13 ; 14 15 16 ; 17 18 19]
C = [21 22 23 ; 24 25 26 ; 27 28 29]
D = cat(3, A, B, C, C, B, C, A)

ic = [1;2;3;3;2;3;1]
ib = Vector{Int}[]
push!(ib,[1;7])
push!(ib,[2;5])
push!(ib,[3;4;6])
ia = [1;2;3]
ia1 = [1;2;3]
ia2 = [7;5;6]

ic_test = groupslices(D,3)
ib_test = groupinds(ic_test)
ia_test = firstinds(ic_test)
ia1_test = firstinds(ib_test)
ia2_test = lastinds(ib_test)

@test isequal(ic, ic_test)
@test isequal(ib, ib_test)
@test isequal(ia, ia_test)
@test isequal(ia1, ia1_test)
@test isequal(ia2, ia2_test)
