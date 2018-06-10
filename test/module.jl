# This file is a part of Julia. License is MIT: https://julialang.org/license
using Base.Test

# Test c api globals assignment
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal1), 10)
@test 10 == Base.testGlobal1

ccall(:jl_set_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Core), Ref(:testConst1), "Hello World")
@test "Hello World" == Core.testConst1

# Test c api globals re-assignment
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal2), 1)
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal2), 2)
@test 2 == Base.testGlobal2
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal2), "hi")
@test "hi" == Base.testGlobal2

# Okay to turn a non-const into a const.
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobalConst), "global")
ccall(:jl_set_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobalConst), "const")
@test "const" == Base.testGlobalConst

# Can't overwrite a const with a value of a new type.
ccall(:jl_set_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst2), 10)
ccall(:jl_set_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst2), "hi")
@test Base.testConst2 == 10

# *Can* overwrite a const only if it's the same type.
ccall(:jl_set_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst3), "initial")
ccall(:jl_set_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst3), "modified")
@test_broken "modified" == Base.testConst3

ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst3), "modified again")
@test_broken "modified again" == Base.testConst3
