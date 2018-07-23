# This file is a part of Julia. License is MIT: https://julialang.org/license
using Base.Test

# Test c api globals assignment
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal1), 10)
@test 10 == Base.testGlobal1

ccall(:jl_define_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Core), Ref(:testConst1), "Hello World")
@test "Hello World" == Core.testConst1

# Test c api globals re-assignment
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal2), 1)
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal2), 2)
@test 2 == Base.testGlobal2
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobal2), "hi")  # Change type
@test "hi" == Base.testGlobal2

# Cannot define a new const when a variable already exists.
ccall(:jl_set_global, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testGlobalConst), "global")
@test_throws ErrorException ccall(:jl_define_const, Void, (Ref{Module},Ref{Symbol},Any),
                   Ref(Base), Ref(:testGlobalConst), "const")
@test "global" == Base.testGlobalConst

# Can't define a new const when a _const_ variable already exists.
ccall(:jl_define_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst2), 10)
@test_throws ErrorException ccall(:jl_define_const, Void,  # Same type
             (Ref{Module},Ref{Symbol},Any), Ref(Base), Ref(:testConst2), 20)
@test_throws ErrorException ccall(:jl_define_const, Void,  # Different type
             (Ref{Module},Ref{Symbol},Any), Ref(Base), Ref(:testConst2), "hi")
@test Base.testConst2 == 10

# jl_set_global can't change value of a const even if it's the same type.
ccall(:jl_define_const, Void, (Ref{Module},Ref{Symbol},Any),
        Ref(Base), Ref(:testConst3), "initial")
@test_throws ErrorException ccall(:jl_set_global, Void,
         (Ref{Module},Ref{Symbol},Any), Ref(Base), Ref(:testConst3), "modified")
