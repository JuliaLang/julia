# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: llvmcall
using InteractiveUtils: code_llvm

#function add1234(x::Tuple{Int32,Int32,Int32,Int32})
#    llvmcall("""%3 = add <4 x i32> %1, %0
#                ret <4 x i32> %3""",
#        Tuple{Int32,Int32,Int32,Int32},
#        Tuple{Tuple{Int32,Int32,Int32,Int32},
#        Tuple{Int32,Int32,Int32,Int32}},
#        (Int32(1),Int32(2),Int32(3),Int32(4)),
#        x)
#end
#
#function add1234(x::NTuple{4,Int64})
#    llvmcall("""%3 = add <4 x i64> %1, %0
#      ret <4 x i64> %3""",NTuple{4,Int64},
#      Tuple{NTuple{4,Int64},NTuple{4,Int64}},
#        (Int64(1),Int64(2),Int64(3),Int64(4)),
#      x)
#end
#
function add1234(x::Tuple{Int32,Int32,Int32,Int32})
    llvmcall("""%3 = extractvalue [4 x i32] %0, 0
      %4 = extractvalue [4 x i32] %0, 1
      %5 = extractvalue [4 x i32] %0, 2
      %6 = extractvalue [4 x i32] %0, 3
      %7 = extractvalue [4 x i32] %1, 0
      %8 = extractvalue [4 x i32] %1, 1
      %9 = extractvalue [4 x i32] %1, 2
      %10 = extractvalue [4 x i32] %1, 3
      %11 = add i32 %3, %7
      %12 = add i32 %4, %8
      %13 = add i32 %5, %9
      %14 = add i32 %6, %10
      %15 = insertvalue [4 x i32] undef, i32 %11, 0
      %16 = insertvalue [4 x i32] %15, i32 %12, 1
      %17 = insertvalue [4 x i32] %16, i32 %13, 2
      %18 = insertvalue [4 x i32] %17, i32 %14, 3
      ret [4 x i32] %18""",Tuple{Int32,Int32,Int32,Int32},
      Tuple{Tuple{Int32,Int32,Int32,Int32},Tuple{Int32,Int32,Int32,Int32}},
        (Int32(1),Int32(2),Int32(3),Int32(4)),
        x)
end

@test add1234(map(Int32,(2,3,4,5))) === map(Int32,(3,5,7,9))
#@test add1234(map(Int64,(2,3,4,5))) === map(Int64,(3,5,7,9))

# Test whether llvmcall escapes the function name correctly
baremodule PlusTest
    using Base: llvmcall
    using Test
    using Base

    function +(x::Int32, y::Int32)
        llvmcall("""%3 = add i32 %1, %0
                    ret i32 %3""",
            Int32,
            Tuple{Int32, Int32},
            x,
            y)
    end
    @test Int32(1) + Int32(2) == Int32(3)
end

# issue #11800
@test_throws ErrorException eval(Expr(:call,Core.Intrinsics.llvmcall,
    """%3 = add i32 %1, %0
       ret i32 %3""", Int32, Tuple{Int32, Int32},
        Int32(1), Int32(2))) # llvmcall must be compiled to be called

# Test whether declarations work properly
function undeclared_ceil(x::Float64)
    llvmcall("""%2 = call double @llvm.ceil.f64(double %0)
        ret double %2""", Float64, Tuple{Float64}, x)
end
@test_throws ErrorException undeclared_ceil(4.2)
@test_throws ErrorException undeclared_ceil(4.2)

function declared_floor(x::Float64)
    llvmcall(
        ("""declare double @llvm.floor.f64(double)
            define double @entry(double) #0 {
            1:
                %2 = call double @llvm.floor.f64(double %0)
                ret double %2
            }
            attributes #0 = { alwaysinline }
         """, "entry"), Float64, Tuple{Float64}, x)
end
@test declared_floor(4.2) ≈ 4.
ir = sprint(code_llvm, declared_floor, Tuple{Float64})
@test occursin("call double @llvm.floor.f64", ir) # should be inlined

function doubly_declared_floor(x::Float64)
    llvmcall(
        ("""declare double @llvm.floor.f64(double)
            define double @entry(double) #0 {
            1:
                %2 = call double @llvm.floor.f64(double %0)
                ret double %2
            }
            attributes #0 = { alwaysinline }
         """, "entry"), Float64, Tuple{Float64}, x+1)-1
end
@test doubly_declared_floor(4.2) ≈ 4.

function doubly_declared2_trunc(x::Float64)
    a = llvmcall(
        ("""declare double @llvm.trunc.f64(double)
            define double @entry(double) #0 {
            1:
                %2 = call double @llvm.trunc.f64(double %0)
                ret double %2
            }
            attributes #0 = { alwaysinline }
         """, "entry"), Float64, Tuple{Float64}, x)
    b = llvmcall(
        ("""declare double @llvm.trunc.f64(double)
            define double @entry(double) #0 {
            1:
                %2 = call double @llvm.trunc.f64(double %0)
                ret double %2
            }
            attributes #0 = { alwaysinline }
         """, "entry"), Float64, Tuple{Float64}, x+1)-1
    a + b
end
@test doubly_declared2_trunc(4.2) ≈ 8.

# Test for proper mangling of external (C) functions
function call_jl_errno()
    llvmcall(
        ("""declare i32 @jl_errno()
            define i32 @entry() #0 {
            0:
                %r = call i32 @jl_errno()
                ret i32 %r
            }
            attributes #0 = { alwaysinline }
         """, "entry"),Int32,Tuple{})
end
call_jl_errno()

module ObjLoadTest
    using Base: llvmcall, @ccallable
    using Test
    didcall = false
    @ccallable Cvoid function jl_the_callback()
        global didcall
        didcall = true
        nothing
    end
    # Make sure everything up until here gets compiled
    jl_the_callback(); didcall = false
    function do_the_call()
        llvmcall(
            ("""declare void @jl_the_callback()
                define void @entry() #0 {
                0:
                    call void @jl_the_callback()
                    ret void
                }
                attributes #0 = { alwaysinline }
            """, "entry"),Cvoid,Tuple{})
    end
    do_the_call()
    @test didcall
end

# Test for proper parenting
local foo
function foo()
    # this IR snippet triggers an optimization relying
    # on the llvmcall function having a parent module
    Base.llvmcall(
     """%1 = getelementptr i64, i64* null, i64 1
        ret void""",
    Cvoid, Tuple{})
end
code_llvm(devnull, foo, ())

module CcallableRetTypeTest
    using Base: llvmcall, @ccallable
    using Test
    @ccallable function jl_test_returns_float()::Float64
        return 42
    end
    function do_the_call()
        llvmcall(
            ("""declare double @jl_test_returns_float()
                define double @entry() #0 {
                0:
                    %1 = call double @jl_test_returns_float()
                    ret double %1
                }
                attributes #0 = { alwaysinline }
            """, "entry"),Float64,Tuple{})
    end
    @test do_the_call() === 42.0
end

# If this test breaks, you've probably broken Cxx.jl - please check
module LLVMCallFunctionTest
    using Base: llvmcall
    using Test

    const libllvmcalltest = "libllvmcalltest"
    const the_ir = unsafe_string(ccall((:MakeIdentityFunction, libllvmcalltest), Cstring, (Any,), Any))

    @eval really_complicated_identity(x) = llvmcall(($(the_ir), "identity"), Any, Tuple{Any}, x)

    mutable struct boxed_struct
    end
    let x = boxed_struct()
        @test really_complicated_identity(x) === x
    end

    # Define two functions that each compute the address of a dedicated internal global variable.
    # The names of these globals are the same, so if their linkages are overwritten, then the
    # linker will merge the globals. Consequently, we can test that linkage is preserved by testing
    # that the addresses of the globals differ. The next few lines of code do just that.
    const the_other_ir1 = unsafe_string(ccall((:MakeLoadGlobalFunction, libllvmcalltest), Cstring, ()))
    const the_other_ir2 = unsafe_string(ccall((:MakeLoadGlobalFunction, libllvmcalltest), Cstring, ()))

    @eval global_value_address1() = llvmcall(($(the_other_ir1), "load_global_var"), Int64, Tuple{})
    @eval global_value_address2() = llvmcall(($(the_other_ir2), "load_global_var"), Int64, Tuple{})

    @test global_value_address1() != global_value_address2()
end

# issue 34166
f34166(x) = Base.llvmcall("ret i$(Sys.WORD_SIZE) %0", Int, (Int,), x)
@test_throws ErrorException f34166(1)
