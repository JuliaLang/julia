# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.llvmcall

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
    using Base.llvmcall
    using Base.Test
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
        ("""declare double @llvm.floor.f64(double)""",
         """%2 = call double @llvm.floor.f64(double %0)
            ret double %2"""),
    Float64, Tuple{Float64}, x)
end
@test declared_floor(4.2) ≈ 4.
ir = sprint(io->code_llvm(io, declared_floor, Tuple{Float64}))
@test contains(ir, "call double @llvm.floor.f64") # should be inlined

function doubly_declared_floor(x::Float64)
    llvmcall(
        ("""declare double @llvm.floor.f64(double)""",
         """%2 = call double @llvm.floor.f64(double %0)
            ret double %2"""),
    Float64, Tuple{Float64}, x+1)-1
end
@test doubly_declared_floor(4.2) ≈ 4.

function doubly_declared2_trunc(x::Float64)
    a = llvmcall(
        ("""declare double @llvm.trunc.f64(double)""",
         """%2 = call double @llvm.trunc.f64(double %0)
            ret double %2"""),
    Float64, Tuple{Float64}, x)
    b = llvmcall(
        ("""declare double @llvm.trunc.f64(double)""",
         """%2 = call double @llvm.trunc.f64(double %0)
            ret double %2"""),
    Float64, Tuple{Float64}, x+1)-1
    a + b
end
@test doubly_declared2_trunc(4.2) ≈ 8.

# Test for single line
function declared_ceil(x::Float64)
    llvmcall(
        ("declare double @llvm.ceil.f64(double)",
         """%2 = call double @llvm.ceil.f64(double %0)
            ret double %2"""),
    Float64, Tuple{Float64}, x)
end
@test declared_ceil(4.2) ≈ 5.0

# Test for multiple lines
function ceilfloor(x::Float64)
    llvmcall(
        ("""declare double @llvm.ceil.f64(double)
            declare double @llvm.floor.f64(double)""",
         """%2 = call double @llvm.ceil.f64(double %0)
            %3 = call double @llvm.floor.f64(double %2)
            ret double %3"""),
    Float64, Tuple{Float64}, x)
end
@test ceilfloor(7.4) ≈ 8.0

# Test for proper declaration extraction
function confuse_declname_parsing()
    llvmcall(
        ("""declare i64 addrspace(0)* @foobar()""",
         """ret void"""),
    Void, Tuple{})
end
confuse_declname_parsing()

# Test for proper mangling of external (C) functions
function call_jl_errno()
    llvmcall(
    (""" declare i32 @jl_errno()""",
    """
    %r = call i32 @jl_errno()
    ret i32 %r
    """),Int32,Tuple{})
end
call_jl_errno()

module ObjLoadTest
    using Base: Test, llvmcall, @ccallable
    didcall = false
    @ccallable Void function jl_the_callback()
        global didcall
        didcall = true
        nothing
    end
    # Make sure everything up until here gets compiled
    jl_the_callback(); didcall = false
    function do_the_call()
        llvmcall(
        (""" declare void @jl_the_callback()""",
        """
        call void @jl_the_callback()
        ret void
        """),Void,Tuple{})
    end
    do_the_call()
    @test didcall
end

# Test for proper parenting
if VersionNumber(Base.libllvm_version) >= v"3.6" # llvm 3.6 changed the syntax for a gep, so just ignore this test on older versions
    local foo
    function foo()
        # this IR snippet triggers an optimization relying
        # on the llvmcall function having a parent module
        Base.llvmcall(
         """%1 = getelementptr i64, i64* null, i64 1
            ret void""",
        Void, Tuple{})
    end
    code_llvm(DevNull, foo, ())
else
    println("INFO: skipping gep parentage test on llvm < 3.6")
end
