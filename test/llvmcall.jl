using Base.llvmcall

function add1234(x::(Int32,Int32,Int32,Int32))
    llvmcall("""%3 = add <4 x i32> %1, %0
      ret <4 x i32> %3""",(Int32,Int32,Int32,Int32),
      ((Int32,Int32,Int32,Int32),(Int32,Int32,Int32,Int32)),
        (int32(1),int32(2),int32(3),int32(4)),
      x)
end

function add1234(x::NTuple{4,Int64})
    llvmcall("""%3 = add <4 x i64> %1, %0
      ret <4 x i64> %3""",NTuple{4,Int64},
      (NTuple{4,Int64},NTuple{4,Int64}),
        (int64(1),int64(2),int64(3),int64(4)),
      x)
end

@test add1234(map(int32,(2,3,4,5))) === map(int32,(3,5,7,9))
@test add1234(map(int64,(2,3,4,5))) === map(int64,(3,5,7,9))

# Test whether llvmcall escapes the function name correctly
baremodule PlusTest
    using Base.llvmcall
    using Base.Test
    using Base

    function +(x::Int32, y::Int32)
        llvmcall("""%3 = add i32 %1, %0
                    ret i32 %3""", Int32, (Int32, Int32), x, y)
    end
    @test int32(1)+int32(2)==int32(3)
end

# Test whether declarations work properly
function undeclared_ceil(x::Float64)
    llvmcall("""%2 = call double @llvm.ceil.f64(double %0)
        ret double %2""", Float64, (Float64,), x)
end
@test_throws ErrorException undeclared_ceil(4.2)
# KNOWN ISSUE: although the llvmcall in undeclare_ceil did error(), the LLVM
# module contains the generated IR referencing the ceil intrinsic. At some
# point, a declaration for that intrinsic is added, breaking any future
# llvmcall((conflicting declaration, ...), ...). Not an issue in LLVM 3.5+.
function declared_floor(x::Float64)
    llvmcall(
        ("""declare double @llvm.floor.f64(double)""",
         """%2 = call double @llvm.floor.f64(double %0)
            ret double %2"""),
    Float64, (Float64,), x)
end
@test_approx_eq declared_floor(4.2) 4.
function doubly_declared_floor(x::Float64)
    llvmcall(
        ("""declare double @llvm.floor.f64(double)""",
         """%2 = call double @llvm.floor.f64(double %0)
            ret double %2"""),
    Float64, (Float64,), x+1)-1
end
@test_approx_eq doubly_declared_floor(4.2) 4.
function doubly_declared2_trunc(x::Float64)
    a = llvmcall(
        ("""declare double @llvm.trunc.f64(double)""",
         """%2 = call double @llvm.trunc.f64(double %0)
            ret double %2"""),
    Float64, (Float64,), x)
    b = llvmcall(
        ("""declare double @llvm.trunc.f64(double)""",
         """%2 = call double @llvm.trunc.f64(double %0)
            ret double %2"""),
    Float64, (Float64,), x+1)-1
    a + b
end
@test_approx_eq doubly_declared2_trunc(4.2) 8.
