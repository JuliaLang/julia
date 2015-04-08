using Base.llvmcall

function add1234(x::(Int32,Int32,Int32,Int32))
    llvmcall("""%3 = add <4 x i32> %1, %0
      ret <4 x i32> %3""",(Int32,Int32,Int32,Int32),
      ((Int32,Int32,Int32,Int32),(Int32,Int32,Int32,Int32)),
        (Int32(1),Int32(2),Int32(3),Int32(4)),
      x)
end

function add1234(x::NTuple{4,Int64})
    llvmcall("""%3 = add <4 x i64> %1, %0
      ret <4 x i64> %3""",NTuple{4,Int64},
      (NTuple{4,Int64},NTuple{4,Int64}),
        (Int64(1),Int64(2),Int64(3),Int64(4)),
      x)
end

@test add1234(map(Int32,(2,3,4,5))) === map(Int32,(3,5,7,9))
@test add1234(map(Int64,(2,3,4,5))) === map(Int64,(3,5,7,9))

# Test whether llvmcall escapes the function name correctly
baremodule PlusTest
    using Base.llvmcall
    using Base.Test
    using Base

    function +(x::Int32, y::Int32)
        llvmcall("""%3 = add i32 %1, %0
                    ret i32 %3""", Int32, (Int32, Int32), x, y)
    end
    @test Int32(1)+Int32(2)==Int32(3)
end
