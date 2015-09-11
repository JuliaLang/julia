# This file is a part of Julia. License is MIT: http://julialang.org/license

using Core.Intrinsics.llvmcall

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
@test eval(Expr(:call, Core.Intrinsics.llvmcall,
    """%3 = add i32 %1, %0
       ret i32 %3""", Int32, Tuple{Int32, Int32},
        Int32(1), Int32(2))) == 3

@test "llvmcall: argument value didn't match declared type" ==
    @test_throws(ErrorException, llvmcall("ret i8 %0", Int8, Tuple{Int8}, 1)).msg

@test "llvmcall: argument value didn't match declared type" ==
    @test_throws(ErrorException, llvmcall("ret i8* %0", Int8, Tuple{Ptr{Any}}, C_NULL)).msg

invalid_llvmasm_arg1() = llvmcall("ret jl_value_t* %0", Int8, Tuple{Any}, 1)
@test startswith(@test_throws(ErrorException, code_llvm(invalid_llvmasm_arg1, Tuple{})).msg,
    "error compiling invalid_llvmasm_arg1: Failed to parse LLVM Assembly: ")

invalid_llvmasm_arg2() = llvmcall("ret i8* %0", Int8, Tuple{Any}, 1)
@test startswith(@test_throws(ErrorException, code_llvm(invalid_llvmasm_arg2, Tuple{})).msg,
    "error compiling invalid_llvmasm_arg2: Failed to parse LLVM Assembly: ")

invalid_llvmasm_arg3() = llvmcall("ret i8 %0", Int8, Tuple{Int}, 1)
@test startswith(@test_throws(ErrorException, code_llvm(invalid_llvmasm_arg3, Tuple{})).msg,
    "error compiling invalid_llvmasm_arg3: Failed to parse LLVM Assembly: ")

invalid_llvmasm_ret() = llvmcall("ret i8 %0", Int32, Tuple{Int8}, Int8(1))
@test startswith(@test_throws(ErrorException, code_llvm(invalid_llvmasm_ret, Tuple{})).msg,
    "error compiling invalid_llvmasm_ret: Failed to parse LLVM Assembly: ")

invalid_llvmasm() = llvmcall("", Int32, Tuple{Int8}, Int8(1))
@test startswith(@test_throws(ErrorException, code_llvm(invalid_llvmasm, Tuple{})).msg,
    "error compiling invalid_llvmasm: Failed to parse LLVM Assembly: ")
