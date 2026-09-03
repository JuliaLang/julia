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

#Since LLVM 18, LLVM makes a best effort to automatically include the intrinsics
function undeclared_ceil(x::Float64)
    llvmcall("""%2 = call double @llvm.ceil.f64(double %0)
        ret double %2""", Float64, Tuple{Float64}, x)
end
@test undeclared_ceil(4.2) == 5.0
@test undeclared_ceil(4.2) == 5.0

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

# Test for proper parenting
begin
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
end

# Issue #48093 - test that non-external globals are not deduplicated
function kernel()
    Base.llvmcall(("""
        @shmem = internal global i8 0, align 8
        define void @entry() {
            store i8 1, i8* @shmem
            ret void
        }""", "entry"), Cvoid, Tuple{})
    Base.llvmcall(("""
        @shmem = internal global i8 0, align 8
        define i8 @entry() {
            %1 = load i8, i8* @shmem
            ret i8 %1
        }""", "entry"), UInt8, Tuple{})
end
@test kernel() == 0x00

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

# Test that codegen can construct constant LLVMPtr #38864
struct MyStruct
    kern::UInt64
    ptr::Core.LLVMPtr{UInt8,1}
end
MyStruct(kern) = MyStruct(kern, reinterpret(Core.LLVMPtr{UInt8,1}, 0))
MyStruct() = MyStruct(0)
s = MyStruct()

# ensure LLVMPtr properly subtypes
@test eltype(supertype(Core.LLVMPtr{UInt8,1})) <: UInt8
@test s.kern == 0
@test reinterpret(Int, s.ptr) == 0

function too_few_args(x::Int32, y::Int32)
    llvmcall("""%3 = add i32 %1, %0
                ret i32 %3""",
        Int32,
        Tuple{Int32, Int32},
        x)
end
@test_throws ErrorException too_few_args(Int32(1), Int32(1))

function too_many_args(x::Int32, y::Int32)
    llvmcall("""%3 = add i32 %1, %0
                ret i32 %3""",
        Int32,
        Tuple{Int32, Int32},
        x,y,x)
end
@test_throws ErrorException too_many_args(Int32(1), Int32(1))

llvmcall_nothing_arg() = Core.Intrinsics.llvmcall("ret i8 0", Int8, Tuple{Nothing}, nothing)
@test_throws ErrorException llvmcall_nothing_arg()

# Intrinsics that belong to a different target cannot be selected by the host back-end.
# LLVM reports that as a fatal error (`LLVM ERROR: Cannot select: intrinsic ...`) and
# aborts the process, so such code must be rejected during codegen instead. This matters
# for ahead-of-time compilation (`--output-o`, PackageCompiler.jl, juliac), which compiles
# every concretely-typed method whether or not it is ever called: GPU packages define
# methods whose bodies are only valid on the device (JuliaGPU/GPUCompiler.jl#611).
@testset "intrinsics of another target" begin
    script = """
        f_nvvm() = ccall("llvm.nvvm.membar.cta", llvmcall, Cvoid, ())
        g_nvvm() = Base.llvmcall((\"""
            declare i32 @llvm.nvvm.read.ptx.sreg.tid.x()
            define i32 @entry() {
                %r = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
                ret i32 %r
            }\""", "entry"), Int32, Tuple{})
        h_amdgcn() = ccall("llvm.amdgcn.s.barrier", llvmcall, Cvoid, ())
        for f in (f_nvvm, g_nvvm, h_amdgcn)
            try
                f()
                exit(1)  # ran to completion
            catch err
                err isa ErrorException || exit(2)
                occursin("not available", err.msg) || exit(3)
            end
        end
        exit(0)
        """
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $script`
    @test success(pipeline(cmd; stderr=devnull))
end

# The opposite direction: intrinsics of the host target must still be accepted, both as
# `ccall(..., llvmcall, ...)` and declared in a `Base.llvmcall` module. Only intrinsics
# that do not require an ISA extension are used, so that this holds on any CPU of the target.
@testset "intrinsics of the host target" begin
    @static if Sys.ARCH === :x86_64 || Sys.ARCH === :i686
        host_ccall() = ccall("llvm.x86.sse2.pause", llvmcall, Cvoid, ())
        host_ir() = Base.llvmcall(("""
            declare void @llvm.x86.sse2.pause()
            define void @entry() {
                call void @llvm.x86.sse2.pause()
                ret void
            }""", "entry"), Cvoid, Tuple{})
    elseif Sys.ARCH === :aarch64
        host_ccall() = ccall("llvm.aarch64.isb", llvmcall, Cvoid, (Int32,), Int32(15))
        host_ir() = Base.llvmcall(("""
            declare void @llvm.aarch64.isb(i32)
            define void @entry() {
                call void @llvm.aarch64.isb(i32 15)
                ret void
            }""", "entry"), Cvoid, Tuple{})
    elseif Sys.ARCH === :armv7l || Sys.ARCH === :armv6l
        host_ccall() = ccall("llvm.arm.hint", llvmcall, Cvoid, (Int32,), Int32(0))
        host_ir() = Base.llvmcall(("""
            declare void @llvm.arm.hint(i32)
            define void @entry() {
                call void @llvm.arm.hint(i32 0)
                ret void
            }""", "entry"), Cvoid, Tuple{})
    elseif Sys.ARCH === :powerpc64le
        host_ccall() = ccall("llvm.ppc.lwsync", llvmcall, Cvoid, ())
        host_ir() = Base.llvmcall(("""
            declare void @llvm.ppc.lwsync()
            define void @entry() {
                call void @llvm.ppc.lwsync()
                ret void
            }""", "entry"), Cvoid, Tuple{})
    else
        # riscv64: every `llvm.riscv.*` intrinsic requires an ISA extension (e.g.
        # `llvm.riscv.pause` needs Zihintpause, which `generic-rv64` lacks).
        host_ccall() = nothing
        host_ir() = nothing
    end
    @test host_ccall() === nothing
    @test host_ir() === nothing
end
