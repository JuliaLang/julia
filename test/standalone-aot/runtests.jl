include("IRGen.jl")
using .IRGen

using Test

# Various tests
using LLVM
llvmmod(native_code) =
    LLVM.Module(ccall(:jl_get_llvm_module, LLVM.API.LLVMModuleRef,
                      (Ptr{Cvoid},), native_code.p))

pkgdir = @__DIR__
bindir = string(Sys.BINDIR, "/../tools")


GC.enable(false)
dump_native(irgen(rand, Tuple{}), "librand.o")
run(`$bindir/clang -shared -fpic librand.o -o librand.so -L$bindir/../lib -ljulia-debug -ldSFMT`)
run(`$bindir/clang -shared -fpic librand.o -o librand.wasm -L$bindir/../lib -ljulia-debug -ldSFMT`)
ccall((:init_lib, "./librand.so"), Cvoid, ()) 
@show ccall((:rand, "./librand.so"), Float64, ()) 
@show ccall((:rand, "./librand.so"), Float64, ()) 
@show ccall((:rand, "./librand.so"), Float64, ()) 
GC.enable(true)

using Dates
fdate(x) = Dates.days(Dates.DateTime(2016, x, 1))
native = irgen(fdate, Tuple{Int})
@show @jlrun fdate(3)
@test fdate(3) == @jlrun fdate(3)

mutable struct AAA
    aaa::Int
    bbb::Int
end
@noinline ssum(x) = x.aaa + x.bbb
fstruct(x) = ssum(AAA(x, 99))
@test fstruct(10) == @jlrun fstruct(10)

module ZZ
mutable struct AAA
    aaa::Int
    bbb::Int
end
@noinline ssum(x) = x.aaa + x.bbb
fstruct(x) = ssum(AAA(x, 99))
end # module
ffstruct(x) = ZZ.fstruct(x)
@test ffstruct(10) == @jlrun ffstruct(10)

twox(x) = 2x
dump_native(irgen(twox, Tuple{Float64}), "libtwox.o")
# run(`$bindir/clang -emit-llvm --target=wasm32-shared -fpic libtwox.o -o libtwox.wasm -L$bindir/../lib -ljulia-debug -ldSFMT`)
@test twox(10) == @jlrun twox(10)

fmap(x) = sum(map(twox, [1, x]))
@test fmap(10) == @jlrun fmap(10)

hellostr() = "hellllllo world!"
@test hellostr() == @jlrun hellostr()

fint() = UInt32
@test fint() == @jlrun fint()

const a = Ref(0x80808080)
jglobal() = a
@show b = @jlrun jglobal()
# @test jglobal()[] == b[]      # Something's broken with mutable's

arraysum(x) = sum([x, 1])
@test arraysum(6) == @jlrun arraysum(6)

fsin(x) = sin(x)
@test fsin(0.5) == @jlrun fsin(0.5)

fccall() = ccall(:jl_ver_major, Cint, ())
@test fccall() == @jlrun fccall()

fcglobal() = cglobal(:jl_n_threads, Cint)
@test fcglobal() == @jlrun fcglobal()

many() = ("jkljkljkl", :jkljkljkljkl, :asdfasdf, "asdfasdfasdf")
## @jlrun doesn't work with this method.
## Here, ccall needs an Any return type, not the tuple type deduced by @jlrun.
# @show @jlrun many()
native = irgen(many, Tuple{})
dump_native(native, "libmany.o")
run(`$bindir/clang -shared -fpic libmany.o -o libmany.so -L$bindir/../lib -ljulia-debug`)
ccall((:init_lib, "./libmany.so"), Cvoid, ()) 
@test many() == ccall((:many, "./libmany.so"), Any, ()) 

const sv = Core.svec(1,2,3,4)
fsv() = sv
@test fsv() == @jlrun fsv()

const arr = [9,9,9,9]
farray() = arr
@show @jlrun farray()
@show farray()
@test farray() == @jlrun farray()

@noinline f(x) = 3x
@noinline fop(f, x) = 2f(x)
funcall(x) = fop(f, x)

@test funcall(2) == @jlrun funcall(2)

hi() = print(Core.stdout, 'X')
@jlrun hi()

hello() = print(Core.stdout, "Hello world...\n")
@jlrun hello()

printint() = print(Core.stdout, 123456)
@jlrun printint()

function fdict(x)
    d = Dict(:a => 2x)
    return 2*d[:a]
end
@test fdict(3) == @jlrun fdict(3)

using LinearAlgebra
function flu(x)
    A = Float64[x 3.3; 6.0 3.0]
    F = lu(A)
    return F.U[1,1]
end
@test flu(3.3) == @jlrun flu(3.3)

# # Exception: fatal error in type inference (type bound) 
# hello2() = print(Core.stdout, 'Z', "Hello world...\n")
# GC.enable(false)
# @jlrun hello2()
# GC.enable(true)

# # Odd error
# printfloat() = print(Core.stdout, 123.456)
# @jlrun printfloat()


nothing

