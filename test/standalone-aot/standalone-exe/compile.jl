include("../IRGen.jl")
using .IRGen

twox(x) = 2x
arraysum(x) = sum([x, 1])
myrand() = rand()

funcs = [
    "twox" => (twox, Tuple{Int}),
    "arraysum" => (arraysum, Tuple{Int}),
    "myrand" => (myrand, Tuple{}),
]

dir = @__DIR__
bindir = string(Sys.BINDIR, "/../tools")

for (fname, (func, sig)) in funcs
    native = irgen(func, sig)
    dump_native(native, "lib$fname.o")
    run(`$bindir/clang -shared -fpic lib$fname.o -o lib$fname.so`)
    run(`$bindir/clang -c -std=gnu99 -I'../../../usr/include/julia' -DJULIA_ENABLE_THREADING=1 -fPIC $fname.c`)
    run(`$bindir/clang -o $fname $fname.o -L$dir -L$dir/../../../usr/lib -Wl,--unresolved-symbols=ignore-in-object-files -Wl,-rpath,'.' -Wl,-rpath,'../../../usr/lib' -ljulia-debug -ldSFMT -l$fname`)
end
