# Test that various constructs support trimming
module Trimmability

using Sockets

world::String = "world!"
const str = OncePerProcess{String}() do
    return "Hello, " * world
end

abstract type Shape end
struct Square <: Shape
    side::Float64
end
struct Circle <: Shape
    radius::Float64
end
area(s::Square) = s.side^2
area(c::Circle) = pi*c.radius^2

sum_areas(v::Vector{Shape}) = sum(area, v)

mutable struct Foo; x::Int; end

# To check that objects embedded in emitted code are retained (kept[] == 0): we did not lose any roots
const kept = Base.RefValue{Int}(0)
# To check that some objects died (dropped[] > 0): GC + finalizers fired
const dropped = Base.RefValue{Int}(0)
note_kept(x::Foo) = (kept[] += 1; nothing)
note_dropped(x::Foo) = (dropped[] += 1; nothing)

const storage = Foo[]
function add_one(x::Cint)::Cint
    entry = Foo(x)
    finalizer(note_kept, entry)
    push!(storage, entry)
    return x + 1
end

let captured = Foo[]
    global @noinline function add_one_captured(x::Cint)::Cint
        entry = Foo(x)
        finalizer(note_kept, entry)
        push!(captured, entry)
        return x + 1
    end
end

const box = Base.RefValue{Any}(nothing)
@noinline function drop_one(x::Cint)
    entry = Foo(x)
    finalizer(note_dropped, entry)
    box[] = entry
    box[] = nothing
    return nothing
end

const fin_total = Base.RefValue{Int}(0)
const fin_log = Int[]

# Test various forms of fully de-virtualized / partially de-virtualized
# and inlineable / non-inlineable finalizers
fin_inline_local(x::Base.RefValue{Int}) = (fin_total[] += x[]; nothing)
fin_inline_escaping(x::Base.RefValue{Int}) = (fin_total[] += x[]; nothing)
@noinline fin_call_local(x::Base.RefValue{Int}) = (fin_total[] += x[]; nothing)
@noinline fin_call_escaping(x::Base.RefValue{Int}) = (fin_total[] += x[]; nothing)
fin_throws_escaping(x::Base.RefValue{Int}) = (push!(fin_log, x[]); nothing)

@noinline function local_finalizers()
    a = Base.RefValue{Int}(1)
    finalizer(fin_inline_local, a)
    b = Base.RefValue{Int}(2)
    finalizer(fin_call_local, b)
    return nothing
end

const escapee = Base.RefValue{Any}(nothing)
@noinline function escaping_finalizers()
    r = Base.RefValue{Int}(8)
    finalizer(fin_inline_escaping, r)
    escapee[] = r
    finalize(r)
    s = Base.RefValue{Int}(16)
    finalizer(fin_call_escaping, s)
    escapee[] = s
    finalize(s)
    t = Base.RefValue{Int}(32)
    finalizer(fin_throws_escaping, t)
    escapee[] = t
    finalize(t)
    escapee[] = nothing
    return nothing
end

@noinline function finalizer_check()
    local_finalizers()
    escaping_finalizers()
    total = 0
    for x in fin_log
        total += x
    end
    return (fin_total[], total)
end

# A `ccall` whose library is given by a runtime value makes the runtime call
# `Libdl.dlopen(lib)` on first use. A custom library type whose `dlopen` method is itself
# fully static keeps that resolvable under trimming; exercise it end to end so that the
# compiled path is actually taken at run time rather than merely verifying.
struct StaticLib end
const static_lib = StaticLib()
const static_lib_name = "libjulia"
Base.Libc.Libdl.dlopen(::StaticLib) =
    ccall(:jl_load_dynamic_library, Ptr{Cvoid}, (Ptr{UInt8}, UInt32, Cint),
          static_lib_name, Base.Libc.Libdl.RTLD_LAZY, Cint(0))
static_lib_ccall() = ccall((:jl_ver_major, static_lib), Cint, ())

# A `LazyLibrary`'s C on-load callback is invoked through a raw pointer, so nothing about
# whether it actually fires is visible to trim verification: an image can verify perfectly
# clean and still never call it. That is exactly the failure mode where a pointer-registered
# callback was appended to the roots vector instead of the vector the dispatcher iterates,
# silently dropping it. Count the firings so such a regression fails here at runtime.
const c_callback_count = Base.RefValue{Int}(0)
note_c_callback() = (c_callback_count[] += 1; nothing)

# `libjulia-internal` is already loaded in the trimmed program, so this `dlopen` picks up
# the existing handle rather than loading anything new.
const c_callback_lib = Base.Libc.Libdl.LazyLibrary("libjulia-internal";
    _on_load_c_callback = @cfunction(note_c_callback, Cvoid, ()))

function _test_cat()
    # hcat
    _cat1a = hcat(randn(3), rand(3), randn(3))
    _cat1b = [randn(3) rand(3) randn(3)]
    _cat1c = hcat(randn(3,3), rand(3,3), randn(3,3))
    _cat1d = [randn(3,3) rand(3,3) randn(3,3)]
    _cat1e = hcat(randn(3,3,3), rand(3,3,3), randn(3,3,3))
    _cat1f = [randn(3,3,3) rand(3,3,3) randn(3,3,3)]

    # v_cat
    _cat2a = vcat(randn(3), rand(3), randn(3))
    _cat2b = [randn(3); rand(3); randn(3)]
    _cat2c = vcat(randn(3,3), rand(3,3), randn(3,3))
    _cat2d = [randn(3,3); rand(3,3); randn(3,3)]
    _cat2e = vcat(randn(3,3,3), rand(3,3,3), randn(3,3,3))
    _cat2f = [randn(3,3,3); rand(3,3,3); randn(3,3,3)]

    # hvcat
    _cat3a = hvcat((2,2), rand(3,2), randn(3,4), rand(1,2), randn(1,4))
    _cat3b = [rand(3,2) randn(3,4); rand(1,2) randn(1,4)]
    _cat3c = hvcat((2, 2), rand(5,2,3), rand(5,4,3), rand(1,2,3), rand(1,4,3))
    _cat3d = [rand(5,2,3) rand(5,4,3); rand(1,2,3) rand(1,4,3)]

    # cat
    _cat4a = cat(randn(3), randn(3); dims = 1)
    _cat4b = cat(randn(3,3,3), randn(3,3,3); dims = 2)
    _cat4c = cat(randn(3), randn(3,3); dims = 2)
    _cat4d = cat(randn(3), randn(3), rand(3), rand(3), randn(3), randn(3); dims = (1,))
    _cat4e = cat(randn(3,3), randn(3,3), rand(3,3), rand(3,3), randn(3,3), randn(3,3); dims = (1,2))
    _cat4f = cat(randn(3,3,3), randn(3,3); dims=(1,3))

    # hvncat
    _cat5a = hvncat(2, randn(3), randn(3), randn(3))
    _cat5b = [randn(3) ;; randn(3) ;; randn(3)]
    _cat5c = hvncat(2, randn(3,3), randn(3,3), randn(3,3))
    _cat5d = [randn(3,3) ;; randn(3,3) ;; randn(3,3)]
    _cat5e = hvncat((1, 2, 2), false, randn(2,3), randn(2,3), randn(2,3), randn(2,3))
    _cat5f = [randn(2,3) ;; randn(2,3) ;;; randn(2,3) ;; randn(2,3)]

    # stack
    _cat6a = stack([randn(3), randn(3), randn(3)])
    _cat6b = stack([randn(3), randn(3), randn(3)]; dims=1)
    _cat6c = stack([randn(2,3), randn(2,3)]; dims=3)
    _cat6d = stack(x -> x .^ 2, [randn(3), randn(3)])

    # repeat
    _cat7a = repeat(randn(3), 2)
    _cat7b = repeat(randn(2,3), 2, 3)
    _cat7c = repeat(randn(2,3); inner=(2,1), outer=(1,3))
    _cat7d = repeat(randn(3,3,3), 1, 2, 1)

    # aggregate to prevent deletion
    _cat1 = _cat1a[1] + _cat1b[1] + _cat1c[1] + _cat1d[1] + _cat1e[1] + _cat1f[1]
    _cat2 = _cat2a[1] + _cat2b[1] + _cat2c[1] + _cat2d[1] + _cat2e[1] + _cat2f[1]
    _cat3 = _cat3a[1] + _cat3b[1] + _cat3c[1] + _cat3d[1]
    _cat4 = _cat4a[1] + _cat4b[1] + _cat4c[1] + _cat4d[1] + _cat4e[1] + _cat4f[1]
    _cat5 = _cat5a[1] + _cat5b[1] + _cat5c[1] + _cat5d[1] + _cat5e[1] + _cat5f[1]
    _cat6 = _cat6a[1] + _cat6b[1] + _cat6c[1] + _cat6d[1]
    _cat7 = _cat7a[1] + _cat7b[1] + _cat7c[1] + _cat7d[1]

    return _cat1 + _cat2 + _cat3 + _cat4 + _cat5 + _cat6 + _cat7
end


function @main(args::Vector{String})::Cint
    println(Core.stdout, str())
    println(Core.stdout, PROGRAM_FILE)
    foreach(x->println(Core.stdout, x), args)

    # test map/mapreduce; should work but relies on inlining and other optimizations
    # test that you can dispatch to some number of concrete cases
    println(Core.stdout, sum_areas(Shape[Circle(1), Square(2)]))

    arr = rand(10)
    sorted_arr = sort(arr)
    tot = sum(sorted_arr)
    tot = prod(sorted_arr)
    a = any(x -> x > 0, sorted_arr)
    b = all(x -> x >= 0, sorted_arr)
    c = map(x -> x^2, sorted_arr)
    d = mapreduce(x -> x^2, +, sorted_arr)
    # e = reduce(xor, rand(Int, 10))

    println(Core.stdout, _test_cat())
    println(Core.stdout, "Version: ", v"1.1")
    println(Core.stdout, "# preferences: ", length(Base.get_preferences()))

    for i = 1:10
        # https://github.com/JuliaLang/julia/issues/60846
        add_one(Cint(i))
        add_one_captured(Cint(i))
        drop_one(Cint(i))
        GC.gc()
    end
    GC.gc(true)

    let (counted, logged) = finalizer_check()
        println(Core.stdout, "finalizers: ", counted, " ", logged)
    end

    println(Core.stdout, "collected: ", kept[], " kept, ", dropped[], " dropped")

    # The callback pointer does not survive precompilation, so re-arm it once here before
    # the library is first loaded, per the `_on_load_c_callback` convention.
    @atomic c_callback_lib._on_load_c_callback = @cfunction(note_c_callback, Cvoid, ())
    Base.Libc.Libdl.dlopen(c_callback_lib)
    println(Core.stdout, "c_callback: ", c_callback_count[])

    try
        sock = connect("localhost", 4900)
        if isopen(sock)
            write(sock, "Hello")
            flush(sock)
            close(sock)
        end
    catch
    end

    println(Core.stdout, "static_lib_ccall: ", static_lib_ccall())

    Base.donotdelete(reshape([1,2,3],:,1,1))

    return 0
end

end
