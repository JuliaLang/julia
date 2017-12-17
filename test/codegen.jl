# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for codegen and optimizations

const opt_level = Base.JLOptions().opt_level
const coverage = (Base.JLOptions().code_coverage > 0) || (Base.JLOptions().malloc_log > 0)
const Iptr = sizeof(Int) == 8 ? "i64" : "i32"

# `_dump_function` might be more efficient but it doesn't really matter here...
get_llvm(@nospecialize(f), @nospecialize(t), strip_ir_metadata=true, dump_module=false) =
    sprint(code_llvm, f, t, strip_ir_metadata, dump_module)

get_llvm_noopt(@nospecialize(f), @nospecialize(t), strip_ir_metadata=true, dump_module=false) =
    Base._dump_function(f, t,
                #=native=# false, #=wrapper=# false, #=strip=# strip_ir_metadata,
                #=dump_module=# dump_module, #=syntax=#:att, #=optimize=#false)


if opt_level > 0
    # Make sure getptls call is removed at IR level with optimization on
    @test !contains(get_llvm(identity, Tuple{String}), " call ")
end

jl_string_ptr(s::String) = ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), s)
core_sizeof(o) = Core.sizeof(o)
function test_loads_no_call(ir, load_types)
    in_function = false
    load_idx = 1
    for line in eachline(IOBuffer(ir))
        if !in_function
            if startswith(line, "define ")
                in_function = true
            end
            continue
        end
        @test !contains(line, " call ")
        load_split = split(line, " load ", limit=2)
        if !coverage && length(load_split) >= 2
            @test load_idx <= length(load_types)
            if load_idx <= length(load_types)
                @test startswith(load_split[2], "$(load_types[load_idx]),")
            end
            load_idx += 1
        end
        if startswith(line, "}")
            break
        end
    end
    if !coverage
        @test load_idx == length(load_types) + 1
    end
end

# This function tests if functions are output when compiled if jl_dump_compiles is enabled.
# Have to go through pains with recursive function (eval probably not required) to make sure
# that inlining won't happen.
function test_jl_dump_compiles()
    tfile = tempname()
    io = open(tfile, "w")
    @eval(test_jl_dump_compiles_internal(x) = x)
    ccall(:jl_dump_compiles, Void, (Ptr{Void},), io.handle)
    @eval test_jl_dump_compiles_internal(1)
    ccall(:jl_dump_compiles, Void, (Ptr{Void},), C_NULL)
    close(io)
    tstats = stat(tfile)
    tempty = tstats.size == 0
    rm(tfile)
    @test tempty == false
end

# This function tests if a toplevel thunk is output if jl_dump_compiles is enabled.
# The eval statement creates the toplevel thunk.
function test_jl_dump_compiles_toplevel_thunks()
    tfile = tempname()
    io = open(tfile, "w")
    topthunk = Meta.lower(Main, :(for i in 1:10; end))
    ccall(:jl_dump_compiles, Void, (Ptr{Void},), io.handle)
    Core.eval(Main, topthunk)
    ccall(:jl_dump_compiles, Void, (Ptr{Void},), C_NULL)
    close(io)
    tstats = stat(tfile)
    tempty = tstats.size == 0
    rm(tfile)
    @test tempty == true
end

if opt_level > 0
    # Make sure `jl_string_ptr` is inlined
    @test !contains(get_llvm(jl_string_ptr, Tuple{String}), " call ")
    s = "aaa"
    @test jl_string_ptr(s) == pointer_from_objref(s) + sizeof(Int)
    # String
    test_loads_no_call(get_llvm(core_sizeof, Tuple{String}), [Iptr])
    # String
    test_loads_no_call(get_llvm(core_sizeof, Tuple{SimpleVector}), [Iptr])
    # Array
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Vector{Int}}), [Iptr])
    # As long as the eltype is known we don't need to load the elsize
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Array{Any}}), [Iptr])
    # Check that we load the elsize
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Vector}), [Iptr, "i16"])

    test_jl_dump_compiles()
    test_jl_dump_compiles_toplevel_thunks()
end

# Make sure we will not elide the allocation
@noinline create_ref1() = Ref(1)
function pointer_not_safepoint()
    a = create_ref1()
    unsafe_store!(Ptr{Int}(pointer_from_objref(a)), 3)
    return a[]
end
@test pointer_not_safepoint() == 3

# The current memcmp threshold is 512bytes, make sure this struct has the same size on
# 32bits and 64bits
struct LargeStruct
    x::NTuple{1024,Int8}
    LargeStruct() = new()
end

const large_struct = LargeStruct()
@noinline create_ref_struct() = Ref(large_struct)
function compare_large_struct(a)
    b = create_ref_struct()
    if a[] === b[]
        b[].x[1]
    else
        a[].x[2]
    end
end

mutable struct MutableStruct
    a::Int
    MutableStruct() = new()
end

breakpoint_mutable(a::MutableStruct) = ccall(:jl_breakpoint, Void, (Ref{MutableStruct},), a)

# Allocation with uninitialized field as gcroot
mutable struct BadRef
    x::MutableStruct
    y::MutableStruct
    BadRef(x) = new(x)
end
Base.cconvert(::Type{Ptr{BadRef}}, a::MutableStruct) = BadRef(a)
Base.unsafe_convert(::Type{Ptr{BadRef}}, ar::BadRef) = Ptr{BadRef}(pointer_from_objref(ar.x))

breakpoint_badref(a::MutableStruct) = ccall(:jl_breakpoint, Void, (Ptr{BadRef},), a)

struct PtrStruct
    a::Ptr{Void}
    b::Int
end

mutable struct RealStruct
    a::Float64
    b::Int
end

function Base.cconvert(::Type{Ref{PtrStruct}}, a::RealStruct)
    (a, Ref(PtrStruct(pointer_from_objref(a), a.b)))
end
Base.unsafe_convert(::Type{Ref{PtrStruct}}, at::Tuple) =
    Base.unsafe_convert(Ref{PtrStruct}, at[2])

breakpoint_ptrstruct(a::RealStruct) =
    ccall(:jl_breakpoint, Void, (Ref{PtrStruct},), a)

if opt_level > 0
    @test !contains(get_llvm(pointer_not_safepoint, Tuple{}), "%gcframe")
    compare_large_struct_ir = get_llvm(compare_large_struct, Tuple{typeof(create_ref_struct())})
    @test contains(compare_large_struct_ir, "call i32 @memcmp")
    @test !contains(compare_large_struct_ir, "%gcframe")

    @test contains(get_llvm(MutableStruct, Tuple{}), "jl_gc_pool_alloc")
    breakpoint_mutable_ir = get_llvm(breakpoint_mutable, Tuple{MutableStruct})
    @test !contains(breakpoint_mutable_ir, "%gcframe")
    @test !contains(breakpoint_mutable_ir, "jl_gc_pool_alloc")

    breakpoint_badref_ir = get_llvm(breakpoint_badref, Tuple{MutableStruct})
    @test !contains(breakpoint_badref_ir, "%gcframe")
    @test !contains(breakpoint_badref_ir, "jl_gc_pool_alloc")

    breakpoint_ptrstruct_ir = get_llvm(breakpoint_ptrstruct, Tuple{RealStruct})
    @test !contains(breakpoint_ptrstruct_ir, "%gcframe")
    @test !contains(breakpoint_ptrstruct_ir, "jl_gc_pool_alloc")
end

function two_breakpoint(a::Float64)
    ccall(:jl_breakpoint, Void, (Ref{Float64},), a)
    ccall(:jl_breakpoint, Void, (Ref{Float64},), a)
end

function load_dummy_ref(x::Int)
    r = Ref{Int}(x)
    Base.@gc_preserve r begin
        unsafe_load(Ptr{Int}(pointer_from_objref(r)))
    end
end

if opt_level > 0
    breakpoint_f64_ir = get_llvm((a)->ccall(:jl_breakpoint, Void, (Ref{Float64},), a),
                                 Tuple{Float64})
    @test !contains(breakpoint_f64_ir, "jl_gc_pool_alloc")
    breakpoint_any_ir = get_llvm((a)->ccall(:jl_breakpoint, Void, (Ref{Any},), a),
                                 Tuple{Float64})
    @test contains(breakpoint_any_ir, "jl_gc_pool_alloc")
    two_breakpoint_ir = get_llvm(two_breakpoint, Tuple{Float64})
    @test !contains(two_breakpoint_ir, "jl_gc_pool_alloc")
    @test contains(two_breakpoint_ir, "llvm.lifetime.end")

    @test load_dummy_ref(1234) === 1234
    load_dummy_ref_ir = get_llvm(load_dummy_ref, Tuple{Int})
    @test !contains(load_dummy_ref_ir, "jl_gc_pool_alloc")
    # Hopefully this is reliable enough. LLVM should be able to optimize this to a direct return.
    @test contains(load_dummy_ref_ir, "ret $Iptr %0")
end

# Issue 22770
let was_gced = false
    @noinline make_tuple(x) = tuple(x)
    @noinline use(x) = ccall(:jl_breakpoint, Void, ())
    @noinline assert_not_gced() = @test !was_gced

    function foo22770()
        b = Ref(2)
        finalizer(x -> was_gced = true, b)
        y = make_tuple(b)
        x = y[1]
        a = Ref(1)
        use(x); use(a); use(y)
        c = Ref(3)
        gc()
        assert_not_gced()
        use(x)
        use(c)
    end
    foo22770()
    gc()
    @test was_gced
end

function egal_svecs()
    a = Core.svec(:a, :b)
    b = Core.svec(:a, :b)
    a === b
end
@test egal_svecs()
@test Core.svec(:a, :b) === Core.svec(:a, :b)

# issue #22582
function issue22582!(a::AbstractArray, b)
    len = length(a)
    if b
        ccall(:jl_array_grow_end, Void, (Any, Csize_t), a, 1)
    end
    return len
end
let c = [1,2,3]
    len1 = length(c)
    len2 = issue22582!(c, true)
    @test len1 == len2
end

# PR #23595
@generated f23595(g, args...) = Expr(:call, :g, Expr(:(...), :args))
x23595 = rand(1)
@test f23595(Core.arrayref, true, x23595, 1) == x23595[]

# Issue #22421
@noinline f22421_1(x) = x[] + 1
@noinline f22421_2(x) = x[] + 2
@noinline f22421_3(x, y, z, v) = x[] + y[] + z[] + v
function g22421_1(x, y, b)
    # Most likely generates a branch with phi node
    if b
        z = x
        v = f22421_1(y)
    else
        z = y
        v = f22421_2(x)
    end
    return f22421_3(x, y, z, v)
end
function g22421_2(x, y, b)
    # Most likely generates a select
    return f22421_3(x, y, b ? x : y, 1)
end

struct A24108
    x::Vector{Int}
end
struct B24108
    x::A24108
end
@noinline f24108(x) = length(x)
# Test no gcframe is allocated for `x.x.x` even though `x.x` isn't live at the call site
g24108(x::B24108) = f24108(x.x.x)

@test g22421_1(Ref(1), Ref(2), true) === 7
@test g22421_1(Ref(3), Ref(4), false) === 16
@test g22421_2(Ref(5), Ref(6), true) === 17
@test g22421_2(Ref(7), Ref(8), false) === 24

if opt_level > 0
    @test !contains(get_llvm(g22421_1, Tuple{Base.RefValue{Int},Base.RefValue{Int},Bool}),
                    "%gcframe")
    @test !contains(get_llvm(g22421_2, Tuple{Base.RefValue{Int},Base.RefValue{Int},Bool}),
                    "%gcframe")
    @test !contains(get_llvm(g24108, Tuple{B24108}), "%gcframe")
end

# Issue 24632
function foo24632(y::Bool)
    x::Union{Int, String} = y ? "ABC" : 1
    isa(x, Int) ? x : 0
end

# A bit coarse-grained perhaps, but ok for now. What we want to check is that the load from
# the semi-boxed union on the if-branch of the `isa` is not annotated !nonnull
@test contains(get_llvm_noopt(foo24632, (Bool,), false), "!dereferenceable_or_null")
@test !contains(get_llvm_noopt(foo24632, (Bool,), false), "!nonnull")
