# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for codegen and optimizations

using Random
using InteractiveUtils

const opt_level = Base.JLOptions().opt_level
const coverage = (Base.JLOptions().code_coverage > 0) || (Base.JLOptions().malloc_log > 0)
const Iptr = sizeof(Int) == 8 ? "i64" : "i32"

# `_dump_function` might be more efficient but it doesn't really matter here...
get_llvm(@nospecialize(f), @nospecialize(t), strip_ir_metadata=true, dump_module=false) =
    sprint(code_llvm, f, t, strip_ir_metadata, dump_module)

get_llvm_noopt(@nospecialize(f), @nospecialize(t), strip_ir_metadata=true, dump_module=false) =
    InteractiveUtils._dump_function(f, t,
                #=native=# false, #=wrapper=# false, #=strip=# strip_ir_metadata,
                #=dump_module=# dump_module, #=syntax=#:att, #=optimize=#false)


if opt_level > 0
    # Make sure getptls call is removed at IR level with optimization on
    @test !occursin(" call ", get_llvm(identity, Tuple{String}))
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
        @test !occursin(" call ", line)
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
    ccall(:jl_dump_compiles, Cvoid, (Ptr{Cvoid},), io.handle)
    @eval test_jl_dump_compiles_internal(1)
    ccall(:jl_dump_compiles, Cvoid, (Ptr{Cvoid},), C_NULL)
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
    # Make sure to cause compilation of the eval function
    # before calling it below.
    Core.eval(Main, Any[:(nothing)][1])
    topthunk = Meta.lower(Main, :(for i in 1:10; end))
    ccall(:jl_dump_compiles, Cvoid, (Ptr{Cvoid},), io.handle)
    Core.eval(Main, topthunk)
    ccall(:jl_dump_compiles, Cvoid, (Ptr{Cvoid},), C_NULL)
    close(io)
    tstats = stat(tfile)
    tempty = tstats.size == 0
    rm(tfile)
    @test tempty == true
end

if opt_level > 0
    # Make sure `jl_string_ptr` is inlined
    @test !occursin(" call ", get_llvm(jl_string_ptr, Tuple{String}))
    # Make sure `Core.sizeof` call is inlined
    s = "aaa"
    @test jl_string_ptr(s) == pointer_from_objref(s) + sizeof(Int)
    # String
    test_loads_no_call(get_llvm(core_sizeof, Tuple{String}), [Iptr])
    # String
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Core.SimpleVector}), [Iptr])
    # Array
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Vector{Int}}), [Iptr])
    # As long as the eltype is known we don't need to load the elsize
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Array{Any}}), [Iptr])
    # Check that we load the elsize
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Vector}), [Iptr, "i16"])
    # Primitive Type size should be folded to a constant
    test_loads_no_call(get_llvm(core_sizeof, Tuple{Ptr}), String[])

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

breakpoint_mutable(a::MutableStruct) = ccall(:jl_breakpoint, Cvoid, (Ref{MutableStruct},), a)

# Allocation with uninitialized field as gcroot
mutable struct BadRef
    x::MutableStruct
    y::MutableStruct
    BadRef(x) = new(x)
end
Base.cconvert(::Type{Ptr{BadRef}}, a::MutableStruct) = BadRef(a)
Base.unsafe_convert(::Type{Ptr{BadRef}}, ar::BadRef) = Ptr{BadRef}(pointer_from_objref(ar.x))

breakpoint_badref(a::MutableStruct) = ccall(:jl_breakpoint, Cvoid, (Ptr{BadRef},), a)

struct PtrStruct
    a::Ptr{Cvoid}
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
    ccall(:jl_breakpoint, Cvoid, (Ref{PtrStruct},), a)

@noinline r_typeassert(c) = c ? (1,1) : nothing
function f_typeassert(c)
    r_typeassert(c)::Tuple
end
@test !occursin("jl_subtype", get_llvm(f_typeassert, Tuple{Bool}))

if opt_level > 0
    @test !occursin("%gcframe", get_llvm(pointer_not_safepoint, Tuple{}))
    compare_large_struct_ir = get_llvm(compare_large_struct, Tuple{typeof(create_ref_struct())})
    @test occursin("call i32 @memcmp(", compare_large_struct_ir) || occursin("call i32 @bcmp(", compare_large_struct_ir)
    @test !occursin("%gcframe", compare_large_struct_ir)

    @test occursin("jl_gc_pool_alloc", get_llvm(MutableStruct, Tuple{}))
    breakpoint_mutable_ir = get_llvm(breakpoint_mutable, Tuple{MutableStruct})
    @test !occursin("%gcframe", breakpoint_mutable_ir)
    @test !occursin("jl_gc_pool_alloc", breakpoint_mutable_ir)

    breakpoint_badref_ir = get_llvm(breakpoint_badref, Tuple{MutableStruct})
    @test !occursin("%gcframe", breakpoint_badref_ir)
    @test !occursin("jl_gc_pool_alloc", breakpoint_badref_ir)

    breakpoint_ptrstruct_ir = get_llvm(breakpoint_ptrstruct, Tuple{RealStruct})
    @test !occursin("%gcframe", breakpoint_ptrstruct_ir)
    @test !occursin("jl_gc_pool_alloc", breakpoint_ptrstruct_ir)
end

function two_breakpoint(a::Float64)
    ccall(:jl_breakpoint, Cvoid, (Ref{Float64},), a)
    ccall(:jl_breakpoint, Cvoid, (Ref{Float64},), a)
end

function load_dummy_ref(x::Int)
    r = Ref{Int}(x)
    GC.@preserve r begin
        unsafe_load(Ptr{Int}(pointer_from_objref(r)))
    end
end

if opt_level > 0
    breakpoint_f64_ir = get_llvm((a)->ccall(:jl_breakpoint, Cvoid, (Ref{Float64},), a),
                                 Tuple{Float64})
    @test !occursin("jl_gc_pool_alloc", breakpoint_f64_ir)
    breakpoint_any_ir = get_llvm((a)->ccall(:jl_breakpoint, Cvoid, (Ref{Any},), a),
                                 Tuple{Float64})
    @test occursin("jl_gc_pool_alloc", breakpoint_any_ir)
    two_breakpoint_ir = get_llvm(two_breakpoint, Tuple{Float64})
    @test !occursin("jl_gc_pool_alloc", two_breakpoint_ir)
    @test occursin("llvm.lifetime.end", two_breakpoint_ir)

    @test load_dummy_ref(1234) === 1234
    load_dummy_ref_ir = get_llvm(load_dummy_ref, Tuple{Int})
    @test !occursin("jl_gc_pool_alloc", load_dummy_ref_ir)
    # Hopefully this is reliable enough. LLVM should be able to optimize this to a direct return.
    @test occursin("ret $Iptr %0", load_dummy_ref_ir)
end

# Issue 22770
let was_gced = false
    @noinline make_tuple(x) = tuple(x)
    @noinline use(x) = ccall(:jl_breakpoint, Cvoid, ())
    @noinline assert_not_gced() = @test !was_gced

    function foo22770()
        b = Ref(2)
        finalizer(x -> was_gced = true, b)
        y = make_tuple(b)
        x = y[1]
        a = Ref(1)
        use(x); use(a); use(y)
        c = Ref(3)
        GC.gc()
        assert_not_gced()
        use(x)
        use(c)
    end
    foo22770()
    GC.gc()
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
        ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), a, 1)
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
    @test !occursin("%gcframe",
                    get_llvm(g22421_1, Tuple{Base.RefValue{Int},Base.RefValue{Int},Bool}))
    @test !occursin("%gcframe",
                    get_llvm(g22421_2, Tuple{Base.RefValue{Int},Base.RefValue{Int},Bool}))
    @test !occursin("%gcframe", get_llvm(g24108, Tuple{B24108}))
end

str_22330 = """
Base.convert(::Type{Array{T,n}}, a::Array) where {T<:Number,n} =
             copyto!(Array{T,n}(undef, size(a)), a)

empty(Dict(),  Pair{Union{},Union{}})
"""
mktemp() do f_22330, _
    write(f_22330, str_22330)
    @test success(`$(Base.julia_cmd()) --startup-file=no $f_22330`)
end

# Alias scope
macro aliasscope(body)
    sym = gensym()
    esc(quote
        $(Expr(:aliasscope))
        $sym = $body
        $(Expr(:popaliasscope))
        $sym
    end)
end

struct Const{T<:Array}
    a::T
end

@eval Base.getindex(A::Const, i1::Int) = Core.const_arrayref($(Expr(:boundscheck)), A.a, i1)
@eval Base.getindex(A::Const, i1::Int, i2::Int, I::Int...) =  (Base.@_inline_meta; Core.const_arrayref($(Expr(:boundscheck)), A.a, i1, i2, I...))

function foo31018!(a, b)
    @aliasscope for i in eachindex(a, b)
        a[i] = Const(b)[i]
    end
end
io = IOBuffer()
code_llvm(io, foo31018!, Tuple{Vector{Int}, Vector{Int}}, optimize=false, raw=true, dump_module=true)
str = String(take!(io))
@test occursin("alias.scope", str)
@test occursin("aliasscope", str)
@test occursin("noalias", str)

# Issue #10208 - Unnecessary boxing for calling objectid
struct FooDictHash{T}
    x::T
end

function f_dict_hash_alloc()
    d = Dict{FooDictHash{Int},Int}()
    for i in 1:10000
        d[FooDictHash(i)] = i+1
    end
    d
end

function g_dict_hash_alloc()
    d = Dict{Int,Int}()
    for i in 1:10000
        d[i] = i+1
    end
    d
end
# Warm up
f_dict_hash_alloc(); g_dict_hash_alloc();
@test (@allocated f_dict_hash_alloc()) == (@allocated g_dict_hash_alloc())

# returning an argument shouldn't alloc a new box
@noinline f33829(x) = (global called33829 = true; x)
g33829() = @allocated Base.inferencebarrier(f33829)(1.1,)
g33829() # warm up
@test (@allocated g33829()) == 0
@test called33829 # make sure there was a global side effect so it's hard for this call to simply be removed
let src = get_llvm(f33829, Tuple{Float64}, true, true)
    @test occursin(r"call [^(]*double @", src)
    @test !occursin(r"call [^(]*\{}", src)
end

let io = IOBuffer()
    # Test for the f(args...) = g(args...) generic codegen optimization
    code_llvm(io, Base.vect, Tuple{Vararg{Union{Float64, Int64}}})
    @test !occursin("__apply", String(take!(io)))
end

function f1_30093(r)
    while r[]>0
        try
        finally
        end
    end
end

@test f1_30093(Ref(0)) == nothing

# issue 33590
function f33590(b, x)
    y = b ? nothing : (x[1] + 1,)
    return something(ifelse(b, x, y))
end
@test f33590(true, (3,)) == (3,)
@test f33590(false, (3,)) == (4,)

# issue 29864
const c29864 = VecElement{Union{Int,Nothing}}(2)
@noinline f29864() = c29864
@noinline g29864() = VecElement{Union{Int,Nothing}}(3)
@test f29864().value === 2
@test g29864().value === 3

# test sret pointing into a struct containing a tracked pointer
# reduced from TerminalLoggers/ProgressLogging
const _PROGRESS_LOGGING_UUID_NS_test = Base.UUID("1e962757-ea70-431a-b9f6-aadf988dcb7f")
_asuuid_test(id) = Base.uuid5(_PROGRESS_LOGGING_UUID_NS_test, repr(id))
@noinline _handle_progress_test(progress) = progress
function _handle_message_test()
    progress = (_asuuid_test(:id), "name")
    return _handle_progress_test(progress)
end
@test _handle_message_test() isa Tuple{Base.UUID, String}

@testset "#30739" begin
    ifelsetuple(n::Integer, k::Integer, f, g) = ntuple(i -> (i <= k ? f : g), n)
    f(x) = x^2; g(x) = x^3;
    a = [1]; b = [2]
    @test ifelsetuple(5, 3, a, b) == ([1], [1], [1], [2], [2])
end

@testset "#36422" begin
    str_36422 = "using InteractiveUtils; code_llvm(Base.ht_keyindex, (Dict{NTuple{65,Int64},Nothing}, NTuple{65,Int64}))"
    @test success(`$(Base.julia_cmd()) --startup-file=no -e $str_36422`)
end
