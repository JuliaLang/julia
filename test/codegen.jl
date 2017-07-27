# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for codegen and optimizations

const opt_level = Base.JLOptions().opt_level
const coverage = (Base.JLOptions().code_coverage > 0) || (Base.JLOptions().malloc_log > 0)
const Iptr = sizeof(Int) == 8 ? "i64" : "i32"

# `_dump_function` might be more efficient but it doesn't really matter here...
get_llvm(@nospecialize(f), @nospecialize(t), strip_ir_metadata=true, dump_module=false) =
    sprint(code_llvm, f, t, strip_ir_metadata, dump_module)

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
    ccall(:jl_dump_compiles, Void, (Ptr{Void},), io.handle)
    eval(@noinline function test_jl_dump_compiles_internal(x)
        if x > 0
            test_jl_dump_compiles_internal(x-1)
        end
        end)
    test_jl_dump_compiles_internal(1)
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
    ccall(:jl_dump_compiles, Void, (Ptr{Void},), io.handle)
    eval(expand(Main, :(for i in 1:10 end)))
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

if opt_level > 0
    @test !contains(get_llvm(isequal, Tuple{Nullable{BigFloat}, Nullable{BigFloat}}), "%gcframe")
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
end

# Issue 22770
let was_gced = false
    @noinline make_tuple(x) = tuple(x)
    @noinline use(x) = ccall(:jl_breakpoint, Void, ())
    @noinline assert_not_gced() = @assert !was_gced

    function foo22770()
        b = Ref(2)
        finalizer(b, x->(global was_gced; was_gced=true))
        y = make_tuple(b)
        x = y[1]
        a = Ref(1)
        use(x); use(a); use(y)
        c = Ref(3)
        gc(); assert_not_gced();
        use(x)
        use(c)
    end
    foo22770()
end

function egal_svecs()
    a = Core.svec(:a, :b)
    b = Core.svec(:a, :b)
    a === b
end
@test egal_svecs()
@test Core.svec(:a, :b) === Core.svec(:a, :b)
