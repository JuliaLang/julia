# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for /base/stacktraces.jl

using Serialization, Base.StackTraces

let
    @noinline child() = stacktrace()
    @noinline parent() = child()
    @noinline grandparent() = parent()
    line_numbers = @__LINE__() .- [3, 2, 1]
    stack = grandparent()

    # Basic tests.
    @assert length(stack) >= 3 "Compiler has unexpectedly inlined functions"

    @test [:child, :parent, :grandparent] == [f.func for f in stack[1:3]]
    for (line, frame) in zip(line_numbers, stack[1:3])
        @test [Symbol(@__FILE__), line] == [frame.file, frame.line]
    end
    @test [false, false, false] == [f.from_c for f in stack[1:3]]

    # Test remove_frames!
    stack = StackTraces.remove_frames!(grandparent(), :parent)
    @test stack[1] == StackFrame(:grandparent, @__FILE__, line_numbers[3])

    stack = StackTraces.remove_frames!(grandparent(), [:child, :something_nonexistent])
    @test stack[1:2] == [
        StackFrame(:parent, @__FILE__, line_numbers[2]),
        StackFrame(:grandparent, @__FILE__, line_numbers[3])
    ]

    b = PipeBuffer()
    frame = stack[1]
    serialize(b, frame)
    frame2 = deserialize(b)
    @test frame !== frame2
    @test frame == frame2
    @test frame.linfo !== nothing
    @test frame2.linfo === nothing
end

# Test from_c
let (default, with_c, without_c) = (stacktrace(), stacktrace(true), stacktrace(false))
    @test default == without_c
    @test length(with_c) > length(without_c)
    @test !isempty(filter(frame -> frame.from_c, with_c))
    @test isempty(filter(frame -> frame.from_c, without_c))
end

@test StackTraces.lookup(C_NULL) == [StackTraces.UNKNOWN] == StackTraces.lookup(C_NULL + 1) == StackTraces.lookup(C_NULL - 1)

let ct = current_task()
    # After a task switch, there should be nothing in catch_backtrace
    yieldto(@task yieldto(ct))
    @test catch_backtrace() == StackFrame[]

    @noinline bad_function() = throw(UndefVarError(:nonexistent))
    @noinline function try_stacktrace()
        try
            bad_function()
        catch
            return stacktrace()
        end
    end
    @noinline function try_catch()
        try
            bad_function()
        catch
            return stacktrace(catch_backtrace())
        end
    end
    line_numbers = @__LINE__() .- [15, 10, 5]

    # Test try...catch with stacktrace
    @test try_stacktrace()[1] == StackFrame(:try_stacktrace, @__FILE__, line_numbers[2])

    # Test try...catch with catch_backtrace
    @test try_catch()[1:2] == [
        StackFrame(:bad_function, @__FILE__, line_numbers[1]),
        StackFrame(:try_catch, @__FILE__, line_numbers[3])
    ]
end

module inlined_test
using Test
@inline g(x) = (x == 3 && throw("a"); x)
@inline h(x) = (x == 3 && g(x); x)
f(x) = (y = h(x); y)
trace = (try; f(3); catch; stacktrace(catch_backtrace()); end)[1:3]
can_inline = Bool(Base.JLOptions().can_inline)
for (frame, func, inlined) in zip(trace, [g,h,f], (can_inline, can_inline, false))
    @test frame.func === typeof(func).name.mt.name
    #@test get(frame.linfo).def === which(func, (Any,)).func
    #@test get(frame.linfo).specTypes === Tuple{typeof(func), Int}
    # line
    @test frame.file === Symbol(@__FILE__)
    @test !frame.from_c
    @test frame.inlined === inlined
end
end

let src = Meta.lower(Main, quote let x = 1 end end).args[1]::Core.CodeInfo,
    li = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ()),
    sf

    setfield!(li, :uninferred, src, :monotonic)
    li.specTypes = Tuple{}
    li.def = @__MODULE__
    sf = StackFrame(:a, :b, 3, li, false, false, 0)
    repr = string(sf)
    @test repr == "Toplevel MethodInstance thunk at b:3"
end
let li = typeof(fieldtype).name.mt.cache.func::Core.MethodInstance,
    sf = StackFrame(:a, :b, 3, li, false, false, 0),
    repr = string(sf)
    @test repr == "fieldtype(...) at b:3"
end

let ctestptr = cglobal((:ctest, "libccalltest")),
    ctest = StackTraces.lookup(ctestptr)

    @test length(ctest) == 1
    @test ctest[1].func === :ctest
    @test ctest[1].linfo === nothing
    @test ctest[1].from_c
    @test ctest[1].pointer === UInt64(ctestptr)
end

# issue #19655
let st = stacktrace(empty!(backtrace()))
    # not in a `catch`, so should return an empty StackTrace
    @test isempty(st)
    @test isa(st, StackTrace)
end

module StackTracesTestMod
    unfiltered_stacktrace() = stacktrace()
    filtered_stacktrace() = StackTraces.remove_frames!(stacktrace(), StackTracesTestMod)
end

# Test that `removes_frames!` can correctly remove frames from within the module
trace = StackTracesTestMod.unfiltered_stacktrace()
@test occursin("unfiltered_stacktrace", string(trace))

trace = StackTracesTestMod.filtered_stacktrace()
@test !occursin("filtered_stacktrace", string(trace))

let bt, topline = @__LINE__
try
    let x = 1
        y = 2x
        z = 2z-1
    end
catch
    bt = stacktrace(catch_backtrace())
end
@test bt[1].line == topline+4
end

# issue #28990
let bt
try
    eval(Expr(:toplevel, LineNumberNode(42, :foo), :(error("blah"))))
catch
    bt = stacktrace(catch_backtrace())
end
@test bt[2].line == 42
@test bt[2].file === :foo
end

@noinline f33065(x; b=1.0, a="") = error()
@noinline f33065(x, y; b=1.0, a="", c...) = error()
let bt
    try
        f33065(0.0f0)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(x::Float32; b::Float64, a::String)"), bt)
    try
        f33065(0.0f0, b=:x)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(x::Float32; b::Symbol, a::String)"), bt)
    try
        f33065(0.0f0, 0.0f0, z=0)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(x::Float32, y::Float32; b::Float64, a::String, c::"), bt)
end

@testset "TypeTreeIO" begin
    ttio = Base.TypeTreesIO.TypeTreeIO()
    obj = view([1, 2, 3], 1:2)
    print(ttio, typeof(obj))
    @test sprint(print, ttio.tree) == sprint(print, typeof(obj))
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>1), node), ttio.tree) == "SubArray{…}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>2), node), ttio.tree) == "SubArray{$Int, 1, Vector{…}, Tuple{…}, true}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>3), node), ttio.tree) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{…}}, true}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>4), node), ttio.tree) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{$Int}}, true}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>3), node), ttio.tree) == "SubArray{…}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>44), node), ttio.tree) == "SubArray{…}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>45), node), ttio.tree) == "SubArray{$Int, 1, Vector{…}, Tuple{…}, true}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>59), node), ttio.tree) == "SubArray{$Int, 1, Vector{…}, Tuple{…}, true}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>60), node), ttio.tree) == "SubArray{Int64, 1, Vector{Int64}, Tuple{UnitRange{…}}, true}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>100), node), ttio.tree) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{$Int}}, true}"
    @test String(take!(ttio)) == sprint(print, typeof(obj))

    # ttio can be reused (after `take!`)
    typ = Vector{V} where V<:AbstractVector{T} where T<:Real
    print(ttio, typ)
    @test sprint(print, ttio.tree) == sprint(print, typ)
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>1), node), ttio.tree) == "Vector{…} where {…}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>2), node), ttio.tree) == "Vector{V} where {T<:Real, V<:AbstractVector{…}}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxdepth=>3), node), ttio.tree) == "Vector{V} where {T<:Real, V<:AbstractVector{T}}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>20), node), ttio.tree) == "Vector{…} where {…}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>46), node), ttio.tree) == "Vector{…} where {…}"
    @test sprint((io, node) -> print(IOContext(io, :type_maxwidth=>47), node), ttio.tree) == "Vector{V} where {T<:Real, V<:AbstractVector{T}}"
    @test String(take!(ttio)) == sprint(print, typ)

    typ = Vector{T} where T<:Real
    print(ttio, typ)
    @test String(take!(ttio)) == sprint(print, typ)
    # IOContext
    print(IOContext(ttio, :color=>true), typ)
    @test String(take!(ttio)) == sprint(print, typ)

    typ = Union{Int32, T} where T
    print(ttio, typ)
    @test String(take!(ttio)) == sprint(print, typ)

    print(ttio, Tuple{})
    @test String(take!(ttio)) == sprint(print, Tuple{})

    # Stacktrace
    a = UInt8(81):UInt8(160)
    b = view(a, 1:64)
    c = reshape(b, (8, 8))
    d = reinterpret(reshape, Float64, c)
    sqrteach(a) = [sqrt(x) for x in a]
    st = try
        sqrteach(d)
    catch e
        stacktrace(catch_backtrace())
    end
    nmi = 0
    for sf in st
        mi = sf.linfo
        if mi !== nothing
            if isa(mi, Core.MethodInstance)
                nmi += 1
                print(ttio, mi.specTypes)
                @test String(take!(ttio)) == sprint(print, mi.specTypes)
            end
        end
        iobuf = IOBuffer()
        ioctx = IOContext(iobuf, :limit=>true,  :color=>true, :displaysize=>(50,1000), :backtrace=>true)
        Base.StackTraces.show_spec_linfo(ioctx, sf)
        str = String(take!(iobuf))
        ioctx = IOContext(iobuf, :limit=>false, :color=>true, :displaysize=>(50,1000), :backtrace=>true)
        Base.StackTraces.show_spec_linfo(ioctx, sf)
        @test str == String(take!(iobuf))
    end
    @test nmi > 0

    # Whole signatures
    m = which(show, (IO, String))
    print(ttio, m.sig)
    @test String(take!(ttio)) == sprint(print, m.sig)
    print(ttio, "show(io::IO, x::String)")
    @test String(take!(ttio)) == "show(io::IO, x::String)"
    T = TypeVar(:T)
    print(ttio, "show(io::IO, x::", T, ')', " where ", T)
    @test String(take!(ttio)) == "show(io::IO, x::T) where T"
end
