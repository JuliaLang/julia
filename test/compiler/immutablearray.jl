using Test
import Core: ImmutableArray
import Core.Compiler: arrayfreeze_tfunc, mutating_arrayfreeze_tfunc, arraythaw_tfunc
const ImmutableVector{T} = Core.ImmutableArray{T,1}

@testset "ImmutableArray tfuncs" begin
    @test arrayfreeze_tfunc(Vector{Int}) === ImmutableVector{Int}
    @test arrayfreeze_tfunc(Vector) === ImmutableVector
    @test arrayfreeze_tfunc(Array) === ImmutableArray
    @test arrayfreeze_tfunc(Any) === ImmutableArray
    @test arrayfreeze_tfunc(ImmutableVector{Int}) === Union{}
    @test arrayfreeze_tfunc(ImmutableVector) === Union{}
    @test arrayfreeze_tfunc(ImmutableArray) === Union{}
    @test mutating_arrayfreeze_tfunc(Vector{Int}) === ImmutableVector{Int}
    @test mutating_arrayfreeze_tfunc(Vector) === ImmutableVector
    @test mutating_arrayfreeze_tfunc(Array) === ImmutableArray
    @test mutating_arrayfreeze_tfunc(Any) === ImmutableArray
    @test mutating_arrayfreeze_tfunc(ImmutableVector{Int}) === Union{}
    @test mutating_arrayfreeze_tfunc(ImmutableVector) === Union{}
    @test mutating_arrayfreeze_tfunc(ImmutableArray) === Union{}
    @test arraythaw_tfunc(ImmutableVector{Int}) === Vector{Int}
    @test arraythaw_tfunc(ImmutableVector) === Vector
    @test arraythaw_tfunc(ImmutableArray) === Array
    @test arraythaw_tfunc(Any) === Array
    @test arraythaw_tfunc(Vector{Int}) === Union{}
    @test arraythaw_tfunc(Vector) === Union{}
    @test arraythaw_tfunc(Array) === Union{}
end

@testset "ImmutableArray builtins" begin
    # basic functionality
    let
        a = [1,2,3]
        b = Core.ImmutableArray(a)
        @test Core.arrayfreeze(a) === b
        @test Core.mutating_arrayfreeze(a) === b
        @test Core.arraythaw(b) == a # arraythaw copies so not ===
    end
    # errors
    @test_throws ArgumentError Core.arrayfreeze()
    @test_throws ArgumentError Core.arrayfreeze([1,2,3], nothing)
    @test_throws TypeError Core.arrayfreeze("not an array")
    @test_throws ArgumentError Core.mutating_arrayfreeze()
    @test_throws ArgumentError Core.mutating_arrayfreeze([1,2,3], nothing)
    @test_throws ArgumentError Core.arraythaw()
    @test_throws TypeError Core.arraythaw([1,2,3])
end
# mutating_arrayfreeze optimization
# =================================

import Core.Compiler: argextype, singleton_type
const EMPTY_SPTYPES = Any[]

code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::Core.CodeInfo
get_code(args...; kwargs...) = code_typed1(args...; kwargs...).code

# check if `x` is a statement with a given `head`
isnew(@nospecialize x) = Meta.isexpr(x, :new)

# check if `x` is a dynamic call of a given function
iscall(y) = @nospecialize(x) -> iscall(y, x)
function iscall((src, f)::Tuple{Core.CodeInfo,Base.Callable}, @nospecialize(x))
    return iscall(x) do @nospecialize x
        singleton_type(argextype(x, src, EMPTY_SPTYPES)) === f
    end
end
iscall(pred::Base.Callable, @nospecialize(x)) = Meta.isexpr(x, :call) && pred(x.args[1])

# check if `x` is a statically-resolved call of a function whose name is `sym`
isinvoke(y) = @nospecialize(x) -> isinvoke(y, x)
isinvoke(sym::Symbol, @nospecialize(x)) = isinvoke(mi->mi.def.name===sym, x)
isinvoke(pred::Function, @nospecialize(x)) = Meta.isexpr(x, :invoke) && pred(x.args[1]::Core.MethodInstance)

function is_array_alloc(@nospecialize x)
    Meta.isexpr(x, :foreigncall) || return false
    args = x.args
    name = args[1]
    isa(name, QuoteNode) && (name = name.value)
    isa(name, Symbol) || return false
    return Core.Compiler.alloc_array_ndims(name) !== nothing
end

# unescaped examples
# ------------------

# simplest -- vector
function unescaped1_1(gen)
    a = [1,2,3,4,5]
    return gen(a)
end
let src = code_typed1(unescaped1_1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped1_1(identity)
    allocated = @allocated unescaped1_1(identity)
    unescaped1_1(ImmutableArray)
    @test allocated == @allocated unescaped1_1(ImmutableArray)
end

# handle matrix etc. (actually this example also requires inter-procedural escape handling)
function unescaped1_2(gen)
    a = [1 2 3; 4 5 6]
    b = [1 2 3 4 5 6]
    return gen(a), gen(b)
end
let src = code_typed1(unescaped1_2, (Type{Core.ImmutableArray},))
    # @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 2
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped1_2(identity)
    allocated = @allocated unescaped1_2(identity)
    unescaped1_2(ImmutableArray)
    @test allocated == @allocated unescaped1_2(ImmutableArray)
end

# multiple returns don't matter
function unescaped2(gen)
    a = [1,2,3,4,5]
    return gen(a), gen(a)
end
let src = code_typed1(unescaped2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 2
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped2(identity)
    allocated = @allocated unescaped2(identity)
    unescaped2(ImmutableArray)
    @test allocated == @allocated unescaped2(ImmutableArray)
end

# arrayset
function unescaped3_1(gen)
    a = Vector{Int}(undef, 5)
    for i = 1:5
        a[i] = i
    end
    return gen(a)
end
let src = code_typed1(unescaped3_1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped3_1(identity)
    allocated = @allocated unescaped3_1(identity)
    unescaped3_1(ImmutableArray)
    @test allocated == @allocated unescaped3_1(ImmutableArray)
end

function unescaped3_2(gen)
    a = Matrix{Float64}(undef, 5, 2)
    for i = 1:5
        for j = 1:2
            a[i, j] = i + j
        end
    end
    return gen(a)
end
let src = code_typed1(unescaped3_2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped3_2(identity)
    allocated = @allocated unescaped3_2(identity)
    unescaped3_2(ImmutableArray)
    @test allocated == @allocated unescaped3_2(ImmutableArray)
end

# array resize
function unescaped4(gen, n)
    a = Int[]
    for i = 1:n
        push!(a, i)
    end
    return gen(a)
end
let src = code_typed1(unescaped4, (Type{Core.ImmutableArray},Int,))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped4(identity, 42)
    allocated = @allocated unescaped4(identity, 42)
    unescaped4(ImmutableArray, 42)
    @test allocated == @allocated unescaped4(ImmutableArray, 42)
end

# inter-procedural
@noinline function same′(a)
    return reverse(reverse(a))
end
function unescaped5(gen)
    a = ones(5)
    a = same′(a)
    return gen(a)
end
let src = code_typed1(unescaped5, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(isinvoke(:same′), src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped5(identity)
    allocated = @allocated unescaped5(identity)
    unescaped5(ImmutableArray)
    @test allocated == @allocated unescaped5(ImmutableArray)
end

# ignore ThrownEscape if it never happens when `arrayfreeze` is called
function unescaped6(gen, n)
    a = Int[]
    for i = 1:n
        push!(a, i)
    end
    n > 100 && throw(a)
    return gen(a)
end
let src = code_typed1(unescaped6, (Type{Core.ImmutableArray},Int,))
    @test count(is_array_alloc, src.code) == 1
    @test_broken count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test_broken count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped6(identity, 42)
    allocated = @allocated unescaped6(identity, 42)
    unescaped6(ImmutableArray, 42)
    @test_broken allocated == @allocated unescaped6(ImmutableArray, 42)
end

# arrayref
function unescaped7_1(gen)
    a = [1,2,3]
    b = getindex(a, 2)
    return gen(a)
end
let src = code_typed1(unescaped7_1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped7_1(identity)
    allocated = @allocated unescaped7_1(identity)
    unescaped7_1(ImmutableArray)
    @test allocated == @allocated unescaped7_1(ImmutableArray)
end

@noinline function ipo_getindex′(a, n)
    ele = getindex(a, n)
    return ele
end
function unescaped7_2(gen)
    a = [1,2,3]
    b = ipo_getindex′(a, 2)
    return gen(a)
end
let src = code_typed1(unescaped7_2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(isinvoke(:ipo_getindex′), src.code) == 1
    @test_broken count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test_broken count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped7_2(identity)
    allocated = @allocated unescaped7_2(identity)
    unescaped7_2(ImmutableArray)
    @test_broken allocated == @allocated unescaped7_2(ImmutableArray)
end

# BoundsError (assumes BoundsError doesn't capture arrays)
function unescaped8_1(gen)
    a = [1,2,3]
    try
        getindex(a, 4)
    catch
        return gen(a)
    end
end
let src = code_typed1(unescaped8_1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped8_1(identity)
    allocated = @allocated unescaped7_1(identity)
    unescaped8_1(ImmutableArray)
    @test allocated == @allocated unescaped7_1(ImmutableArray)
end

const g = Ref{Any}()

function unescaped8_2(gen)
    a = [1,2,3]
    try
        getindex(a, 4)
    catch e
        g[] = e.a # XXX these tests pass, but this optimization is actually incorrect until BoundsError doesn't escape its objects
        return gen(a)
    end
end
let src = code_typed1(unescaped8_2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped8_2(identity)
    allocated = @allocated unescaped7_1(identity)
    unescaped8_2(ImmutableArray)
    @test allocated == @allocated unescaped7_1(ImmutableArray)
end

# escaped examples
# ----------------

const Rx = Ref{Any}() # global memory

function escaped01(gen)
    a = [1,2,3,4,5]
    return a, gen(a)
end
let src = code_typed1(escaped01, (Type{ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped01(identity)
    allocated = @allocated escaped01(identity)
    escaped01(ImmutableArray)
    local a, b
    @test allocated < @allocated a, b = escaped01(ImmutableArray)
    @test a !== b
    @test !(a isa ImmutableArray)
end

escaped02(a, gen) = gen(a)
let src = code_typed1(escaped02, (Vector{Int}, Type{ImmutableArray},))
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    a = [1,2,3]
    escaped02(a, ImmutableArray)
    b = escaped02(a, ImmutableArray)
    @test a !== b
    @test !(a isa ImmutableArray)
    @test b isa ImmutableArray
end

function escaped1(gen)
    a = [1,2,3,4,5]
    global global_array = a
    return gen(a)
end
let src = code_typed1(escaped1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped1(identity)
    allocated = @allocated escaped1(identity)
    escaped1(ImmutableArray)
    local a
    @test allocated < @allocated a = escaped1(ImmutableArray)
    @test global_array !== a
    @test !(global_array isa ImmutableArray)
end

function escaped2(gen)
    a = [1,2,3,4,5]
    Rx[] = a
    return gen(a)
end
let src = code_typed1(escaped2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped2(identity)
    allocated = @allocated escaped2(identity)
    escaped2(ImmutableArray)
    local a
    @test allocated < @allocated a = escaped2(ImmutableArray)
    @test Rx[] !== a
    @test !(Rx[] isa ImmutableArray)
end

function escaped3(gen)
    a = [1,2,3,4,5]
    try
        throw(a)
    catch err
        global global_array = err
    end
    return gen(a)
end
let src = code_typed1(escaped3, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped3(identity)
    allocated = @allocated escaped3(identity)
    escaped3(ImmutableArray)
    local a
    @test allocated < @allocated a = escaped3(ImmutableArray)
    @test global_array !== a
    @test !(global_array isa ImmutableArray)
end

# @testset "maybecopy tests" begin
#     g = nothing # global

#     @noinline function escape(arr)
#         g = arr
#         return arr
#     end

#     function mc1()
#         a = Vector{Int64}(undef, 5)
#         b = Core.maybecopy(a) # doesn't escape in this function - so a === b
#         @test a === b
#     end

#     # XXX broken until maybecopy implementation is correct
#     function mc2()
#         a = Vector{Int64}(undef, 5)
#         try
#             getindex(a, 6)
#         catch e
#             if isa(e, BoundsError)
#                 @test_broken !(e.a === a) # only escapes through throw, so this should copy
#             end
#         end
#     end

#     function mc3()
#         a = Vector{Int64}(undef, 5)
#         escape(a)
#         b = Core.maybecopy(a)
#         @test a === b # escapes elsewhere, so give back the actual object
#     end

#     function mc4()
#         a = Vector{Int64}(undef, 5)
#         escape(a)
#         try
#             getindex(a, 6)
#         catch e
#             if isa(e, BoundsError)
#                 @test e.a === a # already escaped so we don't copy
#             end
#         end
#     end

#     function test_maybecopy()
#         mc1(); mc2(); mc3();
#         mc4();
#     end

#     test_maybecopy()
# end

# Check that broadcast precedence is working correctly
@test typeof(Core.ImmutableArray([1,2,3]) .+ Core.ImmutableArray([4,5,6])) <: Core.ImmutableArray