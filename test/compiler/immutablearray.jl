using Test
import Core: arrayfreeze, mutating_arrayfreeze, arraythaw
import Core.Compiler: arrayfreeze_tfunc, mutating_arrayfreeze_tfunc, arraythaw_tfunc

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

# mutating_arrayfreeze optimization
# =================================

import Core.Compiler: argextype, singleton_type
const EMPTY_SPTYPES = Any[]

code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::Core.CodeInfo

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

# optimizable examples
# --------------------

let # simplest -- vector
    function optimizable(gen)
        a = [1,2,3,4,5]
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test allocated == @allocated optimizable(ImmutableArray)
    end
end

let # handle matrix etc. (actually this example also requires inter-procedural escape handling)
    function optimizable(gen)
        a = [1 2 3; 4 5 6]
        b = [1 2 3 4 5 6]
        return gen(a), gen(b)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        # @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 2
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test allocated == @allocated optimizable(ImmutableArray)
    end
end

let # multiple returns don't matter
    function optimizable(gen)
        a = [1,2,3,4,5]
        return gen(a), gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 2
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test allocated == @allocated optimizable(ImmutableArray)
    end
end

let # arrayset
    function optimizable1(gen)
        a = Vector{Int}(undef, 5)
        for i = 1:5
            a[i] = i
        end
        return gen(a)
    end
    let src = code_typed1(optimizable1, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable1(identity)
        allocated = @allocated optimizable1(identity)
        optimizable1(ImmutableArray)
        @test allocated == @allocated optimizable1(ImmutableArray)
    end

    function unoptimizable(gen)
        a = Matrix{Float64}(undef, 5, 2)
        for i = 1:5
            for j = 1:2
                a[i, j] = i + j
            end
        end
        return gen(a)
    end
    let src = code_typed1(unoptimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        unoptimizable(identity)
        allocated = @allocated unoptimizable(identity)
        unoptimizable(ImmutableArray)
        @test allocated == @allocated unoptimizable(ImmutableArray)
    end
end

let # arrayref
    function optimizable(gen)
        a = [1,2,3]
        b = getindex(a, 2)
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test allocated == @allocated optimizable(ImmutableArray)
    end
end

let # array resize
    function optimizable(gen, n)
        a = Int[]
        for i = 1:n
            push!(a, i)
        end
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},Int,))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity, 42)
        allocated = @allocated optimizable(identity, 42)
        optimizable(ImmutableArray, 42)
        @test allocated == @allocated optimizable(ImmutableArray, 42)
    end
end

@noinline function same′(a)
    return reverse(reverse(a))
end
let # inter-procedural
    function optimizable(gen)
        a = ones(5)
        a = same′(a)
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(isinvoke(:same′), src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test allocated == @allocated optimizable(ImmutableArray)
    end
end

let # ignore ThrownEscape if it never happens when `arrayfreeze` is called
    function optimizable(gen, n)
        a = Int[]
        for i = 1:n
            push!(a, i)
        end
        n > 100 && throw(a)
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},Int,))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity, 42)
        allocated = @allocated optimizable(identity, 42)
        optimizable(ImmutableArray, 42)
        @test allocated == @allocated optimizable(ImmutableArray, 42)
    end
end
@noinline function ipo_getindex′(a, n)
    ele = getindex(a, n)
    return ele
end
let # ignore ThrownEscape if it never happens when `arrayfreeze` is called (interprocedural)
    function optimizable(gen)
        a = [1,2,3]
        b = ipo_getindex′(a, 2)
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(isinvoke(:ipo_getindex′), src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test allocated == @allocated optimizable(ImmutableArray)
    end
end

let # nested case
    function optimizable(gen, n)
        a = [collect(1:m) for m in 1:n]
        for i = 1:n
            a[i][1] = i
        end
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},Int))
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity, 100)
        allocated = @allocated optimizable(identity, 100)
        optimizable(ImmutableArray, 100)
        @test allocated == @allocated optimizable(ImmutableArray, 100)
    end
end

# demonstrate alias analysis
broadcast_identity(a) = broadcast(identity, a)
function optimizable_aa(gen, n) # can't be a closure somehow
    return collect(1:n) |>
           Ref |> Ref |> Ref |>
           broadcast_identity |> broadcast_identity |> broadcast_identity |>
           gen
end
let src = code_typed1(optimizable_aa, (Type{ImmutableArray},Int))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, arrayfreeze)), src.code) == 0
    optimizable_aa(identity, 100)
    allocated = @allocated optimizable_aa(identity, 100)
    optimizable_aa(ImmutableArray, 100)
    @test allocated == @allocated optimizable_aa(ImmutableArray, 100)
end

let # should be possible if we change BoundsError semantics (so that it doesn't capture the indexed array)
    function optimizable(gen)
        a = [1,2,3]
        try
            getindex(a, 4)
        catch
        end
        return gen(a)
    end
    let src = code_typed1(optimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test_broken count(iscall((src, mutating_arrayfreeze)), src.code) == 1
        @test_broken count(iscall((src, arrayfreeze)), src.code) == 0
        optimizable(identity)
        allocated = @allocated optimizable(identity)
        optimizable(ImmutableArray)
        @test_broken allocated == @allocated optimizable(ImmutableArray)
    end
end

# unoptimizable examples
# ----------------------

const Rx = Ref{Any}() # global memory

let # return escape
    function unoptimizable(gen)
        a = [1,2,3,4,5]
        return a, gen(a)
    end
    let src = code_typed1(unoptimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 0
        @test count(iscall((src, arrayfreeze)), src.code) == 1
        unoptimizable(ImmutableArray)
        a, b = unoptimizable(ImmutableArray)
        @test a !== b
        @test !(a isa ImmutableArray)
    end
end

let # arg escape
    unoptimizable(a, gen) = gen(a)
    let src = code_typed1(unoptimizable, (Vector{Int}, Type{ImmutableArray},))
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 0
        @test count(iscall((src, arrayfreeze)), src.code) == 1
        a = [1,2,3]
        unoptimizable(a, ImmutableArray)
        b = unoptimizable(a, ImmutableArray)
        @test a !== b
        @test !(a isa ImmutableArray)
        @test b isa ImmutableArray
    end
end

let # global escape
    function unoptimizable(gen)
        a = [1,2,3,4,5]
        global global_array = a
        return gen(a)
    end
    let src = code_typed1(unoptimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 0
        @test count(iscall((src, arrayfreeze)), src.code) == 1
        unoptimizable(identity)
        unoptimizable(ImmutableArray)
        a = unoptimizable(ImmutableArray)
        @test global_array !== a
        @test !(global_array isa ImmutableArray)
    end
end

let # global escape
    function unoptimizable(gen)
        a = [1,2,3,4,5]
        Rx[] = a
        return gen(a)
    end
    let src = code_typed1(unoptimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 0
        @test count(iscall((src, arrayfreeze)), src.code) == 1
        unoptimizable(identity)
        unoptimizable(ImmutableArray)
        a = unoptimizable(ImmutableArray)
        @test Rx[] !== a
        @test !(Rx[] isa ImmutableArray)
    end
end

let # escapes via exception
    function unoptimizable(gen)
        a = [1,2,3,4,5]
        try
            throw(a)
        catch err
            global global_array = err
        end
        return gen(a)
    end
    let src = code_typed1(unoptimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 0
        @test count(iscall((src, arrayfreeze)), src.code) == 1
        unoptimizable(identity)
        allocated = @allocated unoptimizable(identity)
        unoptimizable(ImmutableArray)
        local a
        @test allocated < @allocated a = unoptimizable(ImmutableArray)
        @test global_array !== a
        @test !(global_array isa ImmutableArray)
    end
end

const g = Ref{Any}()
let # escapes via BoundsError
    function unoptimizable(gen)
        a = [1,2,3]
        try
            getindex(a, 4)
        catch e
            g[] = e.a
        end
        return gen(a)
    end
    let src = code_typed1(unoptimizable, (Type{ImmutableArray},))
        @test count(is_array_alloc, src.code) == 1
        @test count(iscall((src, arrayfreeze)), src.code) == 1
        @test count(iscall((src, mutating_arrayfreeze)), src.code) == 0
        unoptimizable(identity)
        unoptimizable(ImmutableArray)
        ia = unoptimizable(ImmutableArray)
        @test g[] !== ia
    end
end