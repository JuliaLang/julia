# code_native / code_llvm (issue #8239)
# It's hard to really test these, but just running them should be
# sufficient to catch segfault bugs.

function test_code_reflection(freflect, f, types)
    iob = IOBuffer()
    freflect(iob, f, types)
    str = takebuf_string(iob)
    @test !isempty(str)
end

println(STDERR, "The following 'Returned code...' warnings indicate normal behavior:")
test_code_reflection(code_native, ismatch, Tuple{Regex, AbstractString})
test_code_reflection(code_native, +, Tuple{Int, Int})
test_code_reflection(code_native, +, Tuple{Array{Float32}, Array{Float32}})

test_code_reflection(code_llvm, ismatch, Tuple{Regex, AbstractString})
test_code_reflection(code_llvm, +, Tuple{Int, Int})
test_code_reflection(code_llvm, +, Tuple{Array{Float32}, Array{Float32}})

@test_throws Exception code_native(+, Int, Int)
@test_throws Exception code_native(+, Array{Float32}, Array{Float32})

@test_throws Exception code_llvm(+, Int, Int)
@test_throws Exception code_llvm(+, Array{Float32}, Array{Float32})

# code_warntype
module WarnType
using Base.Test

iob = IOBuffer()

pos_stable(x) = x > 0 ? x : zero(x)
pos_unstable(x) = x > 0 ? x : 0

tag = Base.have_color ? Base.text_colors[:red] : "UNION"
code_warntype(iob, pos_unstable, Tuple{Float64,})
str = takebuf_string(iob)
@test !isempty(search(str, tag))
code_warntype(iob, pos_stable, Tuple{Float64,})
str = takebuf_string(iob)
@test isempty(search(str, tag))

type Stable{T,N}
    A::Array{T,N}
end
type Unstable{T}
    A::Array{T}
end
Base.getindex(A::Stable, i) = A.A[i]
Base.getindex(A::Unstable, i) = A.A[i]

tag = Base.have_color ? Base.text_colors[:red] : "ARRAY{FLOAT64,N}"
code_warntype(iob, getindex, Tuple{Unstable{Float64},Int})
str = takebuf_string(iob)
@test !isempty(search(str, tag))
code_warntype(iob, getindex, Tuple{Stable{Float64,2},Int})
str = takebuf_string(iob)
@test isempty(search(str, tag))
code_warntype(iob, getindex, Tuple{Stable{Float64},Int})
str = takebuf_string(iob)
@test !isempty(search(str, tag))

end

# isbits

@test !isbits(Array{Int})
@test isbits(Float32)
@test isbits(Int)
@test !isbits(AbstractString)

# issue #10165
i10165(::DataType) = 0
i10165{T,n}(::Type{AbstractArray{T,n}}) = 1
@test i10165(AbstractArray{Int}) == 0
@test which(i10165, Tuple{Type{AbstractArray{Int}},}).sig == Tuple{DataType,}

# fullname
@test fullname(Base) == (:Base,)
@test fullname(Base.Pkg) == (:Base, :Pkg)

const a_const = 1
not_const = 1
@test isconst(:a_const) == true
@test isconst(Base, :pi) == true
@test isconst(:pi) == true
@test isconst(:not_const) == false
@test isconst(:is_not_defined) == false

@test isimmutable(1) == true
@test isimmutable([]) == false
@test isimmutable("abc") == true

## find bindings tests
@test ccall(:jl_get_module_of_binding, Any, (Any, Any), Base, :sin)==Base
@test_throws UndefVarError ccall(:jl_get_module_of_binding, Any, (Any, Any), Base, :sdi597sl3)

module TestMod7648
using Base.Test
export a9475, c7648, foo7648

const c7648 = 8
d7648 = 9
const f7648 = 10
foo7648(x) = x

    module TestModSub9475
    using Base.Test
    using ..TestMod7648
    export a9475
    a9475 = 5
    b9475 = 7
    let
        @test Base.binding_module(:a9475)==current_module()
        @test Base.binding_module(:c7648)==TestMod7648
        @test Base.module_name(current_module())==:TestModSub9475
        @test Base.fullname(current_module())==(:TestMod7648, :TestModSub9475)
        @test Base.module_parent(current_module())==TestMod7648
    end
    end # module TestModSub9475

using .TestModSub9475

let
    @test Base.binding_module(:d7648)==current_module()
    @test Base.binding_module(:a9475)==TestModSub9475
    @test Base.module_name(current_module())==:TestMod7648
    @test Base.module_parent(current_module())==Main
end
end # module TestMod7648

let
    @test Base.binding_module(TestMod7648, :d7648)==TestMod7648
    @test Base.binding_module(TestMod7648, :a9475)==TestMod7648.TestModSub9475
    @test Base.binding_module(TestMod7648.TestModSub9475, :b9475)==TestMod7648.TestModSub9475
    @test Set(names(TestMod7648))==Set([:TestMod7648, :a9475, :c7648, :foo7648])
    @test isconst(TestMod7648, :c7648)
    @test !isconst(TestMod7648, :d7648)
    @test !isgeneric(isa)
end

let
    using TestMod7648
    @test Base.binding_module(:a9475)==TestMod7648.TestModSub9475
    @test Base.binding_module(:c7648)==TestMod7648
    @test isgeneric(foo7648)
    @test Base.function_name(foo7648)==:foo7648
    @test Base.function_module(foo7648, (Any,))==TestMod7648
    @test functionloc(foo7648, (Any,))[1]=="reflection.jl"
    @test TestMod7648.TestModSub9475.foo7648.env.defs==@which foo7648(5)
    @test TestMod7648==@which foo7648
    @test TestMod7648.TestModSub9475==@which a9475
end
