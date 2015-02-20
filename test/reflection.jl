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
test_code_reflection(code_native, ismatch, (Regex, AbstractString))
test_code_reflection(code_native, +, (Int, Int))
test_code_reflection(code_native, +, (Array{Float32}, Array{Float32}))

test_code_reflection(code_llvm, ismatch, (Regex, AbstractString))
test_code_reflection(code_llvm, +, (Int, Int))
test_code_reflection(code_llvm, +, (Array{Float32}, Array{Float32}))

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
code_warntype(iob, pos_unstable, (Float64,))
str = takebuf_string(iob)
@test !isempty(search(str, tag))
code_warntype(iob, pos_stable, (Float64,))
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
code_warntype(iob, getindex, (Unstable{Float64},Int))
str = takebuf_string(iob)
@test !isempty(search(str, tag))
code_warntype(iob, getindex, (Stable{Float64,2},Int))
str = takebuf_string(iob)
@test isempty(search(str, tag))
code_warntype(iob, getindex, (Stable{Float64},Int))
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
@test which(i10165, (Type{AbstractArray{Int}},)).sig == (DataType,)

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

