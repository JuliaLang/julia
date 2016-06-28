# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "ambiguous" begin
# DO NOT ALTER ORDER OR SPACING OF METHODS BELOW
const lineoffset = @__LINE__ + 0 # XXX: __LINE__ at the end of a line is off-by-one
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Int, y::Int) = 4
ambig(x::Number, y) = 5
# END OF LINE NUMBER SENSITIVITY

ambigs = Any[[], [3], [2,5], [], [3]]

mt = methods(ambig)

getline(m::Method) = m.line - lineoffset

for m in mt
    ln = getline(m)
    atarget = ambigs[ln]
    if isempty(atarget)
        @test m.ambig == nothing
    else
        aln = Int[getline(a) for a in m.ambig]
        @test sort(aln) == atarget
    end
end

@test length(methods(ambig)) == 5
@test length(Base.methods_including_ambiguous(ambig, Tuple)) == 5

@test length(methods(ambig, (Int, Int))) == 1
@test length(methods(ambig, (UInt8, Int))) == 0
@test length(Base.methods_including_ambiguous(ambig, (UInt8, Int))) == 2

@test ambig("hi", "there") == 1
@test ambig(3.1, 3.2) == 5
@test ambig(3, 4) == 4
@test_throws MethodError ambig(0x03, 4)
@test_throws MethodError ambig(0x03, 4)  # test that not inserted into cache

# Ensure it still works with potential inlining
callambig(x, y) = ambig(x, y)
@test_throws MethodError callambig(0x03, 4)

# Printing ambiguity errors
let err = try
              ambig(0x03, 4)
          catch _e_
              _e_
          end
    io = IOBuffer()
    Base.showerror(io, err)
    lines = split(takebuf_string(io), '\n')
    ambig_checkline(str) = startswith(str, "  ambig(x, y::Integer) at") ||
                           startswith(str, "  ambig(x::Integer, y) at")
    @test ambig_checkline(lines[2])
    @test ambig_checkline(lines[3])
end

## Other ways of accessing functions
# Test that non-ambiguous cases work
io = IOBuffer()
@test precompile(ambig, (Int, Int)) == true
cfunction(ambig, Int, (Int, Int))
@test length(code_lowered(ambig, (Int, Int))) == 1
@test length(code_typed(ambig, (Int, Int))) == 1
code_llvm(io, ambig, (Int, Int))
code_native(io, ambig, (Int, Int))

# Test that ambiguous cases fail appropriately
@test precompile(ambig, (UInt8, Int)) == false
cfunction(ambig, Int, (UInt8, Int))  # test for a crash (doesn't throw an error)
@test_throws ErrorException code_llvm(io, ambig, (UInt8, Int))
@test_throws ErrorException code_native(io, ambig, (UInt8, Int))

# Method overwriting doesn't destroy ambiguities
@test_throws MethodError ambig(2, 0x03)
ambig(x, y::Integer) = 3
@test_throws MethodError ambig(2, 0x03)

# Automatic detection of ambiguities
module Ambig1
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
end

ambs = detect_ambiguities(Ambig1)
@test length(ambs) == 1

module Ambig2
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Number, y) = 4
end

ambs = detect_ambiguities(Ambig2)
@test length(ambs) == 2

module Ambig3
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Int, y::Int) = 4
end

ambs = detect_ambiguities(Ambig3)
@test length(ambs) == 1

module Ambig4
ambig(x, y) = 1
ambig(x::Int, y) = 2
ambig(x, y::Int) = 3
ambig(x::Int, y::Int) = 4
end
ambs = detect_ambiguities(Ambig4)
@test length(ambs) == 0

module Ambig5
ambig(x::Int8, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Int) = 3
end

ambs = detect_ambiguities(Ambig5)
@test length(ambs) == 2

# Test that Core and Base are free of ambiguities
@test (x->(isempty(x) || println(x)))(detect_ambiguities(Core, Base; imported=true))

amb_1(::Int8, ::Int) = 1
amb_1(::Integer, x) = 2
amb_1(x, ::Int) = 3
# if there is an ambiguity with some methods and not others, `methods`
# should return just the non-ambiguous ones, i.e. the ones that could actually
# be called.
@test length(methods(amb_1, Tuple{Integer, Int})) == 1

amb_2(::Int, y) = 1
amb_2(x, ::Int) = 2
amb_2(::Int8, y) = 3
@test length(methods(amb_2)) == 3  # make sure no duplicates

amb_3(::Int8, ::Int8) = 1
amb_3(::Int16, ::Int16) = 2
amb_3(::Integer, ::Integer) = 3
amb_3(::Integer, x) = 4
amb_3(x, ::Integer) = 5
# ambiguous definitions exist, but are covered by multiple more specific definitions
let ms = methods(amb_3).ms
    @test !Base.isambiguous(ms[4], ms[5])
end

amb_4(::Int8, ::Int8) = 1
amb_4(::Int16, ::Int16) = 2
amb_4(::Integer, x) = 4
amb_4(x, ::Integer) = 5
# as above, but without sufficient definition coverage
let ms = methods(amb_4).ms
    @test Base.isambiguous(ms[3], ms[4])
end

g16493{T<:Number}(x::T, y::Integer) = 0
g16493{T}(x::Complex{T}, y) = 1
let ms = methods(g16493, (Complex, Any))
    @test length(ms) == 1
    @test first(ms).sig == Tuple{typeof(g16493), Complex{TypeVar(:T, Any, true)}, Any}
end

nothing
end
