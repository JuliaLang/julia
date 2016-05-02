# DO NOT CHANGE LINE NUMBERS BELOW
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Int, y::Int) = 4
ambig(x::Number, y) = 5
# END OF LINE NUMBER SENSITIVITY

ambigs = Any[[], [3], [2,5], [], [3]]

mt = methods(ambig)

getline(m::Method) = m.line - 1  # -1 for the comment at the top

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

@test length(methods(ambig, (Int, Int))) == 1
@test length(methods(ambig, (UInt8, Int))) == 2

@test ambig("hi", "there") == 1
@test ambig(3.1, 3.2) == 5
@test ambig(3, 4) == 4
@test_throws MethodError ambig(0x03, 4)
@test_throws MethodError ambig(0x03, 4)  # test that not inserted into cache

# Ensure it still works with potential inlining
callambig(x, y) = ambig(x, y)
@test_throws MethodError callambig(0x03, 4)

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
@test length(code_lowered(ambig, (UInt8, Int))) == 2
@test length(code_typed(ambig, (UInt8, Int))) == 2
@test_throws ErrorException code_llvm(io, ambig, (UInt8, Int))
@test_throws ErrorException code_native(io, ambig, (UInt8, Int))

# Method overwriting doesn't destroy ambiguities
@test_throws MethodError ambig(2, 0x03)
ambig(x, y::Integer) = 3
@test_throws MethodError ambig(2, 0x03)

nothing
