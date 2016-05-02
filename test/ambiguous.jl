# DO NOT CHANGE LINE NUMBERS BELOW
@noinline foo(x, y) = 1
@noinline foo(x::Integer, y) = 2
@noinline foo(x, y::Integer) = 3
@noinline foo(x::Int, y::Int) = 4
@noinline foo(x::Number, y) = 5
# END OF LINE NUMBER SENSITIVITY

ambigs = Any[[], [3], [2,5], [], [3]]

mt = methods(foo)

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

@test foo("hi", "there") == 1
@test foo(3.1, 3.2) == 5
@test foo(3, 4) == 4
@test_throws ErrorException foo(0x03, 4)
@test_throws ErrorException foo(0x03, 4)  # test that not inserted into cache
