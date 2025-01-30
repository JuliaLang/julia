
@testset "while loops" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    i = 0
    while i < 5
        i = i + 1
        push!(a, i)
    end
    a
end
""") == [1,2,3,4,5]

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    i = 0
    while i < 5
        i = i + 1
        if i == 3
            break
        end
        push!(a, i)
    end
    a
end
""") == [1,2]

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    i = 0
    while i < 5
        i = i + 1
        if isodd(i)
            continue
        end
        push!(a, i)
    end
    a
end
""") == [2,4]

end

@testset "for loops" begin

test_mod = Module()

# iteration
@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:3
        push!(a, i)
    end
    a
end
""") == [1,2,3]

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:0
        push!(a, i)
    end
    a
end
""") == []

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for _ = 1:3
        push!(a, 1)
    end
    a
end
""") == [1, 1, 1]

# break
@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:6
        if i == 3
            break
        end
        push!(a, i)
    end
    a
end
""") == [1, 2]

# continue
@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:6
        if isodd(i)
            continue
        end
        push!(a, i)
    end
    a
end
""") == [2, 4, 6]

# Loop variable scope
@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:3
        push!(a, i)
        i = 100
    end
    a
end
""") == [1,2,3]

@test JuliaLowering.include_string(test_mod, """
let
    i = 100
    for i = 1:3
    end
    i
end
""") == 100

@test JuliaLowering.include_string(test_mod, """
let
    i = 100
    for outer i = 1:2
        nothing
    end
    i
end
""") == 2

# Fancy for loop left hand side - unpacking and scoping
@test JuliaLowering.include_string(test_mod, """
let
    a = []
    i = 100
    j = 200
    for (i,j) in [('a', 'b'), (1,2)]
        push!(a, (i,j))
    end
    (a, i, j)
end
""") == ([('a', 'b'), (1,2)], 100, 200)

end


@testset "multidimensional for loops" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:2, j = 3:4
        push!(a, (i,j))
    end
    a
end
""") == [(1,3), (1,4), (2,3), (2,4)]

@testset "break/continue" begin

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:2, j = 3:4
        push!(a, (i,j))
        break
    end
    a
end
""") == [(1,3)]

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:4, j = 3:4
        if isodd(i)
            continue
        end
        push!(a, (i,j))
    end
    a
end
""") == [(2,3), (2,4), (4,3), (4,4)]

@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:2, j = 1:4
        if isodd(j)
            continue
        end
        push!(a, (i,j))
    end
    a
end
""") == [(1,2), (1,4), (2,2), (2,4)]


end


@testset "Loop variable scope" begin

# Test that `i` is copied in the inner loop
@test JuliaLowering.include_string(test_mod, """
let
    a = []
    for i = 1:2, j = 3:4
        push!(a, (i,j))
        i = 100
    end
    a
end
""") == [(1,3), (1,4), (2,3), (2,4)]

@test JuliaLowering.include_string(test_mod, """
let
    i = 100
    j = 200
    for i = 1:2, j = 3:4
        nothing
    end
    (i,j)
end
""") == (100,200)

@test JuliaLowering.include_string(test_mod, """
let
    i = 100
    j = 200
    for outer i = 1:2, j = 3:4
        nothing
    end
    (i,j)
end
""") == (2,200)

@test JuliaLowering.include_string(test_mod, """
let
    i = 100
    j = 200
    for i = 1:2, outer j = 3:4
        nothing
    end
    (i,j)
end
""") == (100,4)

end

end
