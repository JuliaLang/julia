
@testset "loops" begin

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

@test_throws JuliaLowering.LoweringError JuliaLowering.include_string(test_mod, """
break
""")

@test_throws JuliaLowering.LoweringError JuliaLowering.include_string(test_mod, """
continue
""")

# TODO: Test scope rules

end
