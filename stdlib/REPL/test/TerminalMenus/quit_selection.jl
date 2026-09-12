# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "Quit Selection" begin
    menu = RadioMenu(["a", "b", "c"]; warn=false)
    @test simulate_input(menu, 'q') == -1

    menu = MultiSelectMenu(["a", "b", "c"]; warn=false)
    @test simulate_input(menu, 'q') == Set{Int}()

    menu = RadioMenu(["a", "b", "c"]; charset=:ascii)
    @test simulate_input(menu, 'q') == -1

    menu = MultiSelectMenu(["a", "b", "c"]; charset=:ascii)
    @test simulate_input(menu, 'q') == Set{Int}()

    menu = RadioMenu(["a", "b", "c"]; on_cancel=nothing)
    @test simulate_input(menu, 'q') |> isnothing

    menu = MultiSelectMenu(["a", "b", "c"]; on_cancel=nothing)
    @test simulate_input(menu, 'q') |> isnothing
end
