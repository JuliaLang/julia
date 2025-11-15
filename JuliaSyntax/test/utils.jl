@testset "_printstyled" begin
    ps(str; kws...) = sprint(io->JuliaSyntax._printstyled(IOContext(io, :color=>true), str; kws...))

    @test ps("XX"; fgcolor=:red) == "\e[31mXX\e[0;0m"
    @test ps("XX"; fgcolor=42)   == "\e[38;5;42mXX\e[0;0m"
    @test ps("XX"; fgcolor=(10,100,200)) == "\e[38;2;10;100;200mXX\e[0;0m"

    ps("XX"; bgcolor=:red) == "\e[41mXX\e[0;0m"
    @test ps("XX"; bgcolor=42) == "\e[48;5;42mXX\e[0;0m"
    @test ps("XX"; bgcolor=(10,100,200)) == "\e[48;2;10;100;200mXX\e[0;0m"

    @test ps("XX"; href="https://www.example.com") ==
        "\e]8;;https://www.example.com\e\\XX\e[0;0m\e]8;;\e\\"

    @test ps("XX", fgcolor=:red, bgcolor=:green, href="https://www.example.com") ==
        "\e]8;;https://www.example.com\e\\\e[31m\e[42mXX\e[0;0m\e]8;;\e\\"
end

@testset "ambiguities" begin
    if VERSION >= v"1.8"
        @test detect_ambiguities(JuliaSyntax) == []
        @test detect_unbound_args(JuliaSyntax) == []
    end
end
