mutable struct t109
    i::Int
end

let iob = IOBuffer()
    JSON.print(iob, t109(1))
    @test get(JSON.parse(String(take!(iob))), "i", 0) == 1
end
