dlm_data = readdlm(joinpath("perf", "kernel", "imdb-1.tsv"), '\t')

@test size(dlm_data) == (31383,3)
@test dlm_data[12345,2] == "Gladiator"
@test dlm_data[31383,3] == 2005
@test dlm_data[1,1] == "McClure, Marc (I)"

@test size(readcsv(IOBuffer("1,2,3,4"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,4\n"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,\n"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,4\n1,2,3,4"))) == (2,4)
@test size(readcsv(IOBuffer("1,2,3,4\n1,2,3,"))) == (2,4)
@test size(readcsv(IOBuffer("1,2,3,4\n1,2,3"))) == (2,4)

@test size(readdlm(IOBuffer("1 2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t 2 3 4\n1 2 3"))) == (2,5)
@test size(readdlm(IOBuffer("1\t 2 3 4\n1 2 3\n"))) == (2,5)

let x = [1,2,3], y = [4,5,6], io = IOBuffer()
    writedlm(io, zip(x,y), ",  ")
    seek(io, 0)
    @test readcsv(io) == [x y]
end
