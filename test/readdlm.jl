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
