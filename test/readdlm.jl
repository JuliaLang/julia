dlm_data = readdlm(joinpath("perf/perf2", "imdb-1.tsv"), '\t')

@test size(dlm_data) == (31383,3)
@test dlm_data[12345,2] == "Gladiator"
@test dlm_data[31383,3] == 2005
@test dlm_data[1,1] == "McClure, Marc (I)"
