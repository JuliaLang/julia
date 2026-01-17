path = joinpath(@__DIR__, "../extras/CompilerDevTools")
@test run(`$(Base.julia_cmd()) --project=$path -e "using Pkg; Pkg.test()"`).exitcode == 0
