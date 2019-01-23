module PlotlyBaseTest

using Test
using Dates

@static if VERSION >= v"0.7.0-alpha"
    using DelimitedFiles: readdlm
end


using PlotlyBase
const M = PlotlyBase

# copied from TestSetExtensions.@includetests
macro includetests(testarg...)
    if length(testarg) == 0
        tests = []
    elseif length(testarg) == 1
        tests = testarg[1]
    else
        error("@includetests takes zero or one argument")
    end

    quote
        tests = $tests
        rootfile = @__FILE__
        if length(tests) == 0
            tests = readdir(dirname(rootfile))
            tests = filter(f->endswith(f, ".jl") && f!= basename(rootfile), tests)
        else
            tests = map(f->string(f, ".jl"), tests)
        end
        println();
        for test in tests
            print(splitext(test)[1], ": ")
            include(test)
            println()
        end
    end
end


#try
    @testset "PlotlyJS Tests" begin
        @includetests ARGS
    end
#catch
#    exit(-1)
#end

end
