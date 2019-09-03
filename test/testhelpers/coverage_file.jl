# This file is a part of Julia. License is MIT: https://julialang.org/license

function code_coverage_test()
    if rand(1:2) == 3
        return "hello"
    else
        r = Int[]
        for i = 1:3
            push!(r, i)
        end
        return r
    end
end

exit(code_coverage_test() == [1, 2, 3] ? 0 : 1)

# end of file
