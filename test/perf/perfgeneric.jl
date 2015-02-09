#Generic benchmark driver
for (testfunc, testname, longtestname, problem_sizes) in testdata
    for (n, t, size) in problem_sizes
        @timeit testfunc(n, t) string(testname,"_",size) string(uppercase(size[1]),size[2:end]," ",longtestname," test")
    end
end
