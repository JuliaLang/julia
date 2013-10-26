include("../perfutil.jl")

function cat2d_perf(n, iter)
  a = rand(n,n)
  b = rand(n,n)
  for i=1:iter
    c = [a b; b a]
  end
end

function cat2d_perf2(n, iter)
  a = rand(n,n)
  b = rand(n,n)
  for i=1:iter
    c = Array(Float64,2n,2n)
    c[  1:n,  1:n] = a
    c[  1:n,  n+1:end] = b
    c[n+1:end,1:n] = b
    c[n+1:end,n+1:end] = a
  end
end


function hcat_perf(n, iter)
  a = rand(n,n)
  b = rand(n,n)
  for i=1:iter
    c = [a b b a]
  end
end

function hcat_perf2(n, iter)
  a = rand(n,n)
  b = rand(n,n)
  for i=1:iter
    c = Array(Float64, n, 4n)
    c[:,    1:  n] = a
    c[:,  n+1: 2n] = b
    c[:, 2n+1: 3n] = b
    c[:, 3n+1:end] = a
  end
end

function vcat_perf(n, iter)
  a = rand(n,n)
  b = rand(n,n)
  for i = 1:iter
    c = [a, b, b, a]
  end
end

function vcat_perf2(n, iter)
  a = rand(n,n)
  b = rand(n,n)
  for i=1:iter
    c = Array(Float64, 4n, n)
    c[   1: n, :] = a
    c[ n+1:2n, :] = b
    c[2n+1:3n, :] = b
    c[3n+1:4n, :] = a
  end
end

function catnd_perf(n, iter)
  a = rand(1,n,n,1)
  b = rand(1,n,n)
  for i = 1:iter
    c = cat(3, a, b, b, a)
  end
end

function catnd_perf2(n, iter)
  a = rand(1,n,n,1)
  b = rand(1,n,n)
  for i = 1:iter
    c = Array(Float64, 1, n, 4n, 1)
    c[1,:,   1: n,1] = a
    c[1,:, n+1:2n,1] = b
    c[1,:,2n+1:3n,1] = b
    c[1,:,3n+1:4n,1] = a
  end
end

for (testfunc, testname, longtestname) in [
                                           (cat2d_perf,  "hvcat",        "horizontal/vertical matrix concatenation"),
                                           (cat2d_perf2, "hvcat_setind", "horizontal/vertical matrix concatenation using setindex"),
                                           (hcat_perf,   "hcat",         "horizontal matrix concatenation"),
                                           (hcat_perf2,  "hcat_setind",  "horizontal matrix concatenation using setindex"),
                                           (vcat_perf,   "vcat",         "vertical matrix concatenation"),
                                           (vcat_perf2,  "vcat_setind",  "vertical matrix concatenation using setindex"),
                                           (catnd_perf,  "catnd",        "N-dimensional matrix concatenation"),
                                           (catnd_perf2, "catnd_setind", "N-dimensional matrix concatenation using setindex")]
  for (n, t, size) in [(5  , 20000, "small"),
                       (500, 2, "large")]
    @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
  end
end

