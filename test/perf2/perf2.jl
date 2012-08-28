macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $ex)
        end
        println($name, "\t", t*1000)
    end
end

macro timeit1(ex,name)
    quote
        println($name, "\t", (@elapsed $ex)*1000)
    end
end

srand(1776)  # get more consistent times

require("$JULIA_HOME/../../examples/list.jl")

function listn1n2(n1::Int64,n2::Int64)
    l1 = Nil{Int64}()
    for i=n2:-1:n1
        l1 = Cons{Int64}(i,l1)
    end
    l1
end

@timeit listn1n2(1,10^6) "cons    "
gc()

# issue #1211
load("ziggurat.jl")
a = Array(Float64, 1000000)
@timeit randn_zig!(a) "randn_zig"

# issue #950
load("gk.jl")
@timeit gk(350,[0.1]) "gk      "

# issue #942
require("linalg_sparse.jl")
s = sparse(ones(280,280));
@timeit s*s "sparsemul"

# issue #939
y = [500000:-1:1];
@timeit sortperm(y) "sortperm"

# issue #938
x = 1:600000;
@timeit sparse(x,x,x) "sparserang"

# issue #445
load("stockcorr.jl")
@timeit stockcorr() "stockcorr"

# issue #1163
load("actor_centrality.jl")
@timeit1 actor_centrality() "actorgraph"

# issue #1168
load("laplace.jl")
@timeit1 laplace_vec() "laplace_vec"
@timeit laplace_devec() "laplace_devec"

# issue #1169
load("go_benchmark.jl")
@timeit1 benchmark(10) "go_benchmark"

function cmp_with_func(x::Vector, f::Function)
    count::Int = 0
    for i = 1:length(x)-1
        if f(x[i], x[i+1])
            count += 1
        end
    end
    return count
end

x = randn(200_000)
@timeit (for n in 1:10; count = cmp_with_func(x, isless) end) "funarg  "


function arith_vectorized(b,c,d)
    a = b.*c + d + 1.0
end

len = 1_000_000
b = randn(len)
c = randn(len)
d = randn(len)

@timeit (for n in 1:10; a = arith_vectorized(b,c,d); end) "vectoriz"


function parse()
    file = EachLine(open("random.csv"))
    for line in file
        line = split(line, ',')
    end
end

@timeit parse() "splitline"

