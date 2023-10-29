function kernel!(middle_of_v::AbstractVector, v::AbstractVector, lo_x, hi_x)
    number_below = 0
    for x in v
        a = lo_x !== nothing && x < lo_x
        b = hi_x === nothing || x < hi_x
        number_below += Int(a)
        if a != b
            push!(middle_of_v, x) # TODO: don't use push!, it's slow.
        end
    end
    number_below
end

function my_partialsort(v::AbstractVector, target::Int)
    length(v) < 300 && return partialsort(v, target)
    k = round(Int, length(v)^(1/3))
    sample = similar(v, k^2)
    sample_target = (target - firstindex(v)) / length(v) * length(sample)
    offset = .5k^1.15 # TODO for further optimization: tune this
    lo_i = floor(Int, sample_target - offset)
    hi_i = ceil(Int, sample_target + offset)
    middle_of_v = similar(v, 0)
    sizehint!(middle_of_v, round(Int, 2.5k^2)) # TODO for further optimization: tune this
    for i in 1:4
        seed = hash(i)
        for j in eachindex(sample)
            sample[j] = v[mod(hash(j, seed), eachindex(v))]
        end
        number_below = if lo_i <= firstindex(sample) && lastindex(sample) <= hi_i
            error("too small")
        elseif lo_i <= firstindex(sample)
            hi_x = partialsort!(sample, hi_i)
            kernel!(middle_of_v, v, nothing, hi_x)
        elseif lastindex(sample) <= hi_i
            lo_x = partialsort!(sample, lo_i)
            kernel!(middle_of_v, v, lo_x, nothing)
        else
            # TODO for further optimization: don't sort the middle elements
            middle_of_sample = partialsort!(sample, lo_i:hi_i)
            kernel!(middle_of_v, v, first(middle_of_sample), last(middle_of_sample))
        end
        target_in_middle = target - number_below
        if checkbounds(Bool, middle_of_v, target_in_middle)
            return partialsort!(middle_of_v, target_in_middle)
        end
        empty!(middle_of_v)
    end
    partialsort(v, target)
end

function kernel!(v::AbstractVector, lo_x, hi_x)
    i = 0
    number_below = 0
    for j in eachindex(v)
        x = v[j]
        a = lo_x !== nothing && x < lo_x
        b = hi_x === nothing || x < hi_x
        number_below += a
        # if a != b # This branch is almost never taken, so making it branchless is bad.
        #     v[i], v[j] = v[j], v[i]
        #     i += 1
        # end
        c = a != b # JK, this is faster.
        k = i * c + j
        @inbounds v[j], v[k] = v[k], v[j]
        i += c - 1
    end
    number_below, i+lastindex(v)
end

function move!(v, target, source)
    @assert length(target) == length(source)
    if length(target) == 1 || isdisjoint(target, source)
        for (i, j) in zip(target, source)
            v[i], v[j] = v[j], v[i]
        end
    else
        @assert first(source) <= first(target)
        reverse!(v, first(source), last(target))
        reverse!(v, first(target), last(target))
    end
end

# Uses PartialQuickSort recursively even though ScratchQuickSort is faster because the
# runtime of recursive calls is negligible for large inputs and using PartialQuickSort
# allows this algorithm to be non-allocating.
function my_partialsort!(v::AbstractArray, target)
    # length(v) < 300 && return partialsort!(v, target)
    k = round(Int, length(v)^(1/3))
    sample = view(v, 1:k^2)
    sample_target = (target .- firstindex(v)) ./ length(v) .* length(sample)
    offset = .5k^1.15 # TODO for further optimization: tune this
    lo_i = floor(Int, first(sample_target) - offset)
    hi_i = ceil(Int, last(sample_target) + offset)
    for attempt in 1:4
        seed = hash(attempt)
        for i in firstindex(v):firstindex(v)+k^2-1
            j = mod(hash(i, seed), i:lastindex(v)) # TODO for further optimization: be sneaky and remove this division
            v[i], v[j] = v[j], v[i]
        end
        number_below, lastindex_middle = if lo_i <= firstindex(sample) && lastindex(sample) <= hi_i
            # error("too small")
            0, lastindex(v)
        elseif lo_i <= firstindex(sample)
            sort!(sample, alg=PartialQuickSort(hi_i))
            kernel!(v, nothing, sample[hi_i])
        elseif lastindex(sample) <= hi_i
            sort!(sample, alg=PartialQuickSort(lo_i))
            kernel!(v, sample[lo_i], nothing)
        else
            # TODO for further optimization: don't sort the middle elements
            sort!(sample, alg=PartialQuickSort(lo_i:hi_i))
            kernel!(v, sample[lo_i], sample[hi_i])
        end
        target_in_middle = target .- number_below
        if firstindex(v) <= first(target_in_middle) && last(target_in_middle) <= lastindex_middle
            if target isa Number
                sort!(view(v, firstindex(v):lastindex_middle), alg=PartialQuickSort(target_in_middle))
            else
                # default `partialsort!` is substantially faster than partial quicksort
                # and dominates runtime when sorting for a large proportion of the input.
                partialsort!(view(v, firstindex(v):lastindex_middle), target_in_middle)
            end
            # sort!(view(v, lastindex_middle+1:lastindex(v)))
            move!(v, target, target_in_middle)
            return Base.Sort.maybeview(v, target)
        end
    end
    # println("CRIT FAIL!")
    sort!(v, alg=PartialQuickSort(target))
    v[target]
end


# TESTING

using Random
function report_error(x, target)
    println("x = ", x)
    println("target = ", target)
    # println("my_partialsort(x, target) = ", my_partialsort(x, target))
    println("partialsort(x, target) = ", partialsort(x, target))
    println("my_partialsort!(copy(x), target) = ", my_partialsort!(copy(x), target))
    println("partialsort!(copy(x), target) = ", partialsort!(copy(x), target))
    error("Bad sort")
end

# my_partialsort!([0.3094212, 0.5471155, 0.11776853, 0.23952103, 0.73947215, 0.15547325, 0.36727393, 0.116280936, 0.19844331, 0.7175033], 9:10)
# my_partialsort!([-4489, 18997, -20308, 10695, 13292, 4709, 1125, -10670, -10552], 8:9)
function test()
    for n in 1:1000
        x = rand(n)
        for i in (n < 200 ? (1:n) : rand(1:n, 7))
            rand!(x)
            st = sort(x)
            st[i] == my_partialsort!(copy(x), i) || report_error(x, i)
            if n < 70
                for j in i:n
                    st[i:j] == my_partialsort!(copy(x), i:j) || report_error(x, i:j)
                end
            else
                j = rand(1:n)
                i, j = extrema((i, j))
                st[i:j] == my_partialsort!(copy(x), i:j) || report_error(x, i:j)
            end
        end
    end
end


# TUNING

const DATA = []
function compare(n, t)
    a = 0
    b = 0
    for _ in 1:100
        x = rand(Int, n)
        ta = my_elapsed(partialsort!, x, t)
        tb = my_elapsed(my_partialsort!, x, t)*1.10 # assume I over-optimized for my system by 10%
        #println((ta, tb))
        a += ta^3
        b += tb^3
    end
    res = (b/a)^(1/3)
    push!(DATA, (n, t, res))
    res
end
function my_elapsed(f!::T, x, t, tt=(length(x)+10), evals=max(1, 10^4÷tt), samples=max(1, 3*10^5÷evals÷tt)) where T
    mn = Inf
    for _ in 1:samples
        elapse = @elapsed for _ in 1:evals
            f!(copy(x), t)
        end
        mn = min(mn, elapse)
    end
    mn
end
using Printf
function report(n, t; newline=true, filter=false)
    res = compare(n, t)
    my = choose(n, t)
    str = (@sprintf "%8.5f" res) * (my ? " m" : "  ")
    if .95 < res < 1.05
        filter || print(str)
    elseif (res < 1) == my
        filter || printstyled(str, color=:green)
    else
        filter && print(n, " ", t, " ")
        printstyled(str, color=abs(res-1) > .1 ? :red : :yellow)
        filter && println()
    end
    newline && !filter && println()
end


function choose(n, target)
    v = 1:n
    k = round(Int, length(v)^(1/3))
    sample_target = (target .- firstindex(v)) ./ length(v) .* k^2
    offset = .5k^1.15 # TODO for further optimization: tune this
    lo_i = max(1, floor(Int, first(sample_target) - offset))
    hi_i = min(k^2, ceil(Int, last(sample_target) + offset))
    expected_len = (hi_i - lo_i + 1) * length(v) / k^2
    # length(v) > #=2k^2+82=#200 + 2expected_len
    length(v) > 2k^2+130 + 2expected_len
end

function report_precomputed()
    bad = 0
    very_bad = 0
    for (n, t, res) in DATA
        my = choose(n, t)
        str = (@sprintf "%8.5f" res) * (my ? " m" : "  ")
        if .95 < res < 1.05
        elseif (res < 1) == my
        else
            red = abs(res-1) > .1
            printstyled(n, " ", length(t), " ", t, " ", str, "\n", color=red ? :red : :yellow)
            bad += 1
            very_bad += red
        end
    end
    println((bad, very_bad))
end

function gather_data()
    while true
        n = round(Int, exp(rand()*log(100_000)))
        len = round(Int, rand()^3*n)
        i = rand(1:n-len+1)
        j = i+len-1
        k = rand(1:n)
        #print(n, " ", rpad(i:j, 8));
        report(n, i:j, filter=true); report(n, k, filter=true)
        print(".")
    end
end