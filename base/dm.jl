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
# function kernel!(middle_of_v::AbstractVector, v::AbstractVector, lo_x::Nothing, hi_x)
#     for x in v
#         if x < hi_x
#             push!(middle_of_v, x) # TODO: don't use push!, it's slow.
#         end
#     end
#     0
# end
# function kernel!(middle_of_v::AbstractVector, v::AbstractVector, lo_x, hi_x::Nothing)
#     for x in v
#         if !(x < lo_x)
#             push!(middle_of_v, x) # TODO: don't use push!, it's slow.
#         end
#     end
#     length(v)-length(middle_of_v)
# end


function my_partialsort(v::AbstractVector, target::Int)
    # length(v) < 300 && return partialsort(v, target)
    k = round(Int, length(v)^(1/3))
    sample = similar(v, k^2)
    sample_target = (target - firstindex(v)) / length(v) * length(sample)
    offset = .5k^1.15 # TODO: tune this?
    lo_i = floor(Int, sample_target - offset)
    hi_i = ceil(Int, sample_target + offset)
    middle_of_v = similar(v, 0)
    sizehint!(middle_of_v, round(Int, 2.5k^2))
    passes[] += 1
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
            middle_of_sample = partialsort!(sample, lo_i:hi_i) # TODO don't sort the middle elements
            kernel!(middle_of_v, v, first(middle_of_sample), last(middle_of_sample))
        end
        target_in_middle = target - number_below
        # println(length(middle_of_v) / 2.5k^2)
        if length(middle_of_v) < 2.5k^2
            small[] += 1
        else
            large[] += 1
        end
        if checkbounds(Bool, middle_of_v, target_in_middle)
            return partialsort!(middle_of_v, target_in_middle)
        end
        empty!(middle_of_v)
        fails[] += 1
    end
    passes[] -= 1
    epic_fails[] += 1
    partialsort(v, target)
end

const fails = Ref(0)
const passes = Ref(0)
const epic_fails = Ref(0)
const small = Ref(0)
const large = Ref(0)
reset() = (fails[] = 0; passes[] = 0; epic_fails[] = 0; small[] = 0; large[] = 0)

using Random
function run(n, ks=1:n; t=.1, m=ceil(Int, t*100_000_000 / n / length(ks)))
    reset()
    x = rand(n)
    for _ in 1:m
        rand!(x)
        for k in ks
            my_partialsort(x, k)
        end
    end
end
