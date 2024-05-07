#!/usr/bin/env -S julia --project=@scriptdir

module Main2

using Core: OpaqueClosure
using Base: @ccallable
using Base.Experimental: @opaque

function make_offset_func(y::Float64)
    # why does the implicit convert here not work?
    return (@opaque (x::Float64)->(x + y))::OpaqueClosure{Tuple{Float64}, Float64}
end

@noinline function run_all(callables)
    input = rand(Float64)
    println(Core.stdout, "test input: ", string(input))
    for (i, (oc,)) in enumerate(callables)
        result = oc(input)
        println(Core.stdout, "callable #", string(i), " gave result: ", string(result))
    end
end

const OC = OpaqueClosure{Tuple{Float64},Float64}

@ccallable function main()::Cint

    oc = @noinline make_offset_func(1.5)
    println(Core.stdout, "got: ", string(oc(1.5)))

    callables = Tuple{OpaqueClosure{Tuple{Float64},Float64}}[]
    push!(callables, (make_offset_func(1.0),))
    push!(callables, (make_offset_func(2.5),))
    push!(callables, ((@opaque (x::Float64)->2x)::OC,))
    push!(callables, ((@opaque (x::Float64)->begin
        println(Core.stdout, "Got argument: ", string(x))
        return x
    end)::OC,))

    run_all(callables)

    return 0
end

end
