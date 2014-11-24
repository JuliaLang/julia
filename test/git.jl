import Base.Git
include("gitutils.jl")

@test Git.version() >= v"1.7.3"

dir = string("tmp.",randstring())
@test !ispath(dir)
mkdir(dir)
@test isdir(dir)
try cd(dir) do

    run(`git init -q`)
    run(`git config user.name "Julia Tester"`)
    run(`git config user.email test@julialang.org`)
    run(`git commit -q --allow-empty -m "initial empty commit"`)
    git_verify(Dict(), Dict(), Dict())

    # each path can have one of these content in each of head, index, work
    # for a total of length(contents)^3 = 4^3 = 64 combinations.
    # each path can be in any of these 64 "superpositions" before & after
    # for a total of 64^2 = 4096 files needed to test all transitions
    # between before and after superpositions of git repo states.

    contents = [nothing, "foo", "bar", Dict{Any,Any}("baz"=>"qux")]
    b = length(contents)
    states = [ [ base(b,k,6) => contents[rem(div(k,b^p),b)+1] for k=0:(b^3)^2-1 ] for p=0:5 ]

    git_setup(states[1:3]...)
    try Git.transact() do
        git_setup(states[4:6]...)
        throw(nothing)
    end catch x
        is(x,nothing) || rethrow()
    end
    git_verify(states[1:3]...)

end finally rm(dir, recursive=true) end
