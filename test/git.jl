import Base.Git
include("gitutils.jl")

dir = string("tmp.",randstring())
@test !ispath(dir)
mkdir(dir)
@test isdir(dir)
try cd(dir) do

    run(`git init -q`)
    run(`git commit -q --allow-empty -m "initial empty commit"`)
    verify_tree(Dict(), "HEAD")
    verify_work(Dict())

    # each path can have one of these content in each of head, index, work
    # for a total of length(contents)^3 = 4^3 = 64 combinations.
    # each path can be in any of these 64 "superpositions" before & after
    # for a total of 64^2 = 4096 files needed to test all transitions
    # between before and after superpositions of git repo states.

    contents = [nothing, "foo", "bar", {"baz"=>"qux"}]
    b = length(contents)
    files = 0:(b^3)^2-1
    states = [ [ base(b,k,6) => contents[rem(div(k,b^p),b)+1] for k in files ] for p=0:5 ]

    git_setup(states[1:3]...)
    try Git.transact() do
        git_setup(states[4:6]...)
        throw(nothing)
    end catch x
        is(x,nothing) || rethrow()
    end
    git_verify(states[1:3]...)

end finally run(`rm -rf $dir`) end
