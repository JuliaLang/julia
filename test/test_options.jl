test_context("Options for functions")

test_group("basic functionality")
oo = options(:a, true, :b, 7)
@test length(oo.keys) == 2
@test oo[:a] == true
@test oo[:b] > 6
@test oo[:c] == nothing
@test sprint(show, oo) == "a = true, b = 7"

test_group("adding defaults")
uu = add_defaults!(oo, :a, false, :b, 0, :c, "cat")
@test oo[:a] == true
@test oo[:b] == 7
@test oo[:c] == "cat"
@test length(uu.keys) == 0
uu2 = add_defaults!(oo, :a, false, :c, "cat", :etc)
@test length(uu2.keys) == 1
@test uu2[:b] == 7
# TODO: when test.jl allows for catching exceptions, add one for the above w/out :etc

test_group("simple example")
function f1(a, b, o::Options)
    add_defaults!(o, :op, "plus")
    let op = o[:op]
        if op == "plus"
            return a + b
        else
            return a - b
        end
    end
end
f1(a, b) = f1(a, b, options())
@test f1(3, 2) == 5
@test f1(3, 2, options(:op, "plus")) == 5
@test f1(3, 2, options(:op, "other")) == 1

test_group("example with etc")
function f2(a, b, o::Options)
    etc = add_defaults!(o, :op, "plus", :etc)
    let op = o[:op]
        if op == "plus"
            return a + f2b(b, etc)
        else
            return a - f2b(b, etc)
        end
    end
end
f2(a, b) = f2(a, b, options())
function f2b(x, o::Options)
    add_defaults!(o, :double, false)
    let double = o[:double]
        return double ? x*2 : x
    end
end
@test f2(3, 2) == 5
@test f2(3, 2, options(:op, "plus")) == 5
@test f2(3, 2, options(:op, "plus", :double, false)) == 5
@test f2(3, 2, options(:op, "plus", :double, true)) == 7

    

