test_context("Options for functions")

require("options.jl")
import OptionsMod.*

test_group("basic functionality")
oo = Options(:a, true, :b, 7)
@test length(oo.key2index) == 2
@test oo[:a] == true
@test oo[:b] > 6
@test oo[:c] == nothing
@test sprint(show, oo) == "a = true, b = 7 (CheckError)"

test_group("other constructors")
oo2 = Options(CheckWarn, :(a=true), :(b=7))
@test oo2[:a] == true
oo3 = @options a=true b=7
@test oo3[:b] == 7


test_group("changing options")
oo2[:b] = 6
oo2[:c] = "cat"
@test oo2[:b] < 7
@test oo2[:c] == "cat"

test_group("simple example")
function f1(a, b, o::Options)
	@defaults o op="plus"
    if op == "plus"
        return a + b
    else
        return a - b
    end
	@check_used o
end
f1(a, b) = f1(a, b, Options())
@test f1(3, 2) == 5
@test f1(3, 2, Options(:op, "plus")) == 5
@test f1(3, 2, Options(:op, "other")) == 1


    

