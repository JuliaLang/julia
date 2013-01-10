test_context("Options for functions")

require("options")
using OptionsMod

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

test_group("complex example")
function complexfun(x, opts::Options)
    @defaults opts parent=3 both=7
    sub1, both1 = subfun1(x, opts)
    sub2, both2 = subfun2(x, opts)
    @check_used opts
    return parent, both, sub1, both1, sub2, both2
end
complexfun(x) = complexfun(x, Options())

function subfun1(x, opts::Options)
    @defaults opts sub1="sub1 default" both=0
    @check_used opts
    return sub1, both
end

function subfun2(x, opts::Options)
    @defaults opts sub2="sub2 default" both=22
    @check_used opts
    return sub2, both
end

@test complexfun(5) == (3,7,"sub1 default", 0, "sub2 default", 22)
opts = @options sub2=15
@test complexfun(5, opts) == (3,7,"sub1 default", 0, 15, 22)
@set_options opts both=8
@test complexfun(5, opts) == (3,8,"sub1 default", 8, 15, 8)
@set_options opts sub1a=5
@testfails complexfun(5, opts)
