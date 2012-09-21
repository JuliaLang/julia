# use with extras/test.jl

test_context("NamedIndex")

require("named.jl")
import Named.*

test_group("creation")

ni1 = NamedIndex(["one", "two", "three", "four", "five"])
@test length(ni1) == 5

test_group("access")
@test ni1["one"] == 1
@test ni1[2] == 2
@test ni1[3:4] == [3,4]
@test ni1[["one", "five"]] == [1,5]
@testfails ni1["six"] == 6
@test ni1[6] == 6            # falls through even if there's no name for this!
@test ni1[[true, false, true, false, true]] == [1,3,5]
@test ni1[:one] == ni1["one"]
@test ni1[[:one, :five]] == [1, 5]

test_group("changing names")
ni2 = copy(ni1)
@test ni1[1] == ni2[1]
names!(ni2, ["uno", "dos", "tres", "quatro", "cinco"])
@test ni2["uno"] == 1
@test ni2[2] == 2

replace_names!(ni2, ["tres", "quatro"], ["troix", "quatre"])
@test ni2["troix"] == 3
replace_names!(ni2, "cinco", "cinc")
@test ni2["cinc"] == 5
@test names(ni2) == ["uno", "dos", "troix", "quatre", "cinc"]
replace_names!(ni2, 1, "ichi")
replace_names!(ni2, [2,4], ["ni", "shi"])
@test names(ni2) == ["ichi", "ni", "troix", "shi", "cinc"]
@testfails replace_names!(ni2, 2, "ichi")

test_group("methods")
@test has(ni1, "one") == true
@test keys(ni1) == ["one", "two", "three", "four", "five"]

test_group("changing")
ni3 = copy(ni1)
push(ni3, "six")
@test ni3["six"] == 6
del(ni3, "one")
del(ni3, 5)
@test names(ni3) == ["two", "three", "four", "five"]

test_group("groups")
ni4 = copy(ni1)
set_group(ni4, "odd", ["one", "three", "five"])
@test all(ni4["odd"] .== [1, 3, 5])
@test length(get_groups(ni4)) == 1
set_groups(ni4, {"even"=>["two", "four"], "prime"=>["one", "two", "three", "five"]})
@test length(ni4["prime"]) == 4
@test isgroup(ni4, "nope") == false

test_group("UTF-8")
ni5 = copy(ni1)
replace_names!(ni5, 4, "fourty€")
@test ni5["fourty€"] == 4
@test ni5[:fourty€] == 4

test_context("NamedVector")

test_group("basics")
nams = ["one", "two", "three", "four"]
vals = [11, 22, 33, 44]
t1 = NamedVector()
for i = 1:4
    t1[nams[i]] = vals[i]
end
@test t1["one"] == 11
@test t1[2] == 22
@test t1[1:3] == [11, 22, 33]
@test t1[["four", "two"]] == [44, 22]
@test t1[:one] == 11
@test t1[[:four, :two]] == [44, 22]
@test first(t1) == 11
@test last(t1) == 44
@test sum(t1) == 110
@test sprint(show, t1) == "4-element Any NamedVector\n1, one: 11\n2, two: 22\n3, three: 33\n4, four: 44\n"

test_group("iteration")
@test all([x::Int64 for x in t1] .== [11, 22, 33, 44])
@test select(t1, 3) == 33
@test select_kv(t1, 3) == ("three", 33)

test_group("construction")
xx=NamedVector({"asdf"=>1, "qwerty"=>2})
@test typeof(xx) == NamedVector{Int64}
xx2=NamedVector(["asdf", "querty", "uiop"], [11,22,33])
@test typeof(xx2) == NamedVector{Int64}
@test xx2["querty"] == 22
@test xx2[3] == 33

test_group("copying")
t2 = copy(t1)
# neither adding nor modifying should affect t1
t2["five"] = 55
t2["one"] = 111
@test t2["five"] == 55
@test t2["one"] == 111
@testfails t1["five"]
@test t1["one"] == 11

test_group("groups")
t3 = copy(t1)
set_group(t3, "odd", ["one", "three"])
@test all(t3["odd"] .== [11, 33])
@test length(get_groups(t3)) == 1
