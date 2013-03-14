# Test OrderedDict creation
od = OrderedDict{String,Int}()
for i = 1:factorial(5)
   od[join(nthperm(split("abcde",""),i))] = i
end
@test length(od) == factorial(5)

ks = String[join(nthperm(split("abcde",""),i)) for i = 1:factorial(5)]
vs = 1:factorial(5)
od2 = OrderedDict(ks,vs)
od3 = similar(od2)
sizehint(od3, length(od2.slots))
for item in od2
    push!(od3, item)
end
od4 = copy(od2)
od5 = copy(od2)

# Test equality
@test od == od2

# Key-value access
@test keys(od) == ks
@test values(od) == vs

@test all([(indexof(od, ks[i]) == i) for i = 1:factorial(5)])

# Index access
@test od4[1] == ("abcde", 1)
@test od4[120] == ("edcba", 120)

@test all([od[i] == (join(nthperm(split("abcde",""),i)),i) for i = 1:factorial(5)])

# Deletion, compaction
for i = 21:factorial(5)
    delete!(od, ks[i])
end

@test all([(indexof(od, ks[i]) == i) for i = 1:20])
@test length(od) == 20

for i = 2:2:factorial(5)
    delete!(od2, ks[i])   # delete by key
    delete!(od3, i)       # delete by numerical index
end

@test all([(indexof(od2, ks[i]) == int(i/2)) for i = 1:2:factorial(5)])
@test length(od2) == 60
@test od2 == od3

@test isempty(od) == false
for i = 1:20
    delete!(od, ks[i])
end
@test isempty(od) == true

@test isempty(od2) == false
empty!(od2)
@test isempty(od2) == true

@test collect(od4) == [(k,v) for (k,v) in zip(ks,vs)]

## Serialize/deserialize
# TODO: fix this!
#io = IOString()
#serialize(io, od4)
#seek(io, 0)
#od6 = deserialize(io)
#@test od4 == od6

# first, last, reverse
@test first(od4) == od4[1] == ("abcde", 1)
@test last(od4) == od4[end] == ("edcba", 120)
@test reverse!(od4) == reverse(od5)
@test reverse!(od4) == od5

# push, pop, shift, unshift
@test push!(od4, ("hi", 0)) == ("hi", 0)
@test od4[length(od4)] == ("hi", 0)
@test unshift!(od4, ("bye", 100)) == ("bye", 100)
@test od4[1] == ("bye", 100)

@test pop!(od4) == ("hi", 0)
@test shift!(od4) == ("bye", 100)
@test od4 == od5

# Comparison with, construction from Dict
d = Dict(ks,vs)
@test d == od4  # should this be true???
@test od4 == d  # should this be true???

# Sorting
odd = OrderedDict(d)
sort!(odd)
sd = sort(d)

@test odd == sd
@test keys(odd) == ks
@test keys(sd) == ks
@test values(odd) == vs
@test values(sd) == vs

sort!(odd, Sort.Reverse)
@test keys(odd) == reverse(ks)
@test values(odd) == reverse(vs)

sortby!(odd, x -> odd[x])  # sort by value
@test odd == sd
