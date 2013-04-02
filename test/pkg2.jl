include("../base/pkg2.jl")

vers = [v"0",v"1",v"2",v"3",v"4"]
ivals = [NoVersions(),AllVersions()]
for v in vers
	push!(ivals,VersionsGreater(v))
end
for v in vers, w in vers
    v < w && push!(ivals,VersionsBetween(v,w))
end
for i1 in ivals, i2 in ivals
	i3 = intersect(i1,i2)
	for v in vers
		@test !((contains(i1,v) & contains(i2,v)) $ contains(i3,v))
	end
end
