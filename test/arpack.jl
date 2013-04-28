begin
	local n,a,asym,d,v
	n = 10
	a = rand(n,n)
	asym = a' * a

	(d,v) = eigs(asym; nev=3)
	@test sum(asym*v[:,1]-d[1]*v[:,1]) < 1e-8

	(d,v) = eigs(a; nev=3)
	@test abs(sum(a*v[:,2]-d[2]*v[:,2])) < 1e-8
end
