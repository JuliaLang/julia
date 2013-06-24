begin
    local n,a,asym,d,v
    n = 10
    a = rand(n,n)

    for elty in (Float32, Float64) # Complex tests fail (#3511)
        a     = convert(Matrix{elty}, a)
        asym  = a' + a                  # symmetric indefinite
        apd   = a'*a                    # symmetric positive-definite

	(d,v) = eigs(a, nev=3)
	Test.@test_approx_eq a*v[:,2] d[2]*v[:,2]

	(d,v) = eigs(asym, nev=3)
	Test.@test_approx_eq asym*v[:,1] d[1]*v[:,1]

	(d,v) = eigs(apd, nev=3)
	Test.@test_approx_eq apd*v[:,3] d[3]*v[:,3]
    end
end
