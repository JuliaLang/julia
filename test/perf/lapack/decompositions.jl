function svdtest(n, counts)
	A = rand(n,n)
	B = svdfact(A)
	for ct = 1:counts-1
		B = svdfact(A)
	end
	B
end

function schurtest(n, counts)
	A = rand(n,n)
	B = schurfact(A)
	for ct = 1:counts-1
		B = schurfact(A)
	end
	B
end

function choleskytest(n, counts)
	A = rand(n,n)
	A = A'*A
	B = cholfact(A)
	for ct = 1:counts-1
		B = cholfact(A)
	end
	B
end

function qrtest(n, counts)
	A = rand(n,n)
	B = qrfact(A)
	for ct = 1:counts-1
		B = qrfact(A)
	end
	B
end

function lutest(n, counts)
	A = rand(n,n)
	B = lufact(A)
	for ct = 1:counts-1
		B = lufact(A)
	end
	B
end

@timeit svdtest(2, 20_000) "svdtest_tiny" "Tiny svdfact test"
@timeit svdtest(16, 1_000) "svdtest_small" "Small svdfact test"
@timeit svdtest(64, 100) "svdtest_medium" "Medium svdfact test"
@timeit svdtest(256, 5) "svdtest_large" "Large svdfact test"
@timeit svdtest(1024, 1) "svdtest_huge" "Huge svdfact test"

@timeit schurtest(2, 20_000) "schurtest_tiny" "Tiny schurfact test"
@timeit schurtest(16, 1_000) "schurtest_small" "Small schurfact test"
@timeit schurtest(64, 100) "schurtest_medium" "Medium schurfact test"
@timeit schurtest(256, 5) "schurtest_large" "Large schurfact test"
@timeit schurtest(1024, 1) "schurtest_huge" "Huge schurfact test"

@timeit choleskytest(2, 20_000) "choleskytest_tiny" "Tiny cholfact test"
@timeit choleskytest(16, 1_000) "choleskytest_small" "Small cholfact test"
@timeit choleskytest(64, 100) "choleskytest_medium" "Medium cholfact test"
@timeit choleskytest(256, 5) "choleskytest_large" "Large cholfact test"
@timeit choleskytest(1024, 1) "choleskytest_huge" "Huge cholfact test"

@timeit qrtest(2, 100_000) "qrtest_tiny" "Tiny qrfact test"
@timeit qrtest(16, 5_000) "qrtest_small" "Small qrfact test"
@timeit qrtest(64, 500) "qrtest_medium" "Medium qrfact test"
@timeit qrtest(256, 5) "qrtest_large" "Large qrfact test"
@timeit qrtest(1024, 1) "qrtest_huge" "Huge qrfact test"

@timeit lutest(2, 100_000) "lutest_tiny" "Tiny lufact test"
@timeit lutest(16, 10_000) "lutest_small" "Small lufact test"
@timeit lutest(64, 1_000) "lutest_medium" "Medium lufact test"
@timeit lutest(256, 100) "lutest_large" "Large lufact test"
@timeit lutest(1024, 2) "lutest_huge" "Huge lufact test"
