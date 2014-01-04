
# Supporting routines

@test Base.rcompress_dims((3, 4), 1) == (true,  [3, 4])
@test Base.rcompress_dims((3, 4), 2) == (false, [3, 4])
@test Base.rcompress_dims((3, 4), 3) == (false, [12])
@test Base.rcompress_dims((3, 4), (1, 2)) == (true, [12])

@test Base.rcompress_dims((3, 4, 5), 1) == (true,  [3, 20])
@test Base.rcompress_dims((3, 4, 5), 2) == (false, [3, 4, 5])
@test Base.rcompress_dims((3, 4, 5), 3) == (false, [12, 5])
@test Base.rcompress_dims((3, 4, 5), (1, 2)) == (true,  [12, 5])
@test Base.rcompress_dims((3, 4, 5), (1, 3)) == (true,  [3, 4, 5])
@test Base.rcompress_dims((3, 4, 5), (2, 3)) == (false, [3, 20])
@test Base.rcompress_dims((3, 4, 5), (1, 2, 3)) == (true, [60])

@test Base.rcompress_dims((3, 4, 5, 2), 1) == (true,  [3, 40])
@test Base.rcompress_dims((3, 4, 5, 2), 2) == (false, [3, 4, 10])
@test Base.rcompress_dims((3, 4, 5, 2), 3) == (false, [12, 5, 2])
@test Base.rcompress_dims((3, 4, 5, 2), 4) == (false, [60, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 2)) == (true,  [12, 10])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 3)) == (true,  [3, 4, 5, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 4)) == (true,  [3, 20, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (2, 3)) == (false, [3, 20, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (2, 4)) == (false, [3, 4, 5, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (3, 4)) == (false, [12, 10])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 2, 3)) == (true,  [60, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 2, 4)) == (true,  [12, 5, 2])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 3, 4)) == (true,  [3, 4, 10])
@test Base.rcompress_dims((3, 4, 5, 2), (2, 3, 4)) == (false, [3, 40])
@test Base.rcompress_dims((3, 4, 5, 2), (1, 2, 3, 4)) == (true, [120])

# main tests

safe_sum{T}(A::Array{T}, region) = reducedim(+,A,region,zero(T))

Areduc = rand(3, 4, 5, 6)
for region in {
	1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4), 
	(1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)}

	@test_approx_eq sum(Areduc, region) safe_sum(Areduc, region)
end

