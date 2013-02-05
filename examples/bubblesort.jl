importall Base

type BubbleSort <: Sort.Algorithm end

function sort!(::BubbleSort, o::Sort.Ordering, v::AbstractVector, lo::Int, hi::Int)
	while true
		clean = true
		for i = lo:hi-1
			if Sort.lt(o, v[i+1], v[i])
				v[i+1], v[i] = v[i], v[i+1]
				clean = false
			end
		end
		clean && break
	end
	return v
end
