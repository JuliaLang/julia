function scale!(A::DArray, x::Number)
    @sync for p in procs(A)
        @spawnat p scale!(localpart(A), x)
    end
end
