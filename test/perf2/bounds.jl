function laplacian2(A::Matrix, B::Matrix)
    d1 = 1
    d2 = size(A,1)
    for j = 2:size(B,2)-1
        offset = (j-1)*size(A,1)
        for i = 2:size(B,1)-1
            ii = offset+i
            B[ii] = A[ii+d1] + A[ii-d1] + A[ii+d2] + A[ii-d2] - 4*A[ii]
        end
    end
end

function time_laplacian(A::Matrix, niter::Int)
    B = similar(A)
    print("Laplacian of a matrix: ")
    @time begin
        for n = 1:niter
            laplacian2(A, B)
        end
    end
end

time_laplacian(randn(1000,1000), 100)

# Note: run time_laplacian with bounds-checking on and off (turn off by commenting out the middle two lines in cgutils.cpp's "emit_bounds_check"
