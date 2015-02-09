function laplace_iter_devec(u, dx2, dy2, Niter, N)
    uout = copy(u)
    for iter = 1:Niter
        for i = 2:N-1
            for j = 2:N-1
                uout[i,j] = ( (u[i-1,j]+u[i+1,j])*dy2 + (u[i,j-1]+u[i,j+1])*dx2 ) * (1./(2*(dx2+dy2)))
            end
        end
        u, uout = uout, u
    end
    return u
end

function laplace_devec()
    N = 150
    u = zeros(N, N)
    u[1,:] = 1
    Niter = 2^10
    dx2 = dy2 = 0.1*0.1
    u = laplace_iter_devec(u, dx2, dy2, Niter, N)
end

function laplace_iter_vec(u, dx2, dy2, Niter, N)
    for i = 1:Niter
        u[2:N-1, 2:N-1] = ((u[1:N-2, 2:N-1] + u[3:N, 2:N-1])*dy2 + (u[2:N-1,1:N-2] + u[2:N-1, 3:N])*dx2) * (1./ (2*(dx2+dy2)))
    end
    return u
end

function laplace_vec()
    N = 150
    u = zeros(N,N)
    u[1,:] = 1
    Niter = 2^10
    dx2 = dy2 = 0.1*0.1
    u = laplace_iter_vec(u, dx2, dy2, Niter, N)
end
