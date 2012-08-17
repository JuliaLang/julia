function laplace{T}(u::Matrix{T}, dx2, dy2) 
    uout = zeros(T, size(u)) 
    for i = 2:size(u,1)-1 
        for j = 2:size(u,2)-1 
            uout[i,j] = ((u[i-1,j]+u[i+1,j])*dx2 + (u[i,j-1]+u[i,j+1])*dy2)/(2*(dx2+dy2)) 
        end 
    end 
    return uout 
end 

function laplace_iter(u, dx, dy, Niter)
    dx2 = dx^2
    dy2 = dy^2
    for i = 1:Niter 
        u = laplace(u, dx2, dy2)
    end
    return u
end 

function laplace_devec()
    N = 150
    u = zeros(N,N) 
    u[1,:] = 1 
    Niter = 2^10
    dx = 0.1 
    dy = 0.1
    u = laplace_iter(u, dx, dy, Niter)
end

function laplace_iter_vec(u, dx, dy, Niter, N)
    dx2 = dx*dx;
    dy2 = dy*dy;
    for i = 1:Niter
        u[2:N-1, 2:N-1] = ((u[1:N-2, 2:N-1] + u[3:N, 2:N-1])*dy2 + (u[2:N-1,1:N-2] + u[2:N-1, 3:N])*dx2) * (1./ (2*(dx2+dy2)));
    end
    return u
end

function laplace_vec()
    N = 150 
    u = zeros(N,N) 
    u[1,:] = 1 
    Niter = 2^10 
    dx = 0.1 
    dy = 0.1
    u = laplace_iter_vec(u, dx, dy, Niter, N)
end
