% 3D Laplace equation solver

nx = 290;
ny = 290;
nz = 290;
iters = 100;
sixth = 1.0/6.0;

u1 = zeros(nx, ny, nz);
u2 = zeros(nx, ny, nz);

for k3 = 1:nz
    for k2 = 1:ny
        for k1 = 1:nx
            if k1==1 || k1==nx || k2==1 || k2==ny || k3==1 || k3==nz
                u1(k1,k2,k3) = 1.0;
                u2(k1,k2,k3) = 1.0;
            end
        end
    end
end

tic
for n = 1:iters
    for k3 = 2:nz-1
        for k2 = 2:ny-1
            for k1 = 2:nx-1
                s = (u1(k1-1,k2,k3) + u1(k1+1,k2,k3) + u1(k1,k2-1,k3) + u1(k1,k2+1,k3) + u1(k1,k2,k3-1) + u1(k1,k2,k3+1)) * sixth;
                u2(k1,k2,k3) = s;
            end
        end
    end
    foo = u1;
    u1 = u2;
    u2 = foo;
end
toc

