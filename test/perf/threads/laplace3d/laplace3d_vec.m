% 3D Laplace equation solver

nx = 290;
ny = 290;
nz = 290;
iters = 100;
sixth = 1.0/6.0;

u1 = zeros(nx, ny, nz);
u2 = zeros(nx, ny, nx);

%Boundary conditions
u1(1,:,:) = 1.0;
u1(nx,:,:) = 1.0;
u1(:,1,:) = 1.0;
u1(:,ny,:) = 1.0;
u1(:,:,1) = 1.0;
u1(:,:,nz) = 1.0;
u2 = u1;

i = 2:nx-1;
j = 2:ny-1;
k = 2:nz-1;

%Main
tic
for n = 1:iters
    u2(i,j,k) = (u1(i-1,j,k) + u1(i+1,j,k) + u1(i,j-1,k) + u1(i,j+1,k) + u1(i,j,k-1) + u1(i,j,k+1))*sixth;
    foo = u1;
    u1 = u2;
    u2 = foo; 
end
toc

