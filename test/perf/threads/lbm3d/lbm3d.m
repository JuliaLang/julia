% 3D Lattice Boltzmann (BGK) model of a fluid.
%
% From http://exolete.com/lbm/, reproduced here with the author's
% permission.
%
% D3Q19 model. At each timestep, particle densities propagate
% outwards in the directions indicated in the figure. An
% equivalent 'equilibrium' density is found, and the densities
% relax towards that state, in a proportion governed by omega.
%               Iain Haslam, March 2006.
nx=36;ny=nx;nz=nx; omega=1.0; density=1.0;t1=1/3; t2=1/18; t3=1/36;
F=repmat(density/19,[nx ny nz 19]); FEQ=F; matsize=nx*ny*nz;
CI=[0:matsize:matsize*19];
BOUND=zeros(nx,ny,nz);
for i=1:nx, for j=1:ny, for k=1:nz
	BOUND(i,j,k)=((i-5)^2+(j-6)^2+(k-7)^2)<6;
end, end, end
BOUND(:,:,1)=1;BOUND(:,1,:)=1;
ON=find(BOUND); %matrix offset of each Occupied Node
TO_REFLECT=[ON+CI(2) ON+CI(3) ON+CI(4) ON+CI(5)	ON+CI(6) ON+CI(7) ON+CI(8) ...
  ON+CI(9) ON+CI(10) ON+CI(11) ON+CI(12) ON+CI(13) ON+CI(14) ON+CI(15) ... 
  ON+CI(16) ON+CI(17) ON+CI(18) ON+CI(19)];
REFLECTED=[ON+CI(3) ON+CI(2) ON+CI(5) ON+CI(4) ON+CI(7) ON+CI(6) ON+CI(11) ...
  ON+CI(10) ON+CI(9) ON+CI(8) ON+CI(15) ON+CI(14) ON+CI(13) ON+CI(12) ...
  ON+CI(19) ON+CI(18) ON+CI(17) ON+CI(16)];
avu=1; prevavu=1; ts=0; deltaU=1e-7; numactivenodes=sum(sum(sum(1-BOUND)));
tic;
while (ts<4000 & 1e-10<abs((prevavu-avu)/avu)) | ts<100
	% Propagate
	%nearest-neighbours
	F(:,:,:,2)=F(:,:,[nz 1:nz-1],2);
	F(:,:,:,3)=F(:,:,[2:nz 1],3);
	F(:,:,:,4)=F(:,[ny 1:ny-1],:,4);
	F(:,:,:,5)=F(:,[2:ny 1],:,5);	
	F(:,:,:,6)=F([nx 1:nx-1],:,:,6);
	F(:,:,:,7)=F([2:nx 1],:,:,7);	
	%next-nearest neighbours
	F(:,:,:,8)= F([nx 1:nx-1],[ny 1:ny-1],:,8);
	F(:,:,:,9)= F([nx 1:nx-1],[2:ny 1],:,9);
	F(:,:,:,10)=F([2:nx 1],[ny 1:ny-1],:,10);
	F(:,:,:,11)=F([2:nx 1],[2:ny 1],:,11);	
	F(:,:,:,12)=F([nx 1:nx-1],:,[nz 1:nz-1],12);
	F(:,:,:,13)=F([nx 1:nx-1],:,[2:nz 1],13);
	F(:,:,:,14)=F([2:nx 1],:,[nz 1:nz-1],14);
	F(:,:,:,15)=F([2:nx 1],:,[2:nz 1],15);
	F(:,:,:,16)=F(:,[ny 1:ny-1],[nz 1:nz-1],16);
	F(:,:,:,17)=F(:,[ny 1:ny-1],[2:nz 1],17);
	F(:,:,:,18)=F(:,[2:ny 1],[nz 1:nz-1],18);
	F(:,:,:,19)=F(:,[2:ny 1],[2:nz 1],19);
	BOUNCEDBACK=F(TO_REFLECT); %Densities bouncing back at next timestep
	% Relax; calculate equilibrium state (FEQ) with equivalent speed and density to F 
	DENSITY = sum(F,4);
	UX=(sum(F(:,:,:,[6 8 9 12 13]),4)-sum(F(:,:,:,[7 10 11 14 15]),4))./DENSITY;
	UY=(sum(F(:,:,:,[4 8 10 16 17]),4)-sum(F(:,:,:,[5 9 11 18 19]),4))./DENSITY;
	UZ=(sum(F(:,:,:,[2 12 14 16 18]),4)-sum(F(:,:,:,[3 13 15 17 19]),4))./DENSITY;
	UX(1,:,:)=UX(1,:,:)+deltaU; %Increase inlet pressure
	UX(ON)=0; UY(ON)=0; UZ(ON)=0; DENSITY(ON)=0; U_SQU=UX.^2+UY.^2+UZ.^2;
	U8=UX+UY;U9=UX-UY;U10=-UX+UY;U11=-U8;U12=UX+UZ;U13=UX-UZ;
	U14=-U13;U15=-U12;U16=UY+UZ;U17=UY-UZ;U18=-U17;U19=-U16;
	% Calculate equilibrium distribution: stationary
	FEQ(:,:,:,1)=t1*DENSITY.*(1-3*U_SQU/2);
	% nearest-neighbours
	FEQ(:,:,:,2)=t2*DENSITY.*(1 + 3*UZ + 9/2*UZ.^2 - 3/2*U_SQU);
	FEQ(:,:,:,3)=t2*DENSITY.*(1 - 3*UZ + 9/2*UZ.^2 - 3/2*U_SQU);
	FEQ(:,:,:,4)=t2*DENSITY.*(1 + 3*UY + 9/2*UY.^2 - 3/2*U_SQU);
	FEQ(:,:,:,5)=t2*DENSITY.*(1 - 3*UY + 9/2*UY.^2 - 3/2*U_SQU);
	FEQ(:,:,:,6)=t2*DENSITY.*(1 + 3*UX + 9/2*UX.^2 - 3/2*U_SQU);
	FEQ(:,:,:,7)=t2*DENSITY.*(1 - 3*UX + 9/2*UX.^2 - 3/2*U_SQU);
	% next-nearest neighbours
	FEQ(:,:,:,8) =t3*DENSITY.*(1 + 3*U8  + 9/2*(U8).^2  - 3*U_SQU/2);
	FEQ(:,:,:,9) =t3*DENSITY.*(1 + 3*U9  + 9/2*(U9).^2  - 3*U_SQU/2);
	FEQ(:,:,:,10)=t3*DENSITY.*(1 + 3*U10 + 9/2*(U10).^2 - 3*U_SQU/2);
	FEQ(:,:,:,11)=t3*DENSITY.*(1 + 3*U11 + 9/2*(U11).^2 - 3*U_SQU/2);
	FEQ(:,:,:,12)=t3*DENSITY.*(1 + 3*U12 + 9/2*(U12).^2 - 3*U_SQU/2);
	FEQ(:,:,:,13)=t3*DENSITY.*(1 + 3*U13 + 9/2*(U13).^2 - 3*U_SQU/2);
	FEQ(:,:,:,14)=t3*DENSITY.*(1 + 3*U14 + 9/2*(U14).^2 - 3*U_SQU/2);
	FEQ(:,:,:,15)=t3*DENSITY.*(1 + 3*U15 + 9/2*(U15).^2 - 3*U_SQU/2);
	FEQ(:,:,:,16)=t3*DENSITY.*(1 + 3*U16 + 9/2*(U16).^2 - 3*U_SQU/2);
	FEQ(:,:,:,17)=t3*DENSITY.*(1 + 3*U17 + 9/2*(U17).^2 - 3*U_SQU/2);
	FEQ(:,:,:,18)=t3*DENSITY.*(1 + 3*U18 + 9/2*(U18).^2 - 3*U_SQU/2);
	FEQ(:,:,:,19)=t3*DENSITY.*(1 + 3*U19 + 9/2*(U19).^2 - 3*U_SQU/2);
	F=omega*FEQ+(1-omega)*F;
	F(REFLECTED)=BOUNCEDBACK;
	prevavu=avu;avu=sum(sum(sum(UX)))/numactivenodes; ts=ts+1;
end
toc;
%figure;zcut=5;colormap(gray(2));image(2-BOUND(:,:,5));hold on;
%Thanks to Thomas Wagner for correcting the transposed results plots
%quiver(UY(:,:,zcut),UX(:,:,zcut));xlabel('y');ylabel('x');
%title(['Flow field at z=',num2str(zcut),', after ',num2str(ts),'\deltat']);
%figure;ycut=5;colormap(gray(2));image(2-squeeze(BOUND(:,ycut,:)));hold on;
%quiver(squeeze(UZ(:,ycut,:)),squeeze(UX(:,ycut,:)));xlabel('z');ylabel('x');
%title(['Flow field at y=',num2str(ycut),', after ',num2str(ts),'\deltat']);
