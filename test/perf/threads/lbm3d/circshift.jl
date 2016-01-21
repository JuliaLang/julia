# This file is a part of Julia. License is MIT: http://julialang.org/license

# An in-place version of circshift. Needs generalization -- currently
# works only on a 3D hyperplane of a 4D array and supports only unit
# shifts. Code needs refactoring to reduce duplication.

using Match

# a is the 4D array; index specifies which 3D hyperplane; vec
# specifies the shifts (only 1 and -1)
function circshift3d1!(a,index,vec)
    nx = size(a,1)
    ny = size(a,2)
    nz = size(a,3)
    for i = 1 : length(vec)
        @match vec[i] begin
            1  => shift1!(a,nx,ny,nz,index,i)
           -1  => shiftm1!(a,nx,ny,nz,index,i)
        end
    end
end

function shift1!(a,nx,ny,nz,index,i)
    @match i begin
        1 => shifti!(a,nx,ny,nz,index)
        2 => shiftj!(a,nx,ny,nz,index)
        3 => shiftk!(a,nx,ny,nz,index)
    end
end

function shiftm1!(a,nx,ny,nz,index,i)
    @match i begin
        1 => shiftmi!(a,nx,ny,nz,index)
        2 => shiftmj!(a,nx,ny,nz,index)
        3 => shiftmk!(a,nx,ny,nz,index)
    end
end

function shifti!(a,nx,ny,nz,index)
    for k = 1:nz
        for j = 1:ny
            t = a[end,j,k,index]
            for i = nx-1:-1:1
               a[i+1,j,k,index] = a[i,j,k,index]
            end
            a[1,j,k,index] = t
        end
    end
end

function shiftj!(a,nx,ny,nz,index)
    for i = 1:nx
        for k = 1:nz
            t = a[i,end,k,index]
            for j = ny-1:-1:1
                a[i,j+1,k,index] = a[i,j,k,index]
            end
            a[i,1,k,index] = t
       end
    end
end

function shiftk!(a,nx,ny,nz,index)
    for i = 1:nx
        for j = 1:ny
        t = a[i,j,end,index]
            for k = nz-1:-1:1
                a[i,j,k+1,index] = a[i,j,k,index]
            end
        a[i,j,1,index] = t
        end
    end
end

function shiftmi!(a,nx,ny,nz,index)
    for k = 1:nz
        for j = 1:ny
            t = a[1,j,k,index]
            for i = 1:nx-1
                a[i,j,k,index] = a[i+1,j,k,index]
            end
            a[end,j,k,index] = t
        end
    end
end

function shiftmj!(a,nx,ny,nz,index)
    for k = 1:nz
        for i = 1:nx
            t = a[i,1,k,index]
            for j = 1:ny-1
                a[i,j,k,index] = a[i,j+1,k,index]
            end
            a[i,end,k,index] = t
       end
    end
end

function shiftmk!(a,nx,ny,nz,index)
    for i = 1:nx
        for j = 1:ny
            t = a[i,j,1,index]
            for k = 1:nz-1
                a[i,j,k,index] = a[i,j,k+1,index]
            end
            a[i,j,end,index] = t
       end
    end
end

