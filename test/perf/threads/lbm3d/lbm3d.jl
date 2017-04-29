# This file is a part of Julia. License is MIT: http://julialang.org/license

# 3D Lattice Boltzmann (BGK) model of a fluid.
# (http://exolete.com/lbm/)
# D3Q19 model. At each timestep, particle densities propagate
# outwards in the directions indicated in the figure. An
# equivalent 'equilibrium' density is found, and the densities
# relax towards that state, in a proportion governed by omega.
#               Iain Haslam, March 2006.
#
# Julia version: devectorized and threaded. The in-place
# circshift implementation is needed because of issue #10317.
# Despite that, GC time is ~22% of runtime. relax! and
# calc_equi! are broken out because of issue #10718. The
# threaded call form is used and the for loop form is is
# commented out in both because of issue #10527.

#using PyPlot
using Base.Threads

include("circshift.jl")

const t1 = 1/3
const t2 = 1/18
const t3 = 1/36

const prop_shifts = tuple([ 0  0  1], [ 0  0 -1], [ 0  1  0],
                    	  [ 0 -1  0], [ 1  0  0], [-1  0  0],
                     	  [ 1  1  0], [ 1 -1  0], [-1  1  0],
                     	  [-1 -1  0], [ 1  0  1], [ 1  0 -1],
                     	  [-1  0  1], [-1  0 -1], [ 0  1  1],
                     	  [ 0  1 -1], [ 0 -1  1], [ 0 -1 -1]);

const fourths = tuple([ 6, 8, 9,12,13], [ 7,10,11,14,15],
                      [ 4, 8,10,16,17], [ 5, 9,11,18,19],
                      [ 2,12,14,16,18], [ 3,13,15,17,19])


function relax!(F, UX, UY, UZ, nx, ny, nz, deltaU, t1D, t2D, t3D, sSQU, chunkid, nchunk)
    outerrange = Base.splitrange(nx, nchunk)
    for i = outerrange[chunkid]
    #@threads all for i = 1:nx
        for j = 1:ny
            for k = 1:nz
                density = 0.0
                for l = 1:size(F,4)
                    density = density + F[i,j,k,l]
                end
                fs = Array{Float64}(6)
                for l = 1:6
                    fs[l] = 0.0
                    for m = 1:5
                        fs[l] = fs[l] + F[i,j,k,fourths[l][m]]
                    end
                end
                UX[i,j,k] = (fs[1] - fs[2]) / density
                UY[i,j,k] = (fs[3] - fs[4]) / density
                UZ[i,j,k] = (fs[5] - fs[6]) / density

                if i == 1
                    UX[i,j,k] = UX[i,j,k] + deltaU # Increase inlet pressure
                end

                t1D[i,j,k] = t1 * density
                t2D[i,j,k] = t2 * density
                t3D[i,j,k] = t3 * density
                sSQU[i,j,k] = 3/2 * (UX[i,j,k]^2 + UY[i,j,k]^2 + UZ[i,j,k]^2)
            end
        end
    end
end


function calc_equi!(F, FEQ, t1D, t2D, t3D, U, UX, UY, UZ, sSQU, nx, ny, nz, omega, chunkid, nchunk)
    outerrange = Base.splitrange(nx, nchunk)
    for i = outerrange[chunkid]
    #@threads all for i = 1:nx
        #tid = threadid()
        for j = 1:ny
            for k = 1:nz

                FEQ[i,j,k,1] = t1D[i,j,k,1] * (1 - sSQU[i,j,k,1])

                # nearest neighbors
                FEQ[i,j,k,2] = t2D[i,j,k,1] * (1 + 3*UZ[i,j,k,1] + 9/2*UZ[i,j,k,1]^2 - sSQU[i,j,k,1])
                FEQ[i,j,k,3] = t2D[i,j,k,1] * (1 - 3*UZ[i,j,k,1] + 9/2*UZ[i,j,k,1]^2 - sSQU[i,j,k,1])
                FEQ[i,j,k,4] = t2D[i,j,k,1] * (1 + 3*UY[i,j,k,1] + 9/2*UY[i,j,k,1]^2 - sSQU[i,j,k,1])
                FEQ[i,j,k,5] = t2D[i,j,k,1] * (1 - 3*UY[i,j,k,1] + 9/2*UY[i,j,k,1]^2 - sSQU[i,j,k,1])
                FEQ[i,j,k,6] = t2D[i,j,k,1] * (1 + 3*UX[i,j,k,1] + 9/2*UX[i,j,k,1]^2 - sSQU[i,j,k,1])
                FEQ[i,j,k,7] = t2D[i,j,k,1] * (1 - 3*UX[i,j,k,1] + 9/2*UX[i,j,k,1]^2 - sSQU[i,j,k,1])

                U[1,chunkid]  = UX[i,j,k,1] + UY[i,j,k,1]
                U[2,chunkid]  = UX[i,j,k,1] - UY[i,j,k,1]
                U[3,chunkid]  = -UX[i,j,k,1] + UY[i,j,k,1]
                U[4,chunkid]  = -U[1,chunkid]
                U[5,chunkid]  = UX[i,j,k,1] + UZ[i,j,k,1]
                U[6,chunkid]  = UX[i,j,k,1] - UZ[i,j,k,1]
                U[7,chunkid]  = -U[6,chunkid]
                U[8,chunkid]  = -U[5,chunkid]
                U[9,chunkid]  = UY[i,j,k,1] + UZ[i,j,k,1]
                U[10,chunkid] = UY[i,j,k,1] - UZ[i,j,k,1]
                U[11,chunkid] = -U[10,chunkid]
                U[12,chunkid] = -U[9,chunkid]

                # next-nearest neighbors
                for l = 1:12
                    FEQ[i,j,k,l+7] = t3D[i,j,k,1] * (1 + 3*U[l,chunkid] + 9/2*(U[l,chunkid]^2) - sSQU[i,j,k,1])
                end

                for l = 1:19
                    F[i,j,k,l] = omega * FEQ[i,j,k,l] + (1 - omega) * F[i,j,k,l]
                end

            end
        end
    end
end

precompile(calc_equi!, (Array{Float64,4}, Array{Float64,4}, Array{Float64,3}, Array{Float64,3}, Array{Float64,3}, Array{Float64,2}, Array{Float64,3}, Array{Float64,3}, Array{Float64,3}, Array{Float64,3}, Int64, Int64, Int64, Float64))

function lbm3d(n)
    const nx = n
    const ny = nx
    const nz = nx
    const omega = 1.0
    const density = 1.0

    # Implementation note: setting nchunk to nthreads() is a hack
    # to simulate the previous implementation's use of parallel regions.
    nchunk = nthreads()

    tprop = 0
    trelax = 0
    tequi = 0

    F = repeat([density/19], outer=[nx, ny, nz, 19])
    FEQ = F
    matsize = nx*ny*nz

    CI = [0:matsize:matsize*19;]'

    BOUND = Array{Float64}(nx,ny,nz)

    for i=1:nx, j=1:ny, k=1:nz
        BOUND[i,j,k] = ((i-5)^2 + (j-6)^2 + (k-7)^2) < 6
    end

    BOUND[:,:,1] = 1
    BOUND[:,1,:] = 1

    ON = find(BOUND); # matrix offset of each Occupied Node

    TO_REFLECT = [ON+CI[2] ON+CI[3] ON+CI[4] ON+CI[5] ON+CI[6] ON+CI[7] ON+CI[8] ON+CI[9] ON+CI[10] ON+CI[11] ON+CI[12] ON+CI[13] ON+CI[14] ON+CI[15] ON+CI[16] ON+CI[17] ON+CI[18] ON+CI[19]]
    REFLECTED = [ON+CI[3] ON+CI[2] ON+CI[5] ON+CI[4] ON+CI[7] ON+CI[6] ON+CI[11] ON+CI[10] ON+CI[9] ON+CI[8] ON+CI[15] ON+CI[14] ON+CI[13] ON+CI[12] ON+CI[19] ON+CI[18] ON+CI[17] ON+CI[16]]

    UX = Array{Float64}(nx,ny,nz)
    UY = Array{Float64}(nx,ny,nz)
    UZ = Array{Float64}(nx,ny,nz)
    U  = Array{Float64}(12,nchunk)
    t1D = Array{Float64}(nx,ny,nz)
    t2D = Array{Float64}(nx,ny,nz)
    t3D = Array{Float64}(nx,ny,nz)
    sSQU = Array{Float64}(nx,ny,nz)

    avu = 1
    prevavu = 1
    ts = 0
    deltaU = 1e-7
    numactivenodes = sum(1-BOUND)

    @time while (ts < 4000  &&  (1e-10 < abs((prevavu-avu)/avu)))  ||  ts < 100
        tic()
        # Propagate -- nearest and next-nearest neighbors
        for i = 2:19
            circshift3d1!(F, i, prop_shifts[i-1])
        end
        tprop = tprop + toq()

        # Densities bouncing back at next timestep
        BOUNCEDBACK = F[TO_REFLECT]

        tic()

        # Relax; calculate equilibrium state (FEQ) with equivalent speed and density to F
        @threads for chunk=1:nchunk
            relax!(F, UX, UY, UZ, nx, ny, nz, deltaU, t1D, t2D, t3D, sSQU, chunkid, nchunk)
        end
        for o in ON
            UX[o] = UY[o] = UZ[o] = t1D[o] = t2D[o] = t3D[o] = sSQU[o] = 0.0
        end

        trelax = trelax + toq()
        tic()

        # Calculate equilibrium distribution: stationary
        @threads for chunk=1:nchunk
            calc_equi!(F, FEQ, t1D, t2D, t3D, U, UX, UY, UZ, sSQU, nx, ny, nz, omega)
        end

        tequi = tequi + toq()

        F[REFLECTED] = BOUNCEDBACK

        prevavu = avu
        avu = sum(UX) / numactivenodes
        ts = ts + 1
        #println(avu)
    end

    println("ts: $(ts)")
    println("propagation time: $tprop")
    println("relaxation time: $trelax")
    println("equilibrium time: $tequi")

    #zcut=5
    #quiver(UY[:,:,zcut], UX[:,:,zcut])
    #xlabel("y"); ylabel("x")
    #title("Flow field at z = $(zcut)), after $(ts)")
end

@time lbm3d(36)
#ccall(:jl_threading_profile, Void, ())
