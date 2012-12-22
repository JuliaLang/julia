program laplace_for
implicit none

INTERFACE
    SUBROUTINE for_update2(u, dx2, dy2, nx, ny)
        real(8), intent(inout) :: u(nx,ny)
        real(8), intent(in)    :: dx2, dy2
        integer, intent(in)    :: nx, ny
    END SUBROUTINE
END INTERFACE

integer, parameter :: dp=kind(0.d0)

integer, parameter :: N = 150
integer :: Niter
real(dp) :: u(N, N), t1, t2

read(*,*) Niter

call cpu_time(t1)
u = calc(N, Niter, 0.1_dp, 0.1_dp)
call cpu_time(t2)

open(101,file='data_Vectorized Fortran (pure)', status='unknown')
write(101,*) u
!print *, t2-t1
!print *, u(2, 2)
!print *, sum(u)
!print *, sum(u**2)
contains

function calc(N, Niter, dx, dy) result(u)
integer, intent(in) :: N, Niter
real(dp), intent(in) :: dx, dy
real(dp) :: u(N,N), dx2, dy2
integer :: i
u = 0
u(1,:) = 1
dx2 = dx**2
dy2 = dy**2
DO i = 1,Niter
    call for_update2(u, dx2, dy2, N, N)
END DO
end function

end program
