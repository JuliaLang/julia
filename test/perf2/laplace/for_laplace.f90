SUBROUTINE for_update1(u, dx2, dy2, nx, ny)
real(8), intent(inout) :: u(nx,ny)
real(8), intent(in) :: dx2, dy2
integer, intent(in) :: nx, ny
integer :: i, j

DO j = 2, ny-1
    DO i = 2, nx-1
        u(i, j) = ((u(i+1, j) + u(i-1, j)) * dy2 + &
                   (u(i, j+1) + u(i, j-1)) * dx2) * (1./ (2*(dx2+dy2)))
    END DO
END DO
END SUBROUTINE

SUBROUTINE for_update2(u, dx2, dy2, nx, ny)
real(8), intent(inout) :: u(nx,ny)
real(8), intent(in) :: dx2, dy2
integer, intent(in) :: nx, ny


u(2:nx-1,2:ny-1) = ((u(3:,2:ny-1)+u(:ny-2,2:ny-1))*dy2 + &
        (u(2:nx-1,3:) + u(2:nx-1,:ny-2))*dx2) * (1./ (2*(dx2+dy2)))
END SUBROUTINE
