module types
implicit none
private
public dp
integer, parameter :: dp=kind(0.d0)          ! double precision
end module


module utils
! Various utilities
use types, only: dp
implicit none
private
public trace, mean, std, init_random_seed, randn, assert, stop_error

contains

subroutine stop_error(msg)
! Aborts the program with nonzero exit code
!
! The statement "stop msg" will return 0 exit code when compiled using
! gfortran. stop_error() uses the statement "stop 1" which returns an exit code
! 1 and a print statement to print the message.
!
! Example
! -------
!
! call stop_error("Invalid argument")

character(len=*) :: msg ! Message to print on stdout
print *, msg
stop 1
end subroutine

subroutine assert(condition)
! If condition == .false., it aborts the program.
!
! Arguments
! ---------
!
logical, intent(in) :: condition
!
! Example
! -------
!
! call assert(a == 5)

if (.not. condition) call stop_error("Assert failed.")
end subroutine

real(dp) function trace(A) result(t)
real(dp), intent(in) :: A(:, :)
integer :: i
t = 0
do i = 1, size(A, 1)
    t = t + A(i, i)
end do
end function

real(dp) function mean(x) result(t)
real(dp), intent(in) :: x(:)
t = sum(x) / size(x)
end function

real(dp) function std(x) result(t)
real(dp), intent(in) :: x(:)
t = sqrt(mean(abs(x - mean(x))**2))
end function

subroutine init_random_seed()
integer :: i, n, clock
integer, allocatable :: seed(:)
call random_seed(size=n)
allocate(seed(n))
call system_clock(count=clock)
seed = clock + 37 * [ (i - 1, i = 1, n) ]
call random_seed(put=seed)
end subroutine

FUNCTION rnorm() RESULT( fn_val )

!   This subroutine was taken from: http://jblevins.org/mirror/amiller/rnorm.f90

!   Generate a random normal deviate using the polar method.
!   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
!              normal variables', Siam Rev., vol.6, 260-264, 1964.

IMPLICIT NONE
REAL(dp)  :: fn_val

! Local variables

REAL(dp)            :: u, sum
REAL(dp), SAVE      :: v, sln
LOGICAL, SAVE   :: second = .FALSE.
REAL(dp), PARAMETER :: one = 1, vsmall = TINY( one )

IF (second) THEN
! If second, use the second random number generated on last call

  second = .false.
  fn_val = v*sln

ELSE
! First call; generate a pair of random normals

  second = .true.
  DO
    CALL RANDOM_NUMBER( u )
    CALL RANDOM_NUMBER( v )
    u = SCALE( u, 1 ) - one
    v = SCALE( v, 1 ) - one
    sum = u*u + v*v + vsmall         ! vsmall added to prevent LOG(zero) / zero
    IF(sum < one) EXIT
  END DO
  sln = SQRT(- SCALE( LOG(sum), 1 ) / sum)
  fn_val = u*sln
END IF

RETURN
END FUNCTION rnorm

subroutine randn(A)
real(dp), intent(out) :: A(:, :)
integer :: i, j
do j = 1, size(A, 2)
    do i = 1, size(A, 1)
        A(i, j) = rnorm()
    end do
end do
end subroutine

end module



module bench
use utils, only: trace, randn, std, mean
use types, only: dp
implicit none
private
public fib, mandelperf, pisum, randmatstat, randmatmul

contains

integer recursive function fib(n) result(r)
integer, intent(in) :: n
if (n < 2) then
    r = n
else
    r = fib(n-1) + fib(n-2)
end if
end function

integer function mandel(z0) result(r)
complex(dp), intent(in) :: z0
complex(dp) :: c, z
integer :: n
z = z0
c = z0
do n = 0, 78
    if (abs(z) > 2) then
        r = n
        return
    end if
    z = z**2 + c
end do
r = 79
end function

integer function mandelperf() result(mandel_sum)
real(dp) :: re, im
mandel_sum = 0
re = -2
do while (re < 0.49_dp)
    im = -1
    do while (im < 0.99)
        mandel_sum = mandel_sum + mandel(cmplx(re, im, dp))
        im = im + 0.1_dp
    end do
    re = re + 0.1_dp
end do
end function

real(dp) function pisum() result(s)
integer :: j, k
do j = 1, 499
    s = 0
    do k = 1, 9999
        s = s + 1._dp / k**2
    end do
end do
end function

subroutine randmatstat(t, s1, s2)
integer, intent(in) :: t
real(dp), intent(out) :: s1, s2
real(dp), allocatable, dimension(:, :) :: a, b, c, d, P, Q, X
real(dp), allocatable :: v(:), w(:)
integer :: n, i
n = 5
allocate(a(n, n), b(n, n), c(n, n), d(n, n))
allocate(P(4*n, n), Q(2*n, 2*n), X(2*n, 2*n))
allocate(v(t), w(t))
do i = 1, t
    call randn(a)
    call randn(b)
    call randn(c)
    call randn(d)
    P(:n, :)=a; P(n+1:2*n, :)=b; P(2*n+1:3*n, :)=c; P(3*n+1:, :)=d
    Q(:n,    :n) = a; Q(n+1:,    :n) = b
    Q(:n, n+1: ) = c; Q(n+1:, n+1: ) = d
    X = matmul(transpose(P), P)
    X = matmul(X, X)
    X = matmul(X, X)
    v(i) = trace(X)
    X = matmul(transpose(Q), Q)
    X = matmul(X, X)
    X = matmul(X, X)
    w(i) = trace(X)
end do
s1 = std(v) / mean(v)
s2 = std(w) / mean(w)
end subroutine

subroutine randmatmul(n, C)
integer, intent(in) :: n
real(dp), intent(out), allocatable :: C(:, :)
real(dp), allocatable :: A(:, :), B(:, :)
allocate(A(n, n), B(n, n), C(n, n))
call random_number(A)
call random_number(B)
C = matmul(A, B)
end subroutine

end module

program perf
use types, only: dp
use utils, only: assert, init_random_seed
use bench, only: fib, mandelperf, pisum, randmatstat, randmatmul
implicit none

integer :: i, f
real(dp) :: t1, t2, tmin, pi, s1, s2
real(dp), allocatable :: C(:, :)

call init_random_seed()

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    f = fib(20)
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
call assert(f == 6765)
print *, "fib", tmin*1000

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    f = mandelperf()
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
! This number is processor dependent, as it can differ a bit depending on the
! floating point rounding errors:
!call assert(f == 14307)
print *, "mandel", tmin*1000

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    pi = pisum()
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
call assert(abs(pi - 1.644834071848065_dp) < 1e-6_dp)
print *, "pi_sum", tmin*1000

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    call randmatstat(1000, s1, s2)
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
call assert(s1 > 0.5_dp .and. s1 < 1)
call assert(s2 > 0.5_dp .and. s2 < 1)
print *, "rand_mat_stat", tmin*1000

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    call randmatmul(1000, C)
    call assert(C(1, 1) >= 0)
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
print *, "randmatmul", tmin*1000

end program
