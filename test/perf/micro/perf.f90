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
use utils, only: trace, randn, std, mean, stop_error
use types, only: dp
implicit none
private
public fib, parse_int, quicksort, mandelperf, pisum, randmatstat, randmatmul

contains

integer recursive function fib(n) result(r)
integer, intent(in) :: n
if (n < 2) then
    r = n
else
    r = fib(n-1) + fib(n-2)
end if
end function

integer function parse_int(s, base) result(n)
character(len=*), intent(in) :: s
integer, intent(in) :: base
integer :: i, d
character :: c
n = 0
do i = 1, len(s)
    c = s(i:i)
    d = 0
    if (ichar(c) >= ichar('0') .and. ichar(c) <= ichar('9')) then
        d = ichar(c) - ichar('0')
    else if (ichar(c) >= ichar('A') .and. ichar(c) <= ichar('Z')) then
        d = ichar(c) - ichar('A') + 10
    else if (ichar(c) >= ichar('a') .and. ichar(c) <= ichar('z')) then
        d = ichar(c) - ichar('a') + 10
    else
        call stop_error("parse_int 1")
    end if

    if (base <= d) call stop_error("parse_int 2")
    n = n*base + d
end do

end function


integer function mandel(z0) result(r)
complex(dp), intent(in) :: z0
complex(dp) :: c, z
integer :: n, maxiter
maxiter = 80
z = z0
c = z0
do n = 1, maxiter
    if (abs(z) > 2) then
        r = n-1
        return
    end if
    z = z**2 + c
end do
r = maxiter
end function

integer function mandelperf() result(mandel_sum)
integer :: re, im
volatile :: mandel_sum
mandel_sum = 0
re = -20
do while (re <= 5)
    im = -10
    do while (im <= 10)
        mandel_sum = mandel_sum + mandel(cmplx(re/10._dp, im/10._dp, dp))
        im = im + 1
    end do
    re = re + 1
end do
end function

recursive subroutine quicksort(a, lo0, hi)
real(dp), intent(inout) :: a(:)
integer, intent(in) :: lo0, hi
integer :: i, j, lo
real(dp) :: pivot, t
lo = lo0
i = lo
j = hi
do while (i < hi)
    pivot = a((lo+hi)/2)
    do while (i <= j)
        do while (a(i) < pivot)
            i = i + 1
        end do
        do while (a(j) > pivot)
            j = j - 1
        end do
        if (i <= j) then
            t = a(i)
            a(i) = a(j)
            a(j) = t
            i = i + 1
            j = j - 1
        end if
    end do
    if (lo < j) call quicksort(a, lo, j)
    lo = i
    j = hi
end do
end subroutine

real(dp) function pisum() result(s)
integer :: j, k
do j = 1, 500
    s = 0
    do k = 1, 10000
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
use bench, only: fib, parse_int, quicksort, mandelperf, pisum, randmatstat, &
    randmatmul
implicit none

integer, parameter :: NRUNS = 1000
integer :: i, f, n, m, k, k2
real(dp) :: t1, t2, tmin, pi, s1, s2
real(dp), allocatable :: C(:, :), d(:)
character(len=11) :: s

call init_random_seed()

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    do k = 1, NRUNS
        f = fib(20)
    end do
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
call assert(f == 6765)
print "('fortran,fib,',f0.6)", tmin*1000._dp / NRUNS

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    do k2 = 1, NRUNS
        do k = 1, 1000
            call random_number(s1)
            n = int(s1*huge(n))
            write(s, '(z0)') n
            m = parse_int(s(:len_trim(s)), 16)
            call assert(m == n)
        end do
    end do
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
print "('fortran,parse_int,',f0.6)", tmin*1000._dp / NRUNS

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    do k = 1, NRUNS
        f = mandelperf()
    end do
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
call assert(f == 14791)
print "('fortran,mandel,',f0.6)", tmin*1000._dp / NRUNS

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    do k = 1, NRUNS
        allocate(d(5000))
        call random_number(d)
        call quicksort(d, 1, size(d))
        deallocate(d)
    end do
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
print "('fortran,quicksort,',f0.6)", tmin*1000._dp / NRUNS

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    pi = pisum()
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
call assert(abs(pi - 1.644834071848065_dp) < 1e-6_dp)
print "('fortran,pi_sum,',f0.6)", tmin*1000

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    call randmatstat(1000, s1, s2)
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
! call assert(s1 > 0.5_dp .and. s1 < 1)
! call assert(s2 > 0.5_dp .and. s2 < 1)
print "('fortran,rand_mat_stat,',f0.6)", tmin*1000

tmin = 1e9_dp
do i = 1, 5
    call cpu_time(t1)
    call randmatmul(1000, C)
    call assert(C(1, 1) >= 0)
    call cpu_time(t2)
    if (t2-t1 < tmin) tmin = t2-t1
end do
print "('fortran,rand_mat_mul,',f0.6)", tmin*1000

end program
