MODULE QUAD
  use material
  IMPLICIT NONE

  REAL :: XI(4,4), W(4,4)

CONTAINS
  SUBROUTINE setInt
    ! Order 1
    XI(1,1) = 0.
    W(1,1) = 2.

    ! Order 2
    XI(1,2) = -1. / SQRT(3.)
    XI(2,2) = -XI(1,2)
    W(1,2) = 1.0
    W(2,2) = W(1,2)

    ! Order 3
    XI(1,3) = -SQRT(3./5.)
    XI(2,3) = 0.
    XI(3,3) = -XI(1,3)
    W(1,3) = 5./9.
    W(2,3) = 8./9.
    W(3,3) = W(1,3)

    ! Order 4
    XI(1,4) = -0.8611363116
    XI(2,4) = -0.3399810436
    XI(3,4) = -XI(2,4)
    XI(4,4) = -XI(1,4)
    W(1,4) = 0.3478548451
    W(2,4) = 0.6521451549
    W(3,4) = W(2,4)
    W(4,4) = W(1,4)

  END SUBROUTINE setInt

  REAL FUNCTION gaussQuad(g, x1, x2, nl)
    IMPLICIT NONE
    REAL :: g
    REAL, INTENT(IN) :: x1
    REAL, INTENT(IN) :: x2
    integer, intent(in) :: nl
    integer i

    gaussQuad = 0.
    do i = 1,Nl
       gaussQuad = gaussQuad + g(x1 + 0.5*(x2-x1)*(1+XI(i,Nl))) * W(i,Nl)
    end do
    gaussQuad = gaussQuad * 0.5 * (x2 - x1)

  END FUNCTION gaussQuad

  subroutine Shape(xi, N, PSI, DPSI)
    implicit none
    real, intent(in) :: xi
    integer, intent(in) :: N
    real, intent(out) :: psi(N), dpsi(N)

    if (N .gt. 3) then
       print *, 'N must be < 3; only accept linear and quadratic shape functions'
       stop
    end if

    if (abs(xi) > 1) then
       print *, 'Xi must be in [-1,1] in the master element'
       stop
    end if

    ! linear shape functions
    if (N .eq. 2) then
       PSI(1) = 0.5 * (1. - xi)
       PSI(2) = 0.5 * (1. + xi)
       DPSI(1) = -0.5
       DPSI(2) = 0.5
    end if

    if (N .eq. 3) then
       PSI(1) = xi * (xi - 1.) * 0.5
       PSI(2) = 1. - XI**2
       PSI(3) = xi * (xi+1.) * 0.5
       DPSI(1) = xi - 0.5
       DPSI(2) = -2.*xi
       DPSI(3) = xi + .5
    end if

  end subroutine Shape

  subroutine Elem(x1, x2, N, EK, EF, Nl, mat)
    implicit none
    real, intent(in) :: x1, x2
    integer, intent(in) :: N, Nl, mat
    real, intent(out) :: EK(N,N), EF(N)
    real :: x, dx, xk, xc, xb, xf, PSI(N), DPSI(N)
    integer i,j,l

    dx = (x2 - x1) / 2.
    EK = 0.
    EF = 0.

    ! begin integration
    do l = 1,Nl
       x = x1 + (1. + XI(l,Nl)) * dx
       call Getmat(mat, x, xk, xc, xb, xf)
       call Shape(XI(l,Nl), N, PSI, DPSI)

       do i = 1,N
          EF(i) = EF(i) + PSI(i) * xf* W(l,Nl) * dx

          do j = 1,N
             EK(i,j) = EK(i,j) + W(l,Nl)*dx*(xk*DPSI(i)*DPSI(j)/(dx**2) + &
              xc*PSI(i)*DPSI(j)/dx + xb*PSI(i)*PSI(j))
          end do
       end do
    end do
  end subroutine Elem


END MODULE QUAD
