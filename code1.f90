PROGRAM code1
  USE quad
  USE testFunctions
  IMPLICIT NONE

  REAL :: y, z, z2
  REAL, external :: fun
  real :: PI = 4.D0*DATAN(1.D0)
  integer :: N
  real, allocatable :: PSI(:), DPSI(:)

  CALL setInt

  z = gaussQuad(f1, 0., 1., 2)
  z2 = gaussQuad(f2, 0., 2*pi, 2)
  PRINT *, z, z2

  ! test shape functions
  N = 3
  allocate( PSI(N), DPSI(N) )
  call Shape(0.111192, N, PSI, DPSI)
  print *, 'a = ', sum(PSI)
  print *, 'b = ', sum(DPSI)

END PROGRAM code1
