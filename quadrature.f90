MODULE QUAD
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

END MODULE QUAD
