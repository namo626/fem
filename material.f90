module material
  implicit none

contains

  subroutine Getmat(mat, x, xk, xc, xb, xf)
    integer, intent(in) :: mat
    real, intent(in) :: x
    real, intent(out) :: xk, xc, xb, xf

    select case (mat)
    case(1)
       xk = -1.
       xc = 0.
       xb = 1.
       xf = x
    case(2)
       xk = 2.
       xc = .5
       xb = 0.
       xf = 1.
    end select

  end subroutine Getmat

end module material
