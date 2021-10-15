module testFunctions
  implicit none
contains

  pure real function f1(x)
    real, intent(in) :: x
    f1 = x + x**2 + x**3
  end function f1

  pure real function f2(x)
    real, intent(in) :: x
    f2 = sin(x)
  end function f2

end module testFunctions
