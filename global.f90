module global
  implicit none

  character (len=100) :: title, filename='input.txt'
  integer nnode  ! number of nodes
  integer nelem  ! number of elements
  integer nmat  ! material number
  integer npoint  ! number of point loads
  real, allocatable :: X(:) ! coordinates of nodes

end module global
