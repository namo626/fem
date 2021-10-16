module prepmod
  use global

  implicit none

  private
  character (len=100) :: junk

  public :: Prep

contains

  subroutine Prep
    implicit none

    ! always unit 1
    open(1, file=filename, action='read')

    read (*, '(A100)') title
    print *, 'TITLE: ', title

    call Rcon
    call Rnode
    ! call Relem
    ! call Rmat
    ! call Rbc

  end subroutine Prep

  subroutine Rcon
    implicit none

    read (1,*) nnode, junk
    write(*,100) 'NNODE = ', nnode
    if ((nnode .lt. 2) .or. (nnode .gt. 100)) then
       print *, 'nnode must lie in [2,100]'
       stop
    end if

    read (1,*) nelem, junk
    write(*,100) 'NELEM = ', nelem
    if ((nelem .lt. 1) .or. (nelem .gt. 99)) then
       print *, 'nelem must lie in [1,99]'
       stop
    end if

    read (1,*) nmat, junk
    write(*,100) 'NMAT = ', nmat
    if ((nmat .lt. 1) .or. (nmat .gt. 5)) then
       print *, 'nmat must lie in [1,5]'
       stop
    end if

    read (1,*) npoint, junk
    write(*,100) 'NPOINT = ', npoint
    if ((npoint .lt. 0) .or. (npoint .gt. 5)) then
       print *, 'npoint must lie in [0,5]'
       stop
    end if

    100 format (A10, I4)

  end subroutine Rcon

  subroutine Rnode ! allocate global coordinate array X
    implicit none
    real :: x1, x2, dx
    integer i

    ! read endpoints from input file (unit 1)
    read (1,*) x1, x2, junk

    ! allocation
    allocate(X(nnode))
    X(1) = x1
    X(nnode) = x2

    dx = (x2 - x1) / (nnode - 1.)
    do i = 2,nnode-1
       X(i) = x1 + (i-1.)*dx
    end do

    ! display results
    print *, 'Coordinates: '
    do i = 1,nnode
       write(*,101) X(i)
    end do

    101 format(F8.3)

  end subroutine Rnode

end module prepmod
