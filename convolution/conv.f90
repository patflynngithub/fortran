! ===============================================================
!
! 2D matrix convolution    (constant boundary, >= Fortran 90)
!
! Execution: ./conv square_matrix_size num_of_convolutions
!
!      e.g., ./conv 5 100
!
! Compilations:
!
!      gfortran -o conv conv.f90
!
!=================================================================

program convolution2D
    implicit none

    ! ------------------------------------------------
    
    interface
         subroutine conv(C,M)
            real, allocatable, intent(inout) :: C(:,:)
            integer, intent(in) :: M
         end subroutine conv
    end interface

    interface
         subroutine print_matrix(C)
            real, allocatable, intent(in) :: C(:,:)
         end subroutine print_matrix
    end interface 

    ! ------------------------------------------------

    real, allocatable, dimension(:,:) :: A    ! 2D matrix to be convolved
    integer :: matrix_size                    ! size of each matrix dimension
    integer :: m,n                            ! # of rows and columns in matrix
    integer :: num_convs                      ! # of convolutions
    character(len=10) :: arg1, arg2           ! command line arguments
    real :: start, finish
    
    ! get command line arguments
    call get_command_argument(1, arg1)   ! square matrix size
    call get_command_argument(2, arg2)   ! # of convolutions
    read(arg1,*) matrix_size   
    read(arg2,*) num_convs
    
    ! set up and initialize the initial matrix
    m = matrix_size; n = matrix_size
    allocate(A(m,n))
    A  = 1

    print *
    print *, 'Number of convolutions: ', num_convs

    call cpu_time(start)
    ! main body of work (convolution)
    call conv(A,num_convs)
    call cpu_time(finish)
    
    print *    
    if (matrix_size < 7) then 
        call print_matrix(A)
    else
        print *,"Matrix too big to print to console"
    end if 

    print *
    print '("Time = ",f6.3," seconds.")',finish-start    
    print *
    
end program convolution2D

! ===========================================================

!
! Convolves matrix A num_convs times  (assumes constant boundaries)
!
! INPUT
!       A         : 2D array to be convolved (source matrix)
!       num_convs : # of convolutions to do (integer)
!
subroutine conv(A,num_convs)
    implicit none

    real, allocatable, intent(inout) :: A(:,:)
    integer, intent(in) :: num_convs

    real, allocatable :: B(:,:)  ! target matrix
    
    integer           :: row_lbound, row_ubound
    integer           :: col_lbound, col_ubound
    integer           :: i,j,k

    ! get 2D array row/column index bounds
    row_lbound = lbound(A,1); row_ubound = ubound(A,1)
    col_lbound = lbound(A,2); col_ubound = ubound(A,2)

    ! set up target matrix
    allocate( B(row_lbound:row_ubound, col_lbound:col_ubound) )
    B = A  ! to get constant boundary set up

    ! do convolutions
    do k = 1,num_convs

        ! do a convolution
        do i = row_lbound+1, row_ubound-1
            do j = col_lbound+1, col_ubound-1
                B(i,j) = A(i,j) + .25*(A(i-1,j) + A(i,j-1) + A(i,j+1) + A(i+1,j))
            end do
        end do
        
        A = B  ! copy target matrix back to source matrix
    end do

end subroutine conv

! ===========================================================

!
! Print 2D matrix in row,column matrix format
!
! INPUT
!       A : 2D array to be printed in row,column matrix format
!
subroutine print_matrix(A)
    implicit none

    real, allocatable, intent(in)  :: A(:,:)
    
    integer           :: row_lbound, row_ubound
    integer           :: i
 
    ! get 2D array row index bound
    row_lbound = lbound(A,1); row_ubound = ubound(A,1)

    ! print the matrix a row at a time
    do i = row_lbound,row_ubound
        print *, A(i, :)
    end do
    
end subroutine print_matrix
