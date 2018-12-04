! ===============================================================
!
! 2D matrix convolution    (>= Fortran 90)
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

PROGRAM convolution2D
    IMPLICIT NONE

    ! ------------------------------------------------
    
    INTERFACE
         SUBROUTINE conv(C,M)
            REAL, ALLOCATABLE, INTENT(INOUT) :: C(:,:)
            INTEGER, INTENT(IN) :: M
         END SUBROUTINE conv
    END INTERFACE

    INTERFACE
         SUBROUTINE print_matrix(C)
            REAL, ALLOCATABLE, INTENT(IN) :: C(:,:)
         END SUBROUTINE print_matrix
    END INTERFACE 

    ! ------------------------------------------------

    REAL, ALLOCATABLE, DIMENSION(:,:) :: A    ! matrix to be convolved
    INTEGER :: matrix_size                    ! size of each matrix dimension
    INTEGER :: m,n                            ! # of rows and columns in matrix
    INTEGER :: num_convs                      ! # of convolutions
    CHARACTER(len=10) :: arg                  ! command line argument
    REAL :: start, finish
    
    ! get matrix size from command line and allocate/initialize it
    CALL get_command_argument(1, arg)
    READ(arg,*) matrix_size
    m = matrix_size; n = matrix_size
    ALLOCATE(A(m,n))
    A  = 1

    ! get # of convolutions from command line
    CALL get_command_argument(2, arg)
    READ(arg,*) num_convs
    PRINT *
    PRINT *, 'Number of convolutions: ', num_convs

    CALL cpu_time(start)
    CALL conv(A,num_convs)
    call cpu_time(finish)
    
    PRINT *    
    IF (matrix_size < 7) THEN 
        CALL print_matrix(A)
    ELSE
        PRINT *,"Matrix too big to print out"
    END IF 

    PRINT *
    PRINT '("Time = ",f6.3," seconds.")',finish-start    
    PRINT *
    
END PROGRAM convolution2D

! ===========================================================

!
! Convolves matrix A num_convs times  (assumes constant boundaries)
!
! INPUT
!       A         : 2D array to be convolved (source matrix)
!       num_convs : # of convolutions to do (INTEGER)
!
SUBROUTINE conv(A,num_convs)
    IMPLICIT NONE

    REAL, ALLOCATABLE, INTENT(INOUT) :: A(:,:)
    INTEGER, INTENT(IN) :: num_convs

    REAL, ALLOCATABLE :: B(:,:)  ! target matrix
    
    INTEGER           :: row_lbound, row_ubound
    INTEGER           :: col_lbound, col_ubound
    INTEGER           :: i,j,k

    ! get 2D array row/column index bounds
    row_lbound = LBOUND(A,1); row_ubound = UBOUND(A,1)
    col_lbound = LBOUND(A,2); col_ubound = UBOUND(A,2)

    ! set up target matrix
    ALLOCATE( B(row_lbound:row_ubound, col_lbound:col_ubound) )
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

END SUBROUTINE conv

! ===========================================================

!
! Print 2D matrix in row,column matrix format
!
! INPUT
!       A : 2D array to be printed in row,column matrix format
!
SUBROUTINE print_matrix(A)
    IMPLICIT NONE

    REAL, ALLOCATABLE, INTENT(IN)  :: A(:,:)
    
    INTEGER           :: row_lbound, row_ubound
    INTEGER           :: i
 
    ! get 2D array row index bound
    row_lbound = LBOUND(A,1); row_ubound = UBOUND(A,1)

    ! print the matrix a row at a time
    do i = row_lbound,row_ubound
        PRINT *, A(i, :)
    end do
    
END SUBROUTINE print_matrix
