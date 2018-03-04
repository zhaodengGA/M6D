!-------------------------------------------------------------------------
! M6D_use_matrix.f90
!
! PURPOSE:
!     working matricies
!
!-------------------------------------------------------------------------

module M6D_use_matrix

  implicit none


     

!------------------------------dynamic allocation------------------------


    real, dimension(:,:), allocatable :: mat
    real, dimension(:,:), allocatable :: mat_inv

    real, dimension(:), allocatable :: mat0

    complex, dimension(:,:), allocatable :: cmat
    complex, dimension(:,:), allocatable :: cmat_inv

    real, dimension(:,:), allocatable :: mat5
    real, dimension(:,:), allocatable :: mat5_inv

    complex, dimension(:,:), allocatable :: cmat5
    complex, dimension(:,:), allocatable :: cmat5_inv

end       module M6D_use_matrix
