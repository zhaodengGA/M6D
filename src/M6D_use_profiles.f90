!-------------------------------------------------------------------------
! M6D_use_profiles.f90
!
! PURPOSE:
!  transfer radial profiles 
!
!-------------------------------------------------------------------------

module M6D_use_profiles

  implicit none


!------------------------------dynamic allocation------------------------


    real, dimension(:), allocatable :: den0
    real, dimension(:), allocatable :: temp0
    real, dimension(:), allocatable :: v_th0

    real, dimension(:), allocatable :: nu_hat

    real, dimension(:), allocatable :: u0_r
    real, dimension(:), allocatable :: u0_theta
    real, dimension(:), allocatable :: u0_phi
    real, dimension(:,:), allocatable :: u0
 
    real, dimension(:), allocatable :: Er0
    real, dimension(:,:), allocatable :: E0

    real, dimension(:), allocatable :: qpro


    real, dimension(:,:), allocatable :: Rmaj
    real, dimension(:,:), allocatable :: abs_grad_r
    real, dimension(:,:), allocatable :: abs_grad_theta

!   real, dimension(:,:), allocatable :: B_r
    real, dimension(:,:), allocatable :: B_theta
    real, dimension(:,:), allocatable :: B_phi
    real, dimension(:,:,:), allocatable :: B0

    real, dimension(:), allocatable :: q1
    real, dimension(:), allocatable :: q2
    real, dimension(:), allocatable :: q3

    real, dimension(:,:,:), allocatable :: h

    real, dimension(:,:,:,:,:), allocatable :: Chris

    real, dimension(:), allocatable :: M00
    real, dimension(:,:), allocatable :: M01
    real, dimension(:,:,:), allocatable :: M02
    real, dimension(:,:,:,:), allocatable :: M03
    real, dimension(:,:,:,:,:), allocatable :: M04


end       module M6D_use_profiles
