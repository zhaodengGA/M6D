!-------------------------------------------------------------------------
! M6D_use_advancematrix.f90
!
! PURPOSE:
!  set static matrices used in time advance
!
!-------------------------------------------------------------------------

module M6D_use_advancematrix

  implicit none


     
!------------------------------dynamic allocation--------------


   real, dimension(:,:,:), allocatable ::  G
   real, dimension(:,:,:), allocatable ::  Ginv

   real, dimension(:,:,:,:,:), allocatable :: G3
  real, dimension(:,:,:,:,:,:), allocatable :: G4   !needed for 40-moment
  real, dimension(:,:,:,:,:,:,:), allocatable :: G5  !needed for 121-moment

   real, dimension(:,:,:), allocatable :: Cinv
  
   complex, dimension(:,:,:), allocatable :: Dmat

end       module M6D_use_advancematrix
