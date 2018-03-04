!-------------------------------------------------------------------------
! M6D_use_dynamic.f90
!
! PURPOSE:
!  store the dynamic (time dependent) arrays for the n_pk  distribution funtion  w(j,n,pk) 
!
!  and the moments M0,M1j1,M2j2j1 and the closure M3j3j2j1  for 13-moment
!
!   plus the electric field vector E_r(n,pk), E_theta(n,pk), E_phi(n,pk)...
!  labeled  E(j1,pk)
!  phi(n,pk) is always available from  E_phi = E(3,n,pk) and does not need storage
!
!  
!-------------------------------------------------------------------------

module M6D_use_dynamic

  implicit none


     

!------------------------------ dynamic allocation-----------------------


! w(j,n,p,k) = w(j,n,pk)   !this is the primary communicated time dependent data.
!    complex, dimension(:,:,:), allocatable :: w
!mpi   
!    complex, dimension(:,:,:), allocatable :: w_myid
!    complex, dimension(:,:,:,:), allocatable :: w_myid_tot

! Msup is the primary communicated time dependent data replacing w
    complex, dimension(:,:,:), allocatable :: Msup
    complex, dimension(:,:,:), allocatable :: Msup_myid
    complex, dimension(:,:,:,:), allocatable :: Msup_myid_tot

    complex, dimension(:,:,:), allocatable :: M0
    complex, dimension(:,:,:,:), allocatable :: M1
    complex, dimension(:,:,:,:,:), allocatable :: M2
    complex, dimension(:,:,:,:,:,:), allocatable :: M3   !closure for 10-moment
    complex, dimension(:,:,:,:,:,:,:), allocatable :: M4  !closure for 20
    complex, dimension(:,:,:,:,:,:,:,:), allocatable :: M5 !closure for 35

    complex, dimension(:,:,:), allocatable :: M0_sav
    complex, dimension(:,:,:,:), allocatable :: M1_sav
    complex, dimension(:,:,:,:,:), allocatable :: M2_sav      !10
    complex, dimension(:,:,:,:,:,:), allocatable :: M3_sav    !20
    complex, dimension(:,:,:,:,:,:,:), allocatable :: M4_sav  !35

    complex, dimension(:,:), allocatable :: RHS   !ZD: RHS is dispersion matrix


! E_r(n,p,k) = E_r(n,pk), E_theta(n,p,k) = E_theta(n,pk), E_phi(n,p,k) = E_phi(n,pk) 
!   Electric field vector
    complex, dimension(:,:,:,:), allocatable :: E

!time advance  on (n,pk) tranfer parts or RHS
   complex :: divM1
   complex :: divM2(3)
   complex :: divM3(3,3)
   complex :: divM4(3,3,3)
   complex :: divM5(3,3,3,3)

   complex :: EM1(3)
   complex :: EM2(3,3)
   complex :: EM3(3,3,3)
   complex :: EM4(3,3,3,3)

   complex :: BM1(3)
   complex :: BM2(3,3)
   complex :: BM3(3,3,3)
   complex :: BM4(3,3,3,3)

   complex :: CM0
   complex :: CM1(3)
   complex :: CM2(3,3)
   complex :: CM3(3,3,3)
   complex :: CM4(3,3,3,3)


!  phi_mode(n) 
   complex, dimension(:), allocatable :: phi_mode

!  den_mode(n)
    complex, dimension(:), allocatable :: den_mode


! time  !first half step time = time + del_t/2.  !second half time = time + del_t/2.  
!  Full step step is del_t 
    real :: time
    real :: time_prev  !for use at data step
    integer :: n_time !number of full time steps

! other time time dependent and auxillary and diagnostic data here

end       module M6D_use_dynamic
