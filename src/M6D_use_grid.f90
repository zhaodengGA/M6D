!-------------------------------------------------------------------------
! M6D_use_grid.f90
!
! PURPOSE:
!   inputs
!
!-------------------------------------------------------------------------

module M6D_use_grid

  implicit none

    real, parameter :: pi=3.14159265
    complex, parameter :: xi = (0.0,1.0)
 !   integer,parameter :: singlep=selected_real_kind(6), doublep=selected_real_kind(12)
     
!------------------------------dynamic allocation--------------

! restart      * M6D code has the ability to restart. The code will back up
!              * Msup at each output time moment. After the case finished
!              * running or stopped, M6D can restart either from the 
!              * last backup point or rollback to the point before the last one.
!              * restart=0 for run a case from time=0;
!              * restart=1 for restart from the last backup point;
!              * restart=2 for restart and rollback to the point before the last one.

    integer :: restart  
    integer :: n_g
    integer :: n_r
    integer :: n_theta
    integer :: n_n
    integer :: n_min
    integer :: n_del
    integer :: n_v1
    integer :: n_v3

    integer :: i_zero  !velocity  0,0,0 grid

    integer :: i_zero_r ! lowest radial velocity grid  (1,0,0)
    integer :: i_zero_theta ! lowest radial velocity grid (0,1,0)
    integer :: i_zero_phi   ! lowest radial velocity grid (0,0,1)
    integer :: n_pk

! grid indices

    integer ::  p  !main radial index p=-n_g,0,1,2....n_r-1,n_r-1+n_g
    integer ::  k  !main theta index  k=-n_g,0,1,2....n_theta-1,n_r-1+n_g
    integer ::  n  !toroidal number index n = 0,1,2,3,n_n-1
    integer ::  i  ! summation velocity grid index  i=1:n_v3
    integer ::  j  ! advance velocity grid index    j=1:n_v3

    integer ::  q  ! p-k pointer index in field solve q=1,2,..2,..n_r x n_theta
    integer ::  pk  ! p-k pointer index  for mpi 
!      pk= 1,2,...n_r*n_theta
!      any (p,k) array can be converted to pk   
!      see  p_pk_map(pk), k_pk_map(pk), pk_p_k_map(p,k)

! 3D 1,2,3  indices    1=radius, 2=theta, 3=phi
    integer :: j1
    integer :: j2
    integer :: j3
    integer :: j4
    integer :: j5
    integer :: J6

!kdelta
    real :: kdelta(3,3)
!cross product
    real :: eps(3,3,3)

    integer :: iv_map0
    integer :: iv_map1(3)
    integer :: iv_map2(3,3)    !OK for 13-moment
    integer :: iv_map3(3,3,3)  !needed for 40-moments
    integer :: iv_map4(3,3,3,3)  !needed for 121




! input physics parameters

    real :: omega_star
    real :: r_hat_a
    real :: r_hat_b
    real :: Rmaj0_hat
    real :: q_mid
    real :: s_hat
    real :: aoLn
    real :: aoLT
    real :: nu_mid

! input numerical grid parameters
    real :: v_max

! run controls
    real :: delta
    real :: del_t

    integer :: IC_flag
    real :: delta_i      !i_delta model used with IC_flag = 1
    integer :: i_solver

    integer :: i_geom   !geometry  0=normal toroidal;  1 = r,theta,phi --->x,y,z
    real    :: BC  !BC=0 0-value radial BC    BC=1 is 0-grad radial BC

!  radial boundary condition
!    real :: BC  !BC = 1.0  zero BC; BC=0.0  zero gradient for w and phi
!    real :: CY  !CY = 1. non-cyclic BC = 0.0 or 1.0;  CY= 0.0 cyclic
!  M6D is full-F with only zero BC perturbations to start

! grids
    real, dimension(:), allocatable :: r_hat
    real, dimension(:), allocatable :: theta
    integer, dimension(:), allocatable :: n_tor
    real, dimension(:), allocatable :: delta0n !0.0 except  n_tor=0  then 1.0

! local test wave numbers
    integer :: i_kxkykz
    real :: kx ! radial
    real :: ky ! poloidal  m/r
    real :: kz ! toroidal  n/R


!    real, dimension(:), allocatable :: v_r
!    real, dimension(:), allocatable :: v_theta
!    real, dimension(:), allocatable :: v_phi
    real, dimension(:,:), allocatable :: v

    real, dimension(:), allocatable :: delta0i   !0.0 exept  i=i_zero then 1.0 for shifted Maxwellian

!index maps
    integer, dimension(:), allocatable :: p_pk_map
    integer, dimension(:), allocatable :: k_pk_map
    integer, dimension(:,:), allocatable :: pk_p_k_map

!time step
    integer :: n_step
    integer :: n_step_max

!data step
    integer :: n_data

    real :: imp    !imp=1.0  fast implicit !imp=0 fast explicit


!backup and restart parameters
    real :: time_odd
    real :: time_even
    integer :: n_step_odd
    integer :: n_step_even
    integer :: n_step_start

!MPI 
!    integer :: myid
    integer :: numprocs
!    integer :: rc
    integer :: ierr
    integer :: ki
    integer :: kimax
 
end       module M6D_use_grid
