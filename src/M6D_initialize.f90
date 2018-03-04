!---------------------------------------------------------
! M6D_initialize.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! advance moments 1 full time step simple time centeredpredictor-corrector
!   
!---------------------------------------------------------


subroutine M6D_initialize(myid)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid
    integer  :: i_startM
    real :: comp_amp
!----------------------------------------------------------------------------------
!    allocate(M0(0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!    allocate(M1(3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!    allocate(M2(3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!    allocate(M3(3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!!    allocate(M4(3,3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!!    allocate(M5(3,3,3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))

!    allocate(M0_sav(0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!    allocate(M1_sav(3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!    allocate(M2_sav(3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!!    allocate(M3_sav(3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!!    allocate(M4_sav(3,3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
! OK      for 13-moments
! need M4 for 40-moments
! need M5 for 121-moments


!=================================================================================

    comp_amp=0.0

!   i_startM =2   !start with M2
   i_startM = 1  !start with M1
!   i_startM =0   !start with M0

   if(restart .eq. 0) then

   time = 0.0
   n_step=0

    M0(:,:,:) = 0.0
    M1(:,:,:,:) = 0.0
    M2(:,:,:,:,:) = 0.0
    M3(:,:,:,:,:,:) = 0.0
  if(n_v3 .ge. 20) M4(:,:,:,:,:,:,:) = 0.0
  if(n_v3 .ge. 35) M5(:,:,:,:,:,:,:,:) = 0.0

    M0_sav(:,:,:) = 0.0
    M1_sav(:,:,:,:) = 0.0
    M2_sav(:,:,:,:,:) = 0.0
  if(n_v3 .ge. 20) M3_sav(:,:,:,:,:,:) = 0.0
  if(n_v3 .ge. 35) M4_sav(:,:,:,:,:,:,:) = 0.0

    Msup(:,:,:) = 0.0
    Msup_myid(:,:,:) = 0.0

 ! boundary and initial conditions on n=0 M
   do n=0,n_n-1
    if(n_tor(n) .eq. 0) then
     do p=-1,n_r
      M0(n,p,:) = M00(p)

      do j1=1,3
       M1(j1,n,p,:) =M01(j1,p)
      enddo

      do j2 =1,3
       do j1 =1,3
        M2(j2,j1,n,p,:) =M02(j2,j1,p)
       enddo
      enddo

      do j3 = 1,3
       do j2 = 1,3
        do j1 = 1,3
         M3(j3,j2,j1,n,p,:)=M03(j3,j2,j1,p)
        enddo
       enddo
      enddo

    if(n_v3 .ge. 20) then
     do j4 = 1,3
      do j3 = 1,3
       do j2 = 1,3
        do j1 = 1,3
         M4(j4,j3,j2,j1,n,p,:)=M04(j4,j3,j2,j1,p)
        enddo
       enddo
      enddo
     enddo
    endif

     enddo  !p
    endif !n_tor = 0

  if(i_startM .eq. 0) then
! starting perturbation on M0 which drives
!   and intial pertubation on the electric field
    do p=-1,n_r
     do k=0,n_theta-1
!      M0(n,p,k) = M0(n,p,k) + 0.1*(cos(theta(k))+1.0)
! 10/18/16
       if(p .ge. 0  .and. p .le. n_r-1) then
!TEST      if(p .ge. n_r/2-1 .and. p .le. n_r/2+1) then
       M0(n,p,k) = M0(n,p,k) + 0.1*(cos(theta(k))+1.0)*(1.0+comp_amp*xi) &
         *(cos(2.*pi*(float(p)/float(n_r-1)-0.5))+1.0)/2.0
       endif
!note:  starting M0 is non zero only for n_tor = 0
!       must be careful to subtract in poison quasineutral balance
     enddo !k
!cyclic condition
     M0(n,p,-1) = M0(n,p,n_theta-1)
     M0(n,p,n_theta-1) = M0(n,p,0)
    enddo !p
 endif !i_startM=0

 if(i_startM .eq. 1 ) then
!  do j1=1,3
    j1=2 !start with M1y  which should generate M1x  when   E=0 &  divM2 = 0
   do p=-1,n_r
     do k=0,n_theta-1
       if(p .ge. 0  .and. p .le. n_r-1) then
     M1(j1,n,p,k) = M1(j1,n,p,k) + 0.1*(cos(theta(k))+1.0)*(1.0+comp_amp*xi) &
         *(cos(2.*pi*(float(p)/float(n_r-1)-0.5))+1.0)/2.0
       endif
     enddo !k
!cyclic condition
     M1(j1,n,p,-1) = M1(j1,n,p,n_theta-1)
     M1(j1,n,p,n_theta-1) = M1(j1,n,p,0)
    enddo !p
!   enddo
 endif  !i_startM =1
 
 if(i_startM .eq. 2) then
 do j2=1,3
  do j1=1,3
   do p=-1,n_r
     do k=0,n_theta-1
       if(p .ge. 0  .and. p .le. n_r-1) then
       M2(j2,j1,n,p,k) = M2(j2,j1,n,p,k) + 0.1*(cos(theta(k))+1.0)*(1.0+comp_amp*xi) &
         *(cos(2.*pi*(float(p)/float(n_r-1)-0.5))+1.0)/2.0
       endif
     enddo !k
!cyclic condition
     M2(j2,j1,n,p,-1) = M2(j2,j1,n,p,n_theta-1)
     M2(j2,j1,n,p,n_theta-1) = M2(j2,j1,n,p,0)
    enddo !p
   enddo
  enddo
 endif  !i_startM =2

   enddo !n
!fix 10/18/16
   M0_sav(:,:,:) = M0(:,:,:)
   M1_sav(:,:,:,:) = M1(:,:,:,:)
   M2_sav(:,:,:,:,:) = M2(:,:,:,:,:)
  if(n_v3 .ge. 20) M3_sav(:,:,:,:,:,:) = M3(:,:,:,:,:,:)
  if(n_v3 .ge. 35) M4_sav(:,:,:,:,:,:,:) = M4(:,:,:,:,:,:,:)
  
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if(myid==0) then
   print *, 'time=',time,'  n_step=',n_step
    n_step_start=1
      call M6D_getdata(myid)  !open data field at n_step = 0 & time = 0.
  endif

  call MPI_Barrier(MPI_COMM_WORLD, ierr)
 
  endif !restart = 0

  if(restart .eq. 1 .or. restart .eq. 2) then
   !if(myid==0) then
    call M6D_restart(myid)
   !print *, 'STOP and program restart like Ledge6D'
   !endif
   !stop

! restart calls in Msup(n_v3,n_n,n_pk)   

  call M6D_getMfromMsup(myid)  !Msup on all ps hence M's all ps
                           !this call also updates theta cyclic BC on the M's

  call M6D_getMclosure(myid)  !  Msup to w's to Mclosure

  endif

    if (myid==0 .and. n_step .eq. 1) then
     print *, 'i_startM=',i_startM
     print *, 'time=',time
     print *, 'M6D_initialize done'
    endif

end subroutine M6D_initialize
