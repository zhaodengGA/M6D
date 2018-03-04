!---------------------------------------------------------
! M6D_explicitadvancef90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! advance moments 1 full time step simple time centeredpredictor-corrector
!   
!---------------------------------------------------------


subroutine M6D_explicitadvance(myid)

    use MPI

    use M6D_use_grid     

! add M6D_use_profiles to get sources

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid
!----------------------------------------------------------------------------------

    integer :: ihf   !half 1; full 2 
    real :: hf       ! del_t/2.  multiplier

    real :: S0
    real :: S1(3)
    real :: S2(3,3)
    real :: S3(3,3,3)
    real :: S4(3,3,3,3)

    real :: zeroTEST5
    real :: zeroTEST4
    real :: zeroTEST3
    real :: zeroTEST2
  

    integer :: i_callE

    complex :: RHS0
    complex :: RHS1(3)
    complex :: RHS2(3,3)
    complex :: RHS3(3,3,3)
    complex :: RHS4(3,3,3,3)

    integer :: i_M2symTEST

!mpi

!=================================================================================

    zeroTEST2 = 1.0
    zeroTEST3 = 1.0
!    zeroTEST3 = 0.0
    zeroTEST4 = 1.0
!     zeroTEST4 = 0.0
    zeroTEST5 = 1.0
     
    i_callE = 1  !0 yes callE, 1 no callE
!     i_callE = 0

    i_M2symTEST=0

! before first half step
    M0_sav(:,:,:) = M0(:,:,:)
    M1_sav(:,:,:,:) = M1(:,:,:,:)
    M2_sav(:,:,:,:,:) = M2(:,:,:,:,:)
    if(n_v3 .ge. 20) M3_sav(:,:,:,:,:,:) = M3(:,:,:,:,:,:)
    if(n_v3 .ge. 35) M4_sav(:,:,:,:,:,:,:) = M4(:,:,:,:,:,:,:)

   do ihf =1,2
    hf = float(ihf)

!TEST      call M6D_getEfield(myid)

!       call M6D_getEfield(myid)

    if (i_callE .eq. 1) call M6D_getEfield(myid)

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

! n loop
   do n=0,n_n-1
! pk loop
     do ki=0,kimax-1  !mpi
!      this is the main mpi work loop
       pk=myid*kimax+ki+1
        p=p_pk_map(pk)
        k=k_pk_map(pk)


    call M6D_getdivM(myid,n,pk) !requires 4 nearby  p & k  for each pk
    call M6D_getEM(myid,n,pk)  !maybe nonlinear n-couple
    call M6D_getBM(myid,n,pk)
! getCM not working
!    call M6D_getCM(myid,n,pk)
    CM0=0.
    CM1(:) = 0.
    CM2(:,:) = 0
    CM3(:,:,:) = 0.
    CM4(:,:,:,:) = 0.

!  add Sor0(p), Sor1(j1,p), Sor2(p)  source calls 
!    from M6_use_profles

!start advance M stages as Msup_myid(i,n,ki) 
!  using iv_map0 1 2 from local M's
!
!0-stage
     
!test CM0
!    if (p .eq. n_r/2 .and. k .eq. n_theta/2) then
!     print * , 'CM0=',CM0
!    endif

     RHS0=-divM1+CM0+S0  !CM0 should be 0
     i=iv_map0
     Msup_myid(i,n,ki) = M0_sav(n,p,k) + hf*del_t/2.*RHS0
!1-stage

!test CM1
!    if (p .eq. n_r/2 .and. k .eq. n_theta/2) then
!     print * , 'CM1=',CM1
!    endif

    do j1=1,3 
     RHS1(j1)=-zeroTEST2*divM2(j1)+ EM1(j1)+BM1(j1)+CM1(j1)+S1(j1)  !CM1(j1) should be 0
     i=iv_map1(j1)
     Msup_myid(i,n,ki) = M1_sav(j1,n,p,k) + hf*del_t/2.*RHS1(j1)
    enddo
!2-stage

!test CM2
!    if (p .eq. n_r/2 .and. k .eq. n_theta/2) then
!     print * , 'CM2=',CM2
!    endif

   do j2=1,3
    do j1=1,3
     RHS2(j2,j1)=-zeroTEST3*divM3(j2,j1)+EM2(j2,j1) + BM2(j2,j1)+CM2(j2,j1)+S2(j2,j1)  !S2 is diagonal 
     i=iv_map2(j2,j1)
     Msup_myid(i,n,ki) = M2_sav(j2,j1,n,p,k) + hf*del_t/2.*RHS2(j2,j1)
    enddo
   enddo

!3-stage
   if(n_v3 .ge. 20) then
  do j3=1,3
   do j2=1,3
    do j1=1,3
     RHS3(j3,j2,j1)=-zeroTEST4*divM4(j3,j2,j1)+EM3(j3,j2,j1) + &
        BM3(j3,j2,j1)+CM3(j3,j2,j1)+S3(j3,j2,j1)  !S3 is diagonal 
     i=iv_map3(j3,j2,j1)
     Msup_myid(i,n,ki) = M3_sav(j3,j2,j1,n,p,k) + hf*del_t/2.*RHS3(j3,j2,j1)
    enddo
   enddo
  enddo
  endif  !20

!4-stage
  if(n_v3 .ge. 35) then
 do j4=1,3
  do j3=1,3
   do j2=1,3
    do j1=1,3
     RHS4(j4,j3,j2,j1)=-zeroTEST5*divM5(j4,j3,j2,j1)+EM4(j4,j3,j2,j1) + &
        BM4(j4,j3,j2,j1)+CM4(j4,j3,j2,j1)+S4(j4,j3,j2,j1)  !S4 is diagonal 
     i=iv_map4(j4,j3,j2,j1)
     Msup_myid(i,n,ki) = M4_sav(j4,j3,j2,j1,n,p,k) + hf*del_t/2.*RHS4(j4,j3,j2,j1)
    enddo
   enddo
  enddo
 enddo
  endif  !35
!end advance M stages
     enddo ! pk loop
   enddo !n loop

!start mpi BCAST of Msup
!  Msup(i,n,pk) is "state variable" like w(i,n,pk)

 
     call MPI_GATHER(Msup_myid(:,:,:),n_v3*n_n*kimax,MPI_DOUBLE_COMPLEX,Msup_myid_tot(:,:,:,:),&
                 n_v3*n_n*kimax,MPI_DOUBLE_COMPLEX,0,MPI_COMM_WORLD,ierr)


     Msup(:,:,:)=(0,0)


     if (myid==0) then
       do  i=0,numprocs-1
         do  ki=0,kimax-1
         pk=i*kimax+ki+1
           Msup(:,:,pk)=Msup_myid_tot(:,:,ki,i)
         enddo
       enddo
     endif


     call MPI_BCAST(Msup,n_v3*n_n*n_pk,MPI_DOUBLE_COMPLEX,0,MPI_COMM_WORLD,ierr)

! end mpi BCAST of Msup


  call M6D_getMfromMsup(myid)  !Msup on all ps hence M's all ps
                           !this call also updates theta cyclic BC on the M's


  call M6D_getMclosure(myid)  !  Msup to w's to Mclosure

    
    enddo !ihf
      time = time+del_t   !end of complete time step

!    if(i_M2symTEST .eq. 1) then
!     if (time .ge. 0.6 .and. time .lt. 0.6+del_t) then
!      if(myid==0) then
!       print *, 'M2 symmetry test'
!       print *, M2(1,2,:,n_r/2,n_theta/2), M2(2,1,:,n_r/2,n_theta/2)
!       print *, M2(2,3,:,n_r/2,n_theta/2), M2(3,2,:,n_r/2,n_theta/2)
!       print *, M2(3,1,:,n_r/2,n_theta/2), M2(1,3,:,n_r/2,n_theta/2)
!      endif
!     endif
!    endif
!    M2symTEST  passed OK

    if (myid==0 .and. n_step .eq. 1) then
     print *, 'zeroTEST2=',zeroTEST2
     print *, 'zeroTEST3=',zeroTEST3
     print *, 'zeroTEST4=',zeroTEST4
     print *, 'zeroTEST5=',zeroTEST5
     print *, 'i_callE=',i_callE
     print *, 'i_M2symTEST=',i_M2symTEST
     print *, 'time=',time
     print *, 'M6D_explicitadvance done'
    endif

end subroutine M6D_explicitadvance
