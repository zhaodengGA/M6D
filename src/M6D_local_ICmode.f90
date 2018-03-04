!---------------------------------------------------------
! M6D_local_ICmode
!
!
!--------------------------------------------------------
! PURPOSE:
!
!  computer dispersion matrix R(i,j) for IC-mode local 
!      and solve for  eigenvalues
!   
!  programmed like M6D_explicitadvance(myid)
!---------------------------------------------------------


subroutine M6D_local_ICmode(myid)

    use MPI

    use M6D_use_grid     

    use M6D_use_dynamic

    use M6D_use_profiles

    use M6D_use_advancematrix

!
  implicit none

    integer,intent(in) :: myid
!----------------------------------------------------------------------------------


    real :: zeroTEST5
    real :: zeroTEST4
    real :: zeroTEST3
    real :: zeroTEST2
  

    integer :: i_callE

    integer :: ip
    integer :: jp

    integer :: n_loc
    integer :: p_loc
    integer :: k_loc
    integer :: pk_loc

    complex :: RHS(n_v3,n_v3)

    complex :: divM1_loc(3) !ZD
    complex :: divM2_loc(3,3) !ZD
    complex :: divM3_loc(3,3,3) !ZD
    complex :: divM4_loc(3,3,3)
    complex :: divM5_loc(3,3,3,3)

    complex :: EM1_loc(3)
    complex :: EM2_loc(3,3)
    complex :: EM3_loc(3,3,3)
    complex :: EM4_loc(3,3,3,3)

    complex :: BM1_loc(3)
    complex :: BM2_loc(3,3)
    complex :: BM3_loc(3,3,3)
    complex :: BM4_loc(3,3,3,3)


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

     p_loc=n_r/2
     k_loc=n_theta/2
     pk_loc=pk_p_k_map(p_loc,k_loc)

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

   if(myid .eq. 0) then
    print *, 'inside local_ICmode'
    print *, 'p=',p_loc,'k=',k_loc, 'n_v3=',n_v3
   endif


! n loop
   do n_loc=0,n_n-1
     
 
    RHS(:,:) = 0.0
     do jp=1,n_v3
       if(myid .eq. 0) print *, 'j=',jp
      EM1(:) = 0.
      BM1(:) = 0.
      EM2(:,:) = 0.
      BM2(:,:) = 0.
      divM1 = 0.
      divM2(:) = 0.
      divM3(:,:) = 0.
    if(n_v3 .ge. 20) then
     EM3(:,:,:) = 0.
     BM3(:,:,:) = 0.
     divM4(:,:,:) = 0.
    endif
   if(n_v3 .ge. 35) then
     EM4(:,:,:,:) = 0.
     BM4(:,:,:,:) = 0.
     divM5(:,:,:,:) = 0.
    endif
    

      E(:,:,:,:) =0.
      Msup(:,:,:)=0.
      Msup(jp,n_loc,pk_loc) = 1.0
      M0(:,:,:)=0.
      M1(:,:,:,:)=0.
      M2(:,:,:,:,:)=0.
      M3(:,:,:,:,:,:)=0.
   if(n_v3 .ge. 20) M4(:,:,:,:,:,:,:)=0.
   if(n_v3 .ge. 35) M5(:,:,:,:,:,:,:,:)=0.

      call M6D_getMfromMsup(myid)   !gets M's from Msup
!        for n_v3=10   M0,M1(j1),M2(j2,j1)

      call M6D_getMclosure(myid)  !  Msup to w's to Mclosure
!        for n_v3=10  M3(j3,j1,j1)

    if (i_callE .eq. 1) call M6D_getEfield(myid)  !requires M0

!!!!segF    call M6D_getdivM(myid,n,pk) !requires 4 nearby  p & k  for each pk
                                !unless  i_kxkykz=1 for the local IC mode case
!!!!segF    call M6D_getEM(myid,n,pk)  
!!!!segF    call M6D_getBM(myid,n,pk)    !seems ok

    enddo!jp !ZD

!getdivM_loc
     divM1_loc(:) = 0.  !ZD
     divM1_loc(1) = divM1_loc(1) - xi*kx !ZD
     divM1_loc(2) = divM1_loc(2) - xi*ky !ZD
     divM1_loc(3) = divM1_loc(3) - xi*kz !ZD

     divM2_loc(:,:) = 0. !ZD
     do j1=1,3  !ZD
      divM2_loc(1,j1) = divM2_loc(1,j1) - xi*kx !ZD
      divM2_loc(2,j1) = divM2_loc(2,j1) - xi*ky !ZD
      divM2_loc(3,j1) = divM2_loc(3,j1) - xi*kz !ZD
     enddo !ZD

    divM3_loc(:,:) = 0.
    do j2=1,3
     do j1=1,3
      divM3_loc(j2,j1) = divM3_loc(j2,j1) - xi*kx*M3(1,j2,j1,n_loc,p_loc,k_loc)
      divM3_loc(j2,j1) = divM3_loc(j2,j1) - xi*ky*M3(2,j2,j1,n_loc,p_loc,k_loc)
      divM3_loc(j2,j1) = divM3_loc(j2,j1) - xi*kz*M3(3,j2,j1,n_loc,p_loc,k_loc)
     enddo
    enddo

!getEM_loc
    EM1_loc(:) = 0.
     do j1=1,3
      EM1_loc(j1) = &
        E(j1,n_loc,p_loc,k_loc)*M00(p_loc)+E0(j1,p_loc)*M0(n_loc,p_loc,k_loc)
     enddo

    do j2=1,3
     do j1=1,3
      EM2_loc(j2,j1) = &
        E(j2,n_loc,p_loc,k_loc)*M01(j1,p_loc)+E0(j2,p_loc)*M1(j1,n_loc,p_loc,k_loc) &
       +E(j1,n_loc,p_loc,k_loc)*M01(j2,p_loc)+E0(j1,p_loc)*M1(j2,n_loc,p_loc,k_loc)
     enddo
    enddo

!getBM_loc
   BM1_loc(:) = 0.
    do j1=1,3
     do j2=1,3
      do j3=1,3
     BM1_loc(j1)=BM1_loc(j1) + M1(j2,n_loc,p_loc,k_loc)*eps(j1,j2,j3)*B0(j3,p_loc,k_loc)
      enddo
     enddo
    enddo
   BM1_loc(:) = omega_star*BM1_loc(:)

   BM2_loc(:,:) = 0.
 do j1=1,3
  do j2=1,3
   do j3=1,3
    do j4=1,3
      BM2_loc(j2,j1) = BM2_loc(j2,j1) +  &
        M2(j2,j3,n_loc,p_loc,k_loc)*eps(j1,j3,j4)*B0(j4,p_loc,k_loc)  &
       +M2(j1,j3,n_loc,p_loc,k_loc)*eps(j2,j3,j4)*B0(j4,p_loc,k_loc)
    enddo
   enddo
  enddo
 enddo
   BM2_loc(:,:) = omega_star*BM2_loc(:,:)


! get RHS(i,j)

!ZD--------
     ip=iv_map0
!     RHS(ip,jp)=-divM1
     do j1=1,3   
      jp=iv_map1(j1)
      RHS(ip,jp)=-divM1_loc(j1) 
          if(cabs(RHS(ip,jp)) .lt. 1.E-15) RHS(ip,jp)=0.0
      if(myid .eq. 0) print *, 'j=',jp, 'i=',ip, RHS(ip,jp)
     enddo  
   
     do j1=1,3 
      ip=iv_map1(j1) 
!      RHS(ip,jp)=-zeroTEST2*divM2(j1) + EM1(j1) + BM1(j1)
      !RHS(ip,jp)=-zeroTEST2*divM2_loc(j1) + EM1_loc(j1) + BM1_loc(j1)
       do j2=1,3 
        jp=iv_map2(j2,j1) 
        RHS(ip,jp)=RHS(ip,jp)-zeroTEST2*divM2_loc(j2,j1) 
          if(cabs(RHS(ip,jp)) .lt. 1.E-15) RHS(ip,jp)=0.0 
       if(myid .eq. 0) print *, 'j=',jp, 'i=',ip, RHS(ip,jp) 
       enddo 
     enddo 


    
   do j2=1,3
    do j1=1,3
     ip=iv_map2(j2,j1)
!     RHS(ip,jp)=-zeroTEST3*divM3(j2,j1) + EM2(j2,j1) + BM2(j2,j1)
     RHS(ip,jp)=-zeroTEST3*divM3_loc(j2,j1) + EM2_loc(j2,j1) + BM2_loc(j2,j1)
          if(cabs(RHS(ip,jp)) .lt. 1.E-15) RHS(ip,jp)=0.0
     if(myid .eq. 0) print *, 'j=',jp, 'i=',ip, RHS(ip,jp)
    enddo
   enddo

!3-stage
   if(n_v3 .ge. 20) then
  do j3=1,3
   do j2=1,3
    do j1=1,3
     ip=iv_map3(j3,j2,j1)
     RHS(ip,jp)=-zeroTEST4*divM4(j3,j2,j1)+EM3(j3,j2,j1) + BM3(j3,j2,j1)
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
     ip=iv_map4(j4,j3,j2,j1)
     RHS(ip,jp)=-zeroTEST5*divM5(j4,j3,j2,j1)+EM4(j4,j3,j2,j1) + &
       BM4(j4,j3,j2,j1)
    enddo
   enddo
  enddo
 enddo
  endif  !35
!end advance M stages

  !  enddo!jp !ZD

    if(myid==0) then
     print *, 'local_ICmode matrix R(i,j)'
     do ip=1,n_v3
      do jp=1,n_v3

       print *, 'i=',ip,'j=',jp
       print *, RHS(ip,jp)


      enddo
     enddo
    endif

!-----------------------------------------------------------------

   Dmat(n,:,:) = RHS(:,:)
   call M6D_dispersion_solver(myid)

   enddo !n


!    if (myid==0 ) then
     print *, 'zeroTEST2=',zeroTEST2
     print *, 'zeroTEST3=',zeroTEST3
     print *, 'zeroTEST4=',zeroTEST4
     print *, 'zeroTEST5=',zeroTEST5
     print *, 'i_callE=',i_callE
     print *, 'M6D_local_ICmode done'
!    endif

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

end subroutine M6D_local_ICmode
