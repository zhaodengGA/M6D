!---------------------------------------------------------
! M6D_getMfromMsup.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
!  get M'sfrom Msup  
!---------------------------------------------------------


subroutine M6D_getMfromMsup(myid)

    use MPI

    use M6D_use_grid    

    use M6D_use_profiles

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid
!----------------------------------------------------------------------------------


!mpi

!=================================================================================

! pk loop
!     do ki=0,kimax-1  !mpi
!       pk=myid*kimax+ki+1
      do pk=1,n_pk
        p=p_pk_map(pk)
        k=k_pk_map(pk)


!start  M stages 
!  using iv_map0 1 2 from local M's
!
!0-stage
     i=iv_map0
     M0(:,p,k)=Msup(i,:,pk)
!1-stage
    do j1=1,3 
     i=iv_map1(j1)
     M1(j1,:,p,k)=Msup(i,:,pk) 
    enddo
!2-stage
   do j2=1,3
    do j1=1,3
     i=iv_map2(j2,j1)
     M2(j2,j1,:,p,k)=Msup(i,:,pk)
    enddo
   enddo
!3-stage
  if(n_v3 .ge. 20) then
  do j3=1,3
   do j2=1,3
    do j1=1,3
     i=iv_map3(j3,j2,j1)
     M3(j3,j2,j1,:,p,k)=Msup(i,:,pk)
    enddo
   enddo
  enddo
  endif
!4-starge
  if(n_v3 .ge. 35) then
 do j4=1,3
  do j3=1,3
   do j2=1,3
    do j1=1,3
     i=iv_map4(j4,j3,j2,j1)
     M4(j4,j3,j2,j1,:,p,k)=Msup(i,:,pk)
    enddo
   enddo
  enddo
 enddo
  endif
!end  M stages

   enddo !pk
!  enddo ! ki loop

!fill in BC's  for M's 

!cyclic in theta 
    do p=-1,n_r
       M0(:,p,-1) =M0(:,p,n_theta-1) 
       M0(:,p,n_theta) = M0(:,p,0)

      do j1=1,3
        M1(j1,:,p,-1) =M1(j1,:,p,n_theta-1)
        M1(j1,:,p,n_theta) = M1(j1,:,p,0)
      enddo

      do j2=1,3
       do j1=1,3
        M2(j2,j1,:,p,-1) =M2(j2,j1,:,p,n_theta-1)
        M2(j2,j1,:,p,n_theta) = M2(j2,j1,:,p,0)
       enddo
      enddo

   if(n_v3 .ge. 20) then
     do j3=1,3
      do j2=1,3
       do j1=1,3
        M3(j3,j2,j1,:,p,-1) = M3(j3,j2,j1,:,p,n_theta-1)
        M3(j3,j2,j1,:,p,n_theta) = M3(j3,j2,j1,:,p,0)
       enddo
      enddo
     enddo
   endif

   if(n_v3 .ge. 35) then
    do j4=1,3
     do j3=1,3
      do j2=1,3
       do j1=1,3
        M4(j4,j3,j2,j1,:,p,-1) = M4(j4,j3,j2,j1,:,p,n_theta-1)
        M4(j4,j3,j2,j1,:,p,n_theta) = M4(j4,j3,j2,j1,:,p,0)
       enddo
      enddo
     enddo
    enddo
   endif

    enddo !p

! the radial BC p=-1 and p=n_r 

   do n=0,n_n-1
!    M0(n,-1,:) = 0.0
     M0(n,-1,:) = BC*M0(n,0,:) 
!    M0(n,n_r,:) = 0.0
     M0(n,n_r,:) = BC*M0(n,n_r-1,:) 

!    M1(:,n,-1,:) = 0.0
     M1(:,n,-1,:) = BC*M1(:,n,0,:)  
!    M1(:,n,n_r,:) = 0.0
     M1(:,n,n_r,:) = BC*M1(:,n,n_r-1,:) 

!    M2(:,:,n,-1,:) = 0.0
     M2(:,:,n,-1,:) = BC*M2(:,:,n,0,:)  
!    M2(:,:,n,n_r,:) = 0.0
     M2(:,:,n,n_r,:) = BC*M2(:,:,n,n_r-1,:) 

 if(n_v3 .ge. 20) then
  M3(:,:,:,n,-1,:) = BC*M3(:,:,:,n,0,:)
  M3(:,:,:,n,n_r,:) = BC*M3(:,:,:,n,n_r-1,:)  
 endif

 if(n_v3 .ge. 35) then
  M4(:,:,:,:,n,-1,:) = BC*M4(:,:,:,:,n,0,:)
  M4(:,:,:,:,n,n_r,:) = BC*M4(:,:,:,:,n,n_r-1,:)
 endif

   if(n_tor(n) .eq. 0) then
       M0(n,-1,:)=M00(-1)
       M0(n,n_r,:)=M00(n_r)

       do j1 = 1,3
         M1(j1,n,-1,:)=M01(j1,-1)
         M1(j1,n,n_r,:)=M01(j1,n_r)
       enddo

       do j2 = 1,3
        do j1 = 1,3
         M2(j2,j1,n,-1,:)=M02(j2,j1,-1)
         M2(j2,j1,n,n_r,:)=M02(j2,j1,n_r)
        enddo
       enddo

   if(n_v3 .ge. 20) then
      do j3 =1,3
       do j2 = 1,3
        do j1 = 1,3
         M3(j3,j2,j1,n,-1,:)=M03(j3,j2,j1,-1)
         M3(j3,j2,j1,n,n_r,:)=M03(j3,j2,j1,n_r)
        enddo
       enddo
      enddo
   endif

   if(n_v3 .ge. 35) then
     do j4 =1,3
      do j3 =1,3
       do j2 = 1,3
        do j1 = 1,3
         M4(j4,j3,j2,j1,n,-1,:)=M04(j4,j3,j2,j1,-1)
         M4(j4,j3,j2,j1,n,n_r,:)=M04(j4,j3,j2,j1,n_r)
        enddo
       enddo
      enddo
     enddo
   endif

   endif !n_tor = 0
  enddo !n


! boundary test
!    do p=-1,n_r
!     if (p .lt. 4) then
!      M0(:,p,:) = 0.0
!      M1(:,:,p,:) = 0.0
!      M2(:,:,:,p,:) = 0.0
!     endif
!    enddo

    if (myid==0 .and. n_step .eq. 1) then
!     print *, 'CAUSTION boundary test on p .lt. 4 zeroed'
     print *, 'time=',time
     print *, 'M6D_getMfromMsup done'
     if(n_v3 .gt. 10) print *, 'add M3'
     if(n_v3 .gt. 20) print *, 'add M4'
    endif

end subroutine M6D_getMfromMsup
