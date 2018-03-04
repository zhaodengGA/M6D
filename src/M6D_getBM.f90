!--------------------------------------------------------
! M6D_getBM.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! get the (n,pk) RHS BM term!  
!---------------------------------------------------------


subroutine M6D_getBM(myid,n_loc,pk_loc)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid
    integer,intent(in) :: n_loc
    integer,intent(in) :: pk_loc

    integer :: p_loc
    integer :: k_loc
!----------------------------------------------------------------------------------


!mpi

!=================================================================================


        p_loc=p_pk_map(pk_loc)
        k_loc=k_pk_map(pk_loc)

   BM1(:) = 0.
    do j1=1,3
     do j2=1,3
      do j3=1,3
     BM1(j1)=BM1(j1) + M1(j2,n_loc,p_loc,k_loc)*eps(j1,j2,j3)*B0(j3,p_loc,k_loc)
      enddo
     enddo
    enddo
!the 27 products have only 6 non-zeros.  Should be same as below
!     BM1(1) = M1(2,n_loc,p_loc,k_loc)*B0(3,p_loc,k_loc) &
!                 -M1(3,n_loc,p_loc,k_loc)*B0(2,p_loc,k_loc)
!
!     BM1(2) = M1(3,n_loc,p_loc,k_loc)*B0(1,p_loc,k_loc) &
!                 -M1(1,n_loc,p_loc,k_loc)*B0(3,p_loc,k_loc)
!
!     BM1(3) = M1(1,n_loc,p_loc,k_loc)*B0(2,p_loc,k_loc) &
!                 -M1(2,n_loc,p_loc,k_loc)*B0(1,p_loc,k_loc)
     BM1(:) = omega_star*BM1(:)
     
! From  H&M  p215  Eq. 6.32  we have

   BM2(:,:) = 0.
 do j1=1,3
  do j2=1,3
   do j3=1,3
    do j4=1,3
      BM2(j2,j1) = BM2(j2,j1) +  &
        M2(j2,j3,n_loc,p_loc,k_loc)*eps(j1,j3,j4)*B0(j4,p_loc,k_loc)  &
       +M2(j1,j3,n_loc,p_loc,k_loc)*eps(j2,j3,j4)*B0(j4,p_loc,k_loc)
!note j1 <-> j2 symmetry
! 2x3**3 = 2x81 products but many fewer non-zero
    enddo
   enddo
  enddo
 enddo
   BM2(:,:) = omega_star*BM2(:,:)

!!generalization to BM3 is straightforward
! From  H&M  p215  Eq. 6.32  we have
 if(n_v3 .ge. 20) then
  BM3(:,:,:) = 0.
 do j1=1,3
  do j2=1,3
   do j3=1,3
    do j4=1,3
     do j5=1,3
      BM3(j3,j2,j1) = BM3(j3,j2,j1) +  &
        M3(j2,j3,j4,n_loc,p_loc,k_loc)*eps(j1,j4,j5)*B0(j5,p_loc,k_loc)  &
       +M3(j1,j3,j4,n_loc,p_loc,k_loc)*eps(j2,j4,j5)*B0(j5,p_loc,k_loc)  &
       +M3(j1,j2,j4,n_loc,p_loc,k_loc)*eps(j3,j4,j5)*B0(j5,p_loc,k_loc)
!check j1 <-> j2 <-> j3 symmetry
! 3x 243 products but many fewer non-zero
     enddo
    enddo
   enddo
  enddo
 enddo
  BM3(:,:,:) = omega_star*BM3(:,:,:)
 endif !20

 if(n_v3 .ge. 35) then
  BM4(:,:,:,:) = 0.
 do j1=1,3
  do j2=1,3
   do j3=1,3
    do j4=1,3
     do j5=1,3
      do j6=1,3
      BM4(j4,j3,j2,j1) = BM4(j4,j3,j2,j1) +  &
        M4(j2,j3,j4,j5,n_loc,p_loc,k_loc)*eps(j1,j5,j6)*B0(j6,p_loc,k_loc)  &
       +M4(j1,j3,j4,j5,n_loc,p_loc,k_loc)*eps(j2,j5,j6)*B0(j6,p_loc,k_loc)  &
       +M4(j1,j2,j4,j5,n_loc,p_loc,k_loc)*eps(j3,j5,j6)*B0(j6,p_loc,k_loc)  &
       +M4(j1,j2,j3,j5,n_loc,p_loc,k_loc)*eps(j4,j5,j6)*B0(j6,p_loc,k_loc)
!check j symmetry
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
  BM4(:,:,:,:) = omega_star*BM4(:,:,:,:)
 endif !35


!    if (myid==0 .and. n_step .eq. 1) then
!     print *, 'time=',time
!     print *, 'M6D_getBM done'
!    endif

end subroutine M6D_getBM
