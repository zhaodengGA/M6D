!--------------------------------------------------------
! M6D_getEM.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! get the (n,pk) RHS EM term!   
!---------------------------------------------------------


subroutine M6D_getEM(myid,n_loc,pk_loc)

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

    if(n_n .eq. 1) then !linear run delta=1
     do j1=1,3
      EM1(j1) = &
        E(j1,n_loc,p_loc,k_loc)*M00(p_loc)+E0(j1,p_loc)*M0(n_loc,p_loc,k_loc)
     enddo
    do j2=1,3
     do j1=1,3
      EM2(j2,j1) = &
        E(j2,n_loc,p_loc,k_loc)*M01(j1,p_loc)+E0(j2,p_loc)*M1(j1,n_loc,p_loc,k_loc) &
       +E(j1,n_loc,p_loc,k_loc)*M01(j2,p_loc)+E0(j1,p_loc)*M1(j2,n_loc,p_loc,k_loc)
!symmeterized  j1 <-> j2  12.08.16
     enddo
    enddo

 if(n_v3 .ge. 20) then
   do J3=1,3
    do j2=1,3
     do j1=1,3
      EM3(j3,j2,j1) = &
        E(j3,n_loc,p_loc,k_loc)*M02(j2,j1,p_loc)+E0(j3,p_loc)*M2(j2,j1,n_loc,p_loc,k_loc) &
       +E(j2,n_loc,p_loc,k_loc)*M02(j1,j3,p_loc)+E0(j2,p_loc)*M2(j1,j3,n_loc,p_loc,k_loc) &
       +E(j1,n_loc,p_loc,k_loc)*M02(j2,j3,p_loc)+E0(j1,p_loc)*M2(j2,j3,n_loc,p_loc,k_loc)
!symmeterized  j1 <-> j2 <-> J3 
     enddo
    enddo
   enddo
  endif !20

 if(n_v3 .ge. 35) then
  do j4=1,3
   do J3=1,3
    do j2=1,3
     do j1=1,3
      EM4(j4,j3,j2,j1) = &
        E(j4,n_loc,p_loc,k_loc)*M03(j3,j2,j1,p_loc)+E0(j4,p_loc)*M3(j3,j2,j1,n_loc,p_loc,k_loc) &
       +E(j3,n_loc,p_loc,k_loc)*M03(j4,j2,j1,p_loc)+E0(j3,p_loc)*M3(j4,j2,j1,n_loc,p_loc,k_loc) &
       +E(j2,n_loc,p_loc,k_loc)*M03(j4,j3,j1,p_loc)+E0(j2,p_loc)*M3(j4,j3,j1,n_loc,p_loc,k_loc) &
       +E(j1,n_loc,p_loc,k_loc)*M03(j4,j3,j2,p_loc)+E0(j1,p_loc)*M3(j4,j3,j2,n_loc,p_loc,k_loc)
!symmeterized  j1 <-> j2 <-> J3 
     enddo
    enddo
   enddo
  enddo
 endif !35

 endif !n_n=1 linear

   if(n_n .gt. 1) then !nonlinear run delta =0
!   must set up n <0 complex conjugates in the convolution
     if (myid==0 .and. n_step .eq. 1) then
     print *, 'STOP and add nonlinear EM coding'
    endif
    stop
   endif

!    if (myid==0 .and. n_step .eq. 1) then
!     print *, 'time=',time
!     print *, 'M6D_getEM done'
!    endif

end subroutine M6D_getEM
