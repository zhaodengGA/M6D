!---------------------------------------------------------
! M6D_getMsupfromM.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
!  get Msup from M's!   
!---------------------------------------------------------


subroutine M6D_getMsupfromM(myid)

    use MPI

    use M6D_use_grid    

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
     Msup(i,:,pk) = M0(:,p,k) 
!1-stage
    do j1=1,3 
     i=iv_map1(j1)
     Msup(i,:,pk) = M1(j1,:,p,k) 
    enddo
!2-stage
   do j2=1,3
    do j1=1,3
     i=iv_map2(j2,j1)
     Msup(i,:,pk) = M2(j2,j1,:,p,k) 
    enddo
   enddo
!end  M stages

     enddo !pk
!   enddo !ki loop
     

    if (myid==0 .and. n_step .eq. 1) then
     print *, 'time=',time
     print *, 'M6D_getMsupfromM done'
     if(n_v3 .gt. 13) print *, 'add M3'
     if(n_v3 .gt. 40) print *, 'add M4'
    endif

end subroutine M6D_getMsupfromM
