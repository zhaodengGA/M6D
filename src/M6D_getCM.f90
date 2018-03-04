!--------------------------------------------------------
! M6D_getCM.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! get the (n,pk) RHS CM collision term  
!---------------------------------------------------------


subroutine M6D_getCM(myid,n_loc,pk_loc)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles

    use M6D_use_advancematrix

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid
    integer,intent(in) :: n_loc
    integer,intent(in) :: pk_loc

    integer :: p_loc
    integer :: k_loc

    integer :: mc1
    integer :: mc2

    integer :: i_Ccheck   ! 0 no, 1 yes check CM0 =0 and CM1(j1)=0

    complex :: X(5)
    complex :: c(5)

    complex :: cpart123
    complex :: cpart5
!----------------------------------------------------------------------------------


!mpi

!=================================================================================

        i_Ccheck = 0
!        i_Ccheck = 1  !did NOT pass CM0=0 CM1=0 test

        p_loc=p_pk_map(pk_loc)
        k_loc=k_pk_map(pk_loc)

! compute X's

    do j1=1,3
     X(j1) = M1(j1,n_loc,p_loc,k_loc) - M01(j1,p_loc)*delta0n(n_loc)
    enddo
    
     X(4) = M0(n_loc,p_loc,k_loc) - M00(p_loc)*delta0n(n_loc)
     
     X(5) = 0.
    do j1=1,3
      X(5) = X(5) + (M2(j1,j1,n_loc,p_loc,k_loc) - M02(j1,j1,p_loc)*delta0n(n_loc))/2.
    enddo

! NOTE:  the delta0n(n_loc) factor was left off 9.06.16 M6Dplan notes

! compute the c's
     c(:) = 0.
     do mc1=1,5
      do mc2=1,5
       c(mc1) = c(mc1) + Cinv(mc1,mc2,p_loc)*X(mc2)
      enddo
     enddo

! compute the CM2
    do j2=1,3
     do j1=1,3

      cpart123 =0.
      do j3=1,3
       cpart123 = cpart123 +c(j3)*M03(j3,j2,j1,p_loc)
      enddo
      cpart5=0.
      do j4=1,3
       cpart5 = cpart5 + c(5)*M04(j4,j4,j2,j1,p_loc)/2.
      enddo

      CM2(j2,j1) = -nu_hat(p_loc)*(M2(j2,j1,n_loc,p_loc,k_loc) &
                          - M02(j2,j1,p_loc)*delta0n(n_loc) &
                   - cpart123 - c(4)*M02(j2,j1,p_loc) - cpart5)

     enddo
    enddo

!compute CM1 and CM0
    CM1(:) = 0.
    CM0 = 0.

    if(i_Ccheck .eq. 1) then
!check CM1
     do j1=1,3

      cpart123 =0.
      do j2=1,3
       cpart123 = cpart123 +c(j2)*M02(j2,j1,p_loc)
      enddo
      cpart5=0.
      do j3=1,3
       cpart5 = cpart5 + c(5)*M03(j3,j3,j1,p_loc)/2.
      enddo

      CM1(j1) = -nu_hat(p_loc)*(M1(j1,n_loc,p_loc,k_loc)  &
                         - M01(j1,p_loc)*delta0n(n_loc) &
                   - cpart123 - c(4)*M01(j1,p_loc) - cpart5)

     enddo

!check CM0

      cpart123 =0.
      do j1=1,3
       cpart123 = cpart123 +c(j1)*M01(j1,p_loc)
      enddo
      cpart5=0.
      do j2=1,3
       cpart5 = cpart5 + c(5)*M02(j2,j2,p_loc)/2.
      enddo

      CM0 = -nu_hat(p_loc)*(M0(n_loc,p_loc,k_loc)  &
                         - M00(p_loc)*delta0n(n_loc) &
                   - cpart123 - c(4)*M00(p_loc) - cpart5)


    endif

!    if (myid==0 .and. n_step .eq. 1) then
!     print *, 'time=',time
!     print *, 'M6D_getCM done'
!    endif

end subroutine M6D_getCM
