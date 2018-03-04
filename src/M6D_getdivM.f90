!--------------------------------------------------------
! M6D_getdivM.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! get the (n,pk) RHS divM term!  
!---------------------------------------------------------


subroutine M6D_getdivM(myid,n_loc,pk_loc)

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

! there is no divM0


   if(i_kxkykz .eq. 0) then

! get divM1
! ----------------------------------------------------------------------------

     divM1 = 0.

!3 part j1 addition

! q1 derivative
      divM1 = divM1 +  &
         (M1(1,n_loc,p_loc+1,k_loc)/h(1,p_loc+1,k_loc) &
          - M1(1,n_loc,p_loc-1,k_loc)/h(1,p_loc-1,k_loc)) &
            /(q1(p_loc+1)-q1(p_loc-1))

! q2 derivative
      divM1 = divM1 +  &
         (M1(2,n_loc,p_loc,k_loc+1)/h(2,p_loc,k_loc+1) &
          - M1(2,n_loc,p_loc,k_loc-1)/h(2,p_loc,k_loc-1)) &
            /(q2(k_loc+1)-q2(k_loc-1))
      
! q3 "derivative"
      divM1 = divM1 +  &
         (-xi*n_tor(n_loc))*M1(3,n_loc,p_loc,k_loc)/h(3,p_loc,k_loc)  

! add Chris parts
      do j1=1,3
       do j2=1,3
         divM1 = divM1 + &
          Chris(j2,j1,j2,p_loc,k_loc)*M1(j2,n_loc,p_loc,k_loc)/h(j2,p_loc,k_loc)
       enddo
      enddo

! get divM2
! ----------------------------------------------------------------------------

!  3 part j2 addition

    divM2(:) = 0.0
    
    do j1=1,3
! q1 derivative
      divM2(j1)= divM2(j1) +  &
         (M2(1,j1,n_loc,p_loc+1,k_loc)/h(1,p_loc+1,k_loc)/h(j1,p_loc+1,k_loc) &
          - M2(1,j1,n_loc,p_loc-1,k_loc)/h(1,p_loc-1,k_loc)/h(j1,p_loc-1,k_loc)) &
            /(q1(p_loc+1)-q1(p_loc-1)) &
               *h(j1,p_loc,k_loc)

! q2 derivative
      divM2(j1) = divM2(j1) +  &
         (M2(2,j1,n_loc,p_loc,k_loc+1)/h(2,p_loc,k_loc+1)/h(j1,p_loc,k_loc+1) &
          - M2(2,j1,n_loc,p_loc,k_loc-1)/h(2,p_loc,k_loc-1)/h(j1,p_loc,k_loc-1)) &
            /(q2(k_loc+1)-q2(k_loc-1)) &
              *h(j1,p_loc,k_loc)

! q3 "derivative"
      divM2(j1) = divM2(j1) +  &
        (-xi*n_tor(n_loc))*M2(3,j1,n_loc,p_loc,k_loc)/h(3,p_loc,k_loc)/h(j1,p_loc,k_loc) &
           *h(j1,p_loc,k_loc)
    enddo !j1

! add Chris two parts
     do j1=1,3
      do j2=1,3
       do j3=1,3
         divM2(j1) = divM2(j1) + h(j1,p_loc,k_loc)*( &
          Chris(j2,j2,j3,p_loc,k_loc)*M2(j1,j3,n_loc,p_loc,k_loc)/h(j3,p_loc,k_loc)/h(j1,p_loc,k_loc) &
         +Chris(j1,j2,j3,p_loc,k_loc)*M2(j2,j3,n_loc,p_loc,k_loc)/h(j2,p_loc,k_loc)/h(j3,p_loc,k_loc))
!11.17.16  fixed  M2(j2,j1,...) --> M2(j1,j3,...)
       enddo
      enddo
     enddo

! get divM3
! ----------------------------------------------------------------------------

  divM3(:,:) = 0

! 3 part j3 addition
   do j2=1,3
    do j1=1,3
! q1 derivative
      divM3(j2,j1) = divM3(j2,j1) +  &
         (M3(1,j2,j1,n_loc,p_loc+1,k_loc)/h(1,p_loc+1,k_loc)/h(j2,p_loc+1,k_loc)/h(j1,p_loc+1,k_loc) &
          - M3(1,j2,j1,n_loc,p_loc-1,k_loc)/h(1,p_loc-1,k_loc)/h(j2,p_loc-1,k_loc)/h(j1,p_loc-1,k_loc)) &
            /(q1(p_loc+1)-q1(p_loc-1)) &
               *h(j2,p_loc,k_loc)*h(j1,p_loc,k_loc)

! q2 derivative
      divM3(j2,j1) = divM3(j2,j1) +  &
         (M3(2,j2,j1,n_loc,p_loc,k_loc+1)/h(2,p_loc,k_loc+1)/h(j2,p_loc,k_loc+1)/h(j1,p_loc,k_loc+1) &
          - M3(2,j2,j1,n_loc,p_loc,k_loc-1)/h(2,p_loc,k_loc-1)/h(j2,p_loc,k_loc-1)/h(j1,p_loc,k_loc-1)) &
            /(q2(k_loc+1)-q2(k_loc-1)) &
              *h(j2,p_loc,k_loc)*h(j1,p_loc,k_loc)

! q3 "derivative"
      divM3(j2,j1) = divM3(j2,j1) +  &
        (-xi*n_tor(n_loc))*M3(3,j2,j1,n_loc,p_loc,k_loc)/h(3,p_loc,k_loc)/h(j2,p_loc,k_loc)/h(j1,p_loc,k_loc) &
           *h(j2,p_loc,k_loc)*h(j1,p_loc,k_loc)
    enddo !j1
   enddo !j2

! add Chris three parts
     do j1=1,3
      do j2=1,3
       do j3=1,3
        do j4=1,3
         divM3(j2,j1) = divM3(j2,j1) + h(j2,p_loc,k_loc)*h(j1,p_loc,k_loc)*( &
          Chris(j3,j3,j4,p_loc,k_loc)*M3(j4,j2,j1,n_loc,p_loc,k_loc)/h(j4,p_loc,k_loc)/h(j2,p_loc,k_loc)/h(j1,p_loc,k_loc) &
         +Chris(j2,j3,j4,p_loc,k_loc)*M3(j3,j4,j1,n_loc,p_loc,k_loc)/h(j3,p_loc,k_loc)/h(j4,p_loc,k_loc)/h(j1,p_loc,k_loc) &
         +Chris(j1,j3,j4,p_loc,k_loc)*M3(j3,j2,j4,n_loc,p_loc,k_loc)/h(j3,p_loc,k_loc)/h(j2,p_loc,k_loc)/h(j4,p_loc,k_loc))
        enddo
       enddo
      enddo
     enddo
!NOTE: divM4 and divM5 available only for i_kxkykx =1

    endif

    if(i_kxkykz .eq. 1) then  !this "if" has to be removed for speed



     divM1 = 0.

     divM1 = divM1 - xi*kx*M1(1,n_loc,p_loc,k_loc)
     divM1 = divM1 - xi*ky*M1(2,n_loc,p_loc,k_loc)
     divM1 = divM1 - xi*kz*M1(3,n_loc,p_loc,k_loc)

!test print
!     if(myid .eq. 0) print *, 'divM1=',divM1

     divM2(:) = 0.

     do j1=1,3
      divM2(j1) = divM2(j1) - xi*kx*M2(1,j1,n_loc,p_loc,k_loc)
      divM2(j1) = divM2(j1) - xi*ky*M2(2,j1,n_loc,p_loc,k_loc)
      divM2(j1) = divM2(j1) - xi*kz*M2(3,j1,n_loc,p_loc,k_loc)
!test print
!       if(myid .eq. 0) print *, 'divM2(j2)=',divM2(j1)
     enddo 

    divM3(:,:) = 0.

    do j2=1,3
     do j1=1,3
      divM3(j2,j1) = divM3(j2,j1) - xi*kx*M3(1,j2,j1,n_loc,p_loc,k_loc)
      divM3(j2,j1) = divM3(j2,j1) - xi*ky*M3(2,j2,j1,n_loc,p_loc,k_loc)
      divM3(j2,j1) = divM3(j2,j1) - xi*kz*M3(3,j2,j1,n_loc,p_loc,k_loc)
!test print
!       if(myid .eq. 0) print *, 'divM3(j2,j1)=',divM3(j2,j1)
     enddo
    enddo
   
   if(n_v3 .ge. 20) then

    divM4(:,:,:) = 0.

   do j3=1,3
    do j2=1,3
     do j1=1,3
      divM4(j3,j2,j1) = divM4(j3,j2,j1) - xi*kx*M4(1,j3,j2,j1,n_loc,p_loc,k_loc)
      divM4(j3,j2,j1) = divM4(j3,j2,j1) - xi*ky*M4(2,j3,j2,j1,n_loc,p_loc,k_loc)
      divM4(j3,j2,j1) = divM4(j3,j2,j1) - xi*kz*M4(3,j3,j2,j1,n_loc,p_loc,k_loc)
     enddo
    enddo
   enddo
  endif !20

  if(n_v3 .ge. 35) then
   divM5(:,:,:,:) = 0.

  do j4=1,3
   do j3=1,3
    do j2=1,3
     do j1=1,3
      divM5(j4,j3,j2,j1) = divM5(j4,j3,j2,j1) - xi*kx*M5(1,j4,j3,j2,j1,n_loc,p_loc,k_loc)
      divM5(j4,j3,j2,j1) = divM5(j4,j3,j2,j1) - xi*ky*M5(2,j4,j3,j2,j1,n_loc,p_loc,k_loc)
      divM5(j4,j3,j2,j1) = divM5(j4,j3,j2,j1) - xi*kz*M5(3,j4,j3,j2,j1,n_loc,p_loc,k_loc)
     enddo
    enddo
   enddo
  enddo
  endif !35

    endif
 

    if (myid==0 .and. n_step .eq. 1) then
     print *, i_kxkykz
     print *, 'M6D_getdivM done'
    endif


end subroutine M6D_getdivM

