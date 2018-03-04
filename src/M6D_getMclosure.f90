!---------------------------------------------------------
! M6D_getMclosure.f90f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! get the closure moment
!
! present version is not really mpi...i.e. every processor does the same work
!---------------------------------------------------------


subroutine M6D_getMclosure(myid)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles

    use M6D_use_advancematrix

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid
    complex, dimension(1:n_v3) :: w_loc

!----------------------------------------------------------------------------------

   do n=0,n_n-1

!get w_loc from Msup
!----------------------------------------------------------------------------------
! this pk loop is likely time consuming and maybe later done with mpi  ki-loop

    do pk = 1,n_pk
        p=p_pk_map(pk)
        k=k_pk_map(pk)

      w_loc(:) = 0.
      do i=1,n_v3
       do j=1,n_v3
        w_loc(i) = w_loc(i) + Ginv(i,j,pk)*Msup(j,n,pk)
       enddo
      enddo

!get the 10- moment  closure from w_loc
  if(n_v3 .eq. 10) then

   do j3 = 1,3
    do j2 = 1,3
     do j1 = 1,3
!maybe  :,:,: can be used here  for j3,j2,j1

      M3(j3,j2,j1,n,p,k) = 0.0
       do i = 1,n_v3
        M3(j3,j2,j1,n,p,k) = M3(j3,j2,j1,n,p,k) + G3(i,j3,j2,j1,pk)*w_loc(i)
       enddo

     enddo
    enddo
   enddo

 endif
!get the 20-moment closure from w_loc
  if(n_v3 .eq. 20) then

  do j4 = 1,4
   do j3 = 1,3
    do j2 = 1,3
     do j1 = 1,3
!maybe  :,:,: can be used here  for j3,j2,j1

      M4(j4,j3,j2,j1,n,p,k) = 0.0
       do i = 1,n_v3
        M4(j4,j3,j2,j1,n,p,k) = M4(j4,j3,j2,j1,n,p,k) + G4(i,j4,j3,j2,j1,pk)*w_loc(i)
       enddo

     enddo
    enddo
   enddo
  enddo

 endif


!for 40-moment comment out above and replace with G4
!for 121-moment commnet out above abd replace with G5
    enddo !pk loop
!----------------------------------------------------------------------------------

 if(n_v3 .eq. 10) then
! fill BC for M3
! cyclic theta 
   do p=-1,n_r
    M3(:,:,:,n,p,-1) = M3(:,:,:,n,p,n_theta-1)
    M3(:,:,:,n,p,n_theta) = M3(:,:,:,n,p,0)
   enddo
!radial BC M3 
!    M3(:,:,:,n,-1,:) = 0.0
     M3(:,:,:,n,-1,:) = BC*M3(:,:,:,n,0,:) 
!    M3(:,:,:,n,n_r,:) = 0.0
     M3(:,:,:,n,n_r,:) = BC*M3(:,:,:,n,n_r-1,:) 
   if(n_tor(n) .eq. 0) then
      do j3 = 1,3
       do j2 = 1,3
        do j1 = 1,3
         M3(j3,j2,j1,n,-1,:)=M03(j3,j2,j1,-1)
         M3(j3,j2,j1,n,n_r,:)=M03(j3,j2,j1,n_r)
        enddo
       enddo
      enddo
   endif
 endif

 if(n_v3 .eq. 20) then
! fill BC for M4
! cyclic theta 
   do p=-1,n_r
    M4(:,:,:,:,n,p,-1) = M4(:,:,:,:,n,p,n_theta-1)
    M4(:,:,:,:,n,p,n_theta) = M4(:,:,:,:,n,p,0)
   enddo
!radial BC M4
!    M4(:,:,:,:,n,-1,:) = 0.0
     M4(:,:,:,:,n,-1,:) = BC*M4(:,:,:,:,n,0,:)
!    M4(:,:,:,:,n,n_r,:) = 0.0
     M4(:,:,:,:,n,n_r,:) = BC*M4(:,:,:,:,n,n_r-1,:)
   if(n_tor(n) .eq. 0) then
     do j4 = 1,3
      do j3 = 1,3
       do j2 = 1,3
        do j1 = 1,3
         M4(j4,j3,j2,j1,n,-1,:)=M04(j4,j3,j2,j1,-1)
         M4(j4,j3,j2,j1,n,n_r,:)=M04(j4,j3,j2,j1,n_r)
        enddo
       enddo
      enddo
     enddo
   endif

 endif  !M4

   enddo !n loop



    if (myid==0 .and. n_step .eq. 1) then
     print *, 'M6D_getMclosure done'
    endif

end subroutine M6D_getMclosure
