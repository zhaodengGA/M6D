!---------------------------------------------------------
! M6D_getEfield.f90f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
! get the E-field vector
! use a simple phi-field model
!
! present version is not really mpi...i.e. every processor does the same work
!---------------------------------------------------------


subroutine M6D_getEfield(myid)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles

    use M6D_use_dynamic

!
  implicit none

    integer,intent(in) :: myid

    real :: TioTe
    real :: eps_small
    complex, dimension(0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g) :: phi 
!----------------------------------------------------------------------------------


    TioTe = 1.0
    eps_small = 0.01


   do n=0,n_n-1
    do p=-1,n_r
     do k=-1,n_theta
      phi(n,p,k) = temp0(p)/TioTe/den0(p)*(M0(n,p,k)-delta0n(n)*M00(p))/ &
                            (1.-xi*delta_i -delta0n(n)+delta0n(n)*eps_small)
     enddo !k loop
    enddo !p loop
   enddo !n loop

   do n=0,n_n-1

!    do p=0,n_r-1
!     do k=0,n_theta-1

!      E(1,n,p,k) = -1./h(1,p,k)*(phi(n,p+1,k)-phi(n,p-1,k))/(q1(p+1)-q1(p-1))
!      E(2,n,p,k) = -1./h(2,p,k)*(phi(n,p,k+1)-phi(n,p,k-1))/(q2(k+1)-q2(k-1))
!      E(3,n,p,k) = 1./h(3,p,k)*xi*n_tor(n)*phi(n,p,k)
!     enddo !k
!    enddo !p

! since there are no p to p+1,p-1 or k to k+1,k-1 derivative connections
! needed on E,  this pk loop can be speeded up by mpi.  E(j1) is calculated only
! on the precessors where used.

!     do ki=0,kimax-1  !mpi
!       pk=myid*kimax+ki+1
      do pk=1,n_pk
        p=p_pk_map(pk)
        k=k_pk_map(pk)
!TEST 0's
!      E(1,n,p,k) = 0.0
      E(1,n,p,k) = -1./h(1,p,k)*(phi(n,p+1,k)-phi(n,p-1,k))/(q1(p+1)-q1(p-1)) 
!      E(2,n,p,k) = 0.0
      E(2,n,p,k) = -1./h(2,p,k)*(phi(n,p,k+1)-phi(n,p,k-1))/(q2(k+1)-q2(k-1))
!      E(3,n,p,k) = 0.0
      E(3,n,p,k) = 1./h(3,p,k)*xi*n_tor(n)*phi(n,p,k)


      enddo !pk
!     enddo !ki
  
   enddo

    if(i_kxkykz .eq. 1) then  !must be removed for speed
     
   do n=0,n_n-1
    do p=-1,n_r
     do k=-1,n_theta

      E(1,n,p,k) = -xi*kx*phi(n,p,k)
      E(2,n,p,k) = -xi*ky*phi(n,p,k)
      E(3,n,p,k) = -xi*kz*phi(n,p,k)

     enddo
    enddo
   enddo

    endif

    if (myid==0 .and. n_step .eq. 1) then
     print *, 'i_kxkykz=',i_kxkykz
     print *, delta_i
     print *, 'M6D_getEfield done'
    endif

end subroutine M6D_getEfield
