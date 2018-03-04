!---------------------------------------------------------
! M6D_setup_divgeos.f90
!
!
!--------------------------------------------------------
! PURPOSE:

! setup  general grid q1,q2,q3
! setup  Lame h's and Christofell Chris's do the divergence operatio
!   
!---------------------------------------------------------


subroutine M6D_setup_divgeo(myid)

    use MPI

    use M6D_use_grid     !

    use M6D_use_profiles  !



!
    implicit none
    integer,intent(in) :: myid
 
    real :: Hcap(3,3,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g)

    if(n_pk .eq. 1) return

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    endif

  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    h(1,p,k)=1./abs_grad_r(p,k)
    h(2,p,k)=1./abs_grad_theta(p,k)
    h(3,p,k)=Rmaj(p,k)
   enddo
  enddo

   if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'h(j1,p,k) done'
    print *, '----------------------------------------------------------'
   endif

   do p=-n_g,n_r-1+n_g
    q1(p) = r_hat(p)
   enddo
   do k=-n_g,n_theta-1+n_g
    q2(k) = theta(k)
   enddo
   do n=0,n_n-1
    q3(n) = float(n)*2.*pi/float(n_n)   !q3(n) is not actually used
   enddo
   


  Hcap(:,:,:,:) = 0.0
  do k=0,n_theta-1
    do p=0,n_r-1

     Hcap(1,1,p,k) =(h(1,p+1,k)-h(1,p-1,k))/(q1(p+1)-q1(p-1))
     Hcap(2,1,p,k) = (h(2,p+1,k)-h(2,p-1,k))/(q1(p+1)-q1(p-1))
     Hcap(3,1,p,k) = (h(3,p+1,k)-h(3,p-1,k))/(q1(p+1)-q1(p-1))

     Hcap(1,2,p,k) =(h(1,p,k+1)-h(1,p,k-1))/(q2(k+1)-q2(k-1))
     Hcap(2,2,p,k) = (h(2,p,k+1)-h(2,p,k-1))/(q2(k+1)-q2(k-1))
     Hcap(3,2,p,k) = (h(3,p,k+1)-h(3,p,k-1))/(q2(k+1)-q2(k-1))
   enddo
  enddo
  
  if(i_geom .eq. 1) then
   do j1=1,3 
    do j2=1,3

     do k=0,n_theta-1
      do p=0,n_r-1
       if(abs(Hcap(j1,j2,p,k)) .gt. 0.0000001) then
        if(myid .eq. 0) print *, j1,j2,p,k
        if(myid .eq. 0) print *, Hcap(j1,j2,p,k)
       endif
      enddo
     enddo

    enddo
   enddo
  endif
  if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'Hcap(j1j2,p,k) done'
    print *, '----------------------------------------------------------'
  endif
   
 do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    do j1=1,3
     do j2=1,3
      do j3=1,3
!       Chris(j1,j2,j3,p,k) =  (h(j2,p,k)*Hcap(j2,j3,p,k) &
!                               +h(j3,p,k)*Hcap(j3,j2,p,k) &
!                               -h(j2,p,k)*Hcap(j2,j1,p,k)*kdelta(j2,j3))/ &
!                                    h(j1,p,k)**2
!11.15.16  huge error fixed
       Chris(j1,j2,j3,p,k) =  (h(j2,p,k)*Hcap(j2,j3,p,k)*kdelta(j2,j1) &
                              +h(j3,p,k)*Hcap(j3,j2,p,k)*kdelta(j3,j1) &
                              -h(j2,p,k)*Hcap(j2,j1,p,k)*kdelta(j2,j3))/ &
                                    h(j1,p,k)**2

!    Gamma_upperj1_lowerj2j3  J2<->j3
      enddo
     enddo
    enddo
   enddo
 enddo

!  Chris(:,:,:,:,:)=0.
!  if (myid .eq. 0) then
!   print *, 'CAUTION:  Chris =0 test'
!  endif

  if(i_geom .eq. 1) Chris(:,:,:,:,:) = 0.0

  if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'i_geom=',i_geom
    print *, 'Chris(j1,j2,j3,p,k)  done'
    print *, '----------------------------------------------------------'
  endif
 

    if (myid .eq. 0) print *, 'M6D_setup_divgeo done'

end subroutine M6D_setup_divgeo
