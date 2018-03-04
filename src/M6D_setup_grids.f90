!---------------------------------------------------------
! M6D_setup_grids.f90
!
!
!--------------------------------------------------------
! PURPOSE:

! test code for exploring GRBF 6D moment kinetics with ions and 
!   adiabatic electrons
!   
! Here, set-up grids and grid pointers
!---------------------------------------------------------


subroutine M6D_setup_grids(myid)

    use MPI

    use M6D_use_grid     !


   implicit none
   integer,intent(in) :: myid

   real :: del_r_hat
   real :: del_theta
   
   real :: v_abs
   real :: v_tot(3)
   real :: v_eps(3)
   real :: v_sign
   real :: v_eps1
   real :: v_eps2
   real :: v_eps3
   real :: v_zflip

   integer :: ix
   integer :: iy
   integer :: iz

   integer :: iv_cnt


!radial grid
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'radial grid'
    endif

   del_r_hat = (r_hat_b -r_hat_a)/float(n_r)
   do p=-n_g,n_r-1+n_g
    r_hat(p)=r_hat_a+float(p)*del_r_hat
    if (myid .eq. 0) print *, p,' ',r_hat(p)
   enddo

 if(n_r .eq. 1 .and. n_g .eq.0) then
   r_hat(0) = (r_hat_b + r_hat_a)/2.
    if (myid .eq. 0)   print * , 'set r_hat(0) =',r_hat(0)
 endif

!theta grid
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'theta grid'
    endif

   del_theta = 2.*pi/float(n_theta)
   do k=-n_g,n_theta-1+n_g
!1.28.16rew    theta(k) = 0.0+float(k)*del_theta
    theta(k) = 0.0+float(k)*del_theta -pi
    if (myid .eq. 0)    print *, k,' ',theta(k)
   enddo
   
!ntor grid
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'n_tor grid'
    endif

   delta0n(:)=0.
   do n=0,n_n-1
    n_tor(n) = n_min+n*n_del
    if(n_tor(n) .eq. 0) delta0n(n) = 1.0
    if (myid .eq. 0)    print *, n,' ',n_tor(n),' ',delta0n(n)
   enddo

!ntor grid
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'velocity grid'
    endif

  if(n_v1 .eq. 2 ) then

! Big problem with 13 moments and 13 velocities
! the 9 element vv stress tensor is symtric so there are only 6 indeendent
! elements. Hence 13-moment can determine only 10 GRBF velocity components

! off 3D v axis
!    v_eps(:) = 0.0  !6  null id G rows
    v_eps(:) = 0.1*v_max  !3 pair G rows   j2<->j1 symmetry
   
    i=0
    v_sign=1.0
    do ix = -n_v1, n_v1
      if(ix .ne. 0) then
          i=i+1
          v(1,i) = v_max*float(ix)/float(n_v1) +v_sign*v_eps(1)
          v(2,i) = 0.0 + v_sign*v_eps(2)
          v(3,i) = 0.0 + v_sign*v_eps(3)
          v_sign = -v_sign
       endif
    enddo
    v_sign=1.0
    do iy = -n_v1, n_v1
      if(iy .ne. 0) then
          i=i+1
          v(1,i) = 0.0 + v_sign*v_eps(1)
          v(2,i) = v_max*float(iy)/float(n_v1) + v_sign*v_eps(2)
          v(3,i) = 0.0 + v_sign*v_eps(3)
       endif
    enddo
    v_sign=1.0
    do iz = -n_v1, n_v1
      if(iz .ne. 0) then
          i=i+1
          v(1,i) = 0.0 + v_sign*v_eps(1)
          v(2,i) = 0.0 + v_sign*v_eps(2)
          v(3,i) = v_max*float(iz)/float(n_v1) + v_sign*v_eps(3)
       endif 
    enddo
    i=i+1
    i_zero=i
    v(1,i) = 0.0
    v(2,i) = 0.0
    v(3,i) = 0.0

   endif !13 moment and 13 velocities

  if(n_v1 .eq. 1 .and. n_v3 .eq. 10) then


! off 3D v axis
    v_eps(:) = 0.0  

    i=0
    v_sign=1.0
    do ix = -n_v1, n_v1
      if(ix .ne. 0) then
          i=i+1
          v(1,i) = v_max*float(ix)/float(n_v1) +v_sign*v_eps(1)
          v(2,i) = 0.0 + v_sign*v_eps(2)
          v(3,i) = 0.0 + v_sign*v_eps(3)
          v_sign = -v_sign
       endif
    enddo
    v_sign=1.0
    do iy = -n_v1, n_v1
      if(iy .ne. 0) then
          i=i+1
          v(1,i) = 0.0 + v_sign*v_eps(1)
          v(2,i) = v_max*float(iy)/float(n_v1) + v_sign*v_eps(2)
          v(3,i) = 0.0 + v_sign*v_eps(3)
       endif
    enddo
    v_sign=1.0
    do iz = -n_v1, n_v1
      if(iz .ne. 0) then
          i=i+1
          v(1,i) = 0.0 + v_sign*v_eps(1)
          v(2,i) = 0.0 + v_sign*v_eps(2)
          v(3,i) = v_max*float(iz)/float(n_v1) + v_sign*v_eps(3)
       endif
    enddo
! 6 on plus and minus of 3 axis
! now add 3 offsets
    i=i+1
    v(1,i) = v_max/sqrt(2.0)
    v(2,i) = v_max/sqrt(2.0)
    v(3,i) = v_max*0.0

    v_zflip=-1.0

    i=i+1
    v(1,i) = -v_max/sqrt(2.0)
    v(2,i) = v_max*0.0
    v(3,i) = v_max/sqrt(2.0)*v_zflip
!     v(3,i)=0.

    i=i+1
    v(1,i) = v_max*0.0
    v(2,i) = -v_max/sqrt(2.0)
    v(3,i) = -v_max/sqrt(2.0)*v_zflip
!     v(3,i)=0.
    
! now 9 plus zero-velocity gives 10-velocities
    i=i+1
    i_zero=i
    v(1,i) = 0.0
    v(2,i) = 0.0
    v(3,i) = 0.0

!make sure v_abs differ tp break any symmetry
!!!   v(:,1) = v(:,1)
!!!   v(:,2) = v(:,2)*1.1
!!!   v(:,3) = v(:,3)*0.9
!!!   v(:,4) = v(:,4)*1.2
!!!   v(:,5) = v(:,5)*0.8
!!!   v(:,6) = v(:,6)*1.3
!!!   v(:,7) = v(:,7)*0.7
!!!   v(:,8) = v(:,8)*1.4
!!!   v(:,9) = v(:,9)*0.6
!!!   v(:,10) = v(:,10)

   endif !13-moment and 10 velocities


   if(n_v3 .eq. 20) then
    v_eps1 = 0.5
    v_eps2 = 0.0
    v_eps3 = 0.2

    v(1,1) = 1.0+v_eps1*0.5
    v(2,1) = v_eps3
    v(3,1) = v_eps3
 
    v(1,2) = -1.0+v_eps1*0.5
    v(2,2) = -v_eps3
    v(3,2) = -v_eps3

    v(1,3) = v_eps3
    v(2,3) = 1.0+v_eps1*0.5
    v(3,3) = v_eps3

    v(1,4) = -v_eps3
    v(2,4) = -1.0+v_eps1*0.5
    v(3,4) = -v_eps3

    v(1,5) = v_eps3
    v(2,5) = v_eps3
    v(3,5) = 1.0+v_eps1*0.5

    v(1,6) = -v_eps3
    v(2,6) = -v_eps3
    v(3,6) = -1.0+v_eps1*0.5

    v(1,7) = v_eps3-v_eps1 + v_eps2
    v(2,7) = v_eps3-v_eps1
    v(3,7) = v_eps3-v_eps1

    v(1,8) = 0.0 -v_eps3 - v_eps2
    v(2,8) = 0.0 -v_eps3
    v(3,8) = 0.0 -v_eps3

    i_zero=8

    v(1,9) = 1./sqrt(2.)
    v(2,9) = 1./sqrt(2.)
    v(3,9) = 0.0 + v_eps3

    v(1,10) = -1./sqrt(2.)
    v(2,10) =0.0  + v_eps3
    v(3,10) = 1./sqrt(2.)

    v(1,11) = 0.0 + v_eps3
    v(2,11) = -1./sqrt(2.)
    v(3,11) = 1./sqrt(2.)

    v(1,12) = -v(1,9)
    v(2,12) = -v(2,9)
    v(3,12) = -v(3,9)

    v(1,13) = -v(1,10)
    v(2,13) = -v(2,10)
    v(3,13) = -v(3,10)

    v(1,14) = -v(1,11)
    v(2,14) = -v(2,11)
    v(3,14) = -v(3,11)
    
    v(1,15) = 1./sqrt(3.)
    v(2,15) = 1./sqrt(3.)
    v(3,15) = 1./sqrt(3.)

    v(1,16) = -1./sqrt(3.)
    v(2,16) = 1./sqrt(3.)
    v(3,16) = 1./sqrt(3.)

    v(1,17) = -1./sqrt(3.)
    v(2,17) = -1./sqrt(3.)
    v(3,17) = 1./sqrt(3.)

    v(1,18) = -v(1,15)
    v(2,18) = -v(2,15)
    v(3,18) = -v(3,15)

    v(1,19) = -v(1,16)
    v(2,19) = -v(2,16)
    v(3,19) = -v(3,16)

    v(1,20) = -v(1,17)
    v(2,20) = -v(2,17)
    v(3,20) = -v(3,17)

!make sure v_abs differ
! 1,3,5
    v(:,1) = 0.9*v(:,1)
    v(:,5) = 1.1*v(:,5)
! 2,4,6
    v(:,2) = 0.9*v(:,2)
    v(:,6) = 1.1*v(:,6)
! 9-14
    v(:,9) = 1.3*v(:,9)
    v(:,10) = 1.2*v(:,10)
    v(:,11) = 1.1*v(:,11)
    v(:,12) = 0.9*v(:,12)
    v(:,13) = 0.8*v(:,13)
    v(:,14) = 0.7*v(:,14)
! 15-20  casew100
!    v(:,15) = 1.3*v(:,15)
!    v(:,16) = 1.2*v(:,16)
!    v(:,17) = 1.1*v(:,17)
!    v(:,18) = 0.9*v(:,18)
!    v(:,19) = 0.8*v(:,19)
!    v(:,20) = 0.7*v(:,20)

    v(:,15) = 0.7*v(:,15)
    v(:,16) = 0.8*v(:,16)
    v(:,17) = 0.9*v(:,17)
    v(:,18) = 1.1*v(:,18)
    v(:,19) = 1.2*v(:,19)
    v(:,20) = 1.3*v(:,20)


   v(:,:) = v_max*v(:,:)



   endif


    if (myid .eq. 0) then
    print *, 'n_v3=',n_v3,' ','i_max=',i,' ','i_zero=',i_zero
    print *, 'v_eps1=', v_eps1
    print *, 'v_eps2=', v_eps2
    print *, 'v_eps3=', v_eps3
    print *, 'v_zflip=',v_zflip

     do i=1,n_v3
      print *,v(1,i),' ',v(2,i),' ',v(3,i)
     enddo
    endif


!v_check
    if (myid .eq. 0) then

    v_tot(:) = 0.
   do j1=1,3
    do i=1,n_v3
     v_tot(j1)= v_tot(j1) + v(j1,i)
    enddo
    print *,'j1=',j1, 'v_tot=',v_tot(j1)
   enddo
    v(1,8) = v(1,8)-v_tot(1)
    v(2,8) = v(2,8)-v_tot(2)
    v(3,8) = v(3,8)-v_tot(3)

    print *, 'i=8 v_tot adjustment'
    
    v_tot(:) = 0.
   do j1=1,3
    do i=1,n_v3
     v_tot(j1)= v_tot(j1) + v(j1,i)
    enddo
    print *,'j1=',j1, 'v_tot=',v_tot(j1)
   enddo


!v8 adjustment

    do i=1,n_v3
     v_abs=0.
     v_abs=sqrt(v(1,i)**2+v(2,i)**2+v(3,i)**2)
     print *, 'i=',i,'v_abs=',v_abs
    enddo

    endif


!pk grid pointers
    n_pk = n_r*n_theta
    pk=0
    do p=0,n_r-1
     do k= 0,n_theta-1
      pk = pk+1
      p_pk_map(pk)=p
      k_pk_map(pk)=k
      pk_p_k_map(p,k)=pk
     enddo
    enddo
    if (myid .eq. 0) print *, 'n_pk=',n_pk,' pk=',pk

!M to Msup map

    if (myid .eq. 0) then
     print *, 'iv_map 0 1 2 done'
     if(n_v3 .gt. 10) print *, 'need ivmap 3'
     if(n_v3 .gt. 20) print *, 'need ivmap 4'
    endif
!  larger iv_maps shoiul be done with j1,j2,j3 and j1,j2,j3,j4 loops. 
!  the ordering in clear 
   
!   13 moments and 10 velocities

    iv_map0 =1

    iv_map1(1) = 2  !fixed 123 -->234
    iv_map1(2) = 3
    iv_map1(3) = 4

    iv_map2(1,1) = 5
    iv_map2(1,2) = 6
    iv_map2(1,3) = 7
!    iv_map2(2,1) = 8  !same as 6
    iv_map2(2,1)  = 6
    iv_map2(2,2) =8 
    iv_map2(2,3) = 9
!    iv_map2(3,1) = 11  !same as 7
    iv_map2(3,1) = 7
!    iv_map2(3,2) = 12  !same as 9
    iv_map2(3,2) = 9
    iv_map2(3,3) = 10    !max at n_v3=10

   if(myid .eq. 0) then
    print *, '1=M0'
    print *, '2=M1(1)'
    print *, '3=M1(2)'
    print *, '4=M1(3)'
    print *, '5=M2(1,1)'
    print *, '6=M2(2,1)'
    print *, '7=M2(3,1)=M2(1,3)'
    print *, '8=M2(2,2)'
    print *, '9=M2(2,3)=M2(3,2)'
    print *, '10=M2(3,3)'
   endif
    

   call MPI_Barrier(MPI_COMM_WORLD, ierr)

    iv_cnt=0
    do j1=1,3
     do j2=1,3
      do j3=1,3
        iv_cnt = iv_cnt+1
     if(myid .eq. 0) then
        print *, 'iv_cnt=',iv_cnt
        print *, j1,j2,j3, j1*j2*j3
        print *, '-----------------------------------'
     endif
      enddo
     enddo
    enddo


   if(myid .eq. 0) then
    print *, '-----------------------------------'
     print *, 'iv_map3(j1,j2,j3)'
    print *, '-----------------------------------'
   endif
    iv_map3(:,:,:) = 0
    iv_cnt = 0
    do j1=1,3
     do j2=1,3
      do j3=1,3
       if(j3.ge.j2) then
        if(j2.ge.j1) then
         iv_cnt = iv_cnt+1 
          iv_map3(j1,j2,j3) = iv_cnt
     if(myid .eq. 0) then
       print *, 'iv_cnt=',iv_cnt
       print *, j1,j2,j3, j1*j2*j3
       print *, '-----------------------------------'
     endif
        endif
       endif
      enddo
     enddo
    enddo

   do j1=1,3
     do j2=1,3
      do j3=1,3
       if(iv_map3(j1,j2,j3) .gt. 0) then
         iv_map3(j2,j1,j3) = iv_map3(j1,j2,j3)
         iv_map3(j3,j2,j1) = iv_map3(j1,j2,j3)
         iv_map3(j1,j3,j2) = iv_map3(j1,j2,j3)
       endif
      enddo
     enddo
    enddo

   if(myid .eq. 0) then
    print *, '-----------------------------------'
     print *, 'test iv_map3(j1,j2,j3)'
    print *, '-----------------------------------'
   endif
    iv_cnt = 0
    do j1=1,3
     do j2=1,3
      do j3=1,3
       if(iv_map3(j1,j2,j3) .gt. 0) then
         iv_cnt = iv_cnt +1
       if(myid .eq. 0) then
          print *, 'iv_cnt=',iv_cnt
          print *, j1,j2,j3, j1*j2*j3
          print *, 'iv_map3=',iv_map3(j1,j2,j3)
          print *, '-----------------------------------'
      endif
       endif
      enddo
     enddo
    enddo
  
    iv_map3(:,:,:) = iv_map3(:,:,:) +10  !n_v3=20

   if(myid .eq. 0) then
    print *, '-----------------------------------'
     print *, 'iv_map4(j1,j2,j3,j4)'
    print *, '-----------------------------------'
   endif
    iv_map4(:,:,:,:) = 0
    iv_cnt = 0
    do j1=1,3
     do j2=1,3
      do j3=1,3
       do j4=1,3
      if(j4.ge.j3) then
       if(j3.ge.j2) then
        if(j2.ge.j1) then
         iv_cnt = iv_cnt+1
          iv_map4(j1,j2,j3,j4) = iv_cnt
     if(myid .eq. 0) then
       print *, 'iv_cnt=',iv_cnt
       print *, j1,j2,j3,j4, j1*j2*j3*j4
       print *, '-----------------------------------'
     endif
        endif
       endif
      endif

      enddo
     enddo
    enddo
   enddo

   do j1=1,3
     do j2=1,3
      do j3=1,3
       do j4=1,3
       if(iv_map4(j1,j2,j3,j4) .gt. 0) then

       iv_map4(j1,j2,j3,j4) = iv_map4(j1,j2,j3,j4)
       iv_map4(j1,j2,j4,j3) = iv_map4(j1,j2,j3,j4)
       iv_map4(j1,j3,j4,j2) = iv_map4(j1,j2,j3,j4)
       iv_map4(j1,j3,j2,j4) = iv_map4(j1,j2,j3,j4)
       iv_map4(j1,j4,j3,j2) = iv_map4(j1,j2,j3,j4)
       iv_map4(j1,j4,j2,j3) = iv_map4(j1,j2,j3,j4)

!       iv_map4(j2,j1,j3,j4) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j2,j1,j4,j3) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j2,j3,j4,j1) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j2,j3,j2,j4) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j2,j4,j3,j1) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j2,j4,j1,j3) = iv_map4(j1,j2,j3,j4)

!       iv_map4(j3,j2,j1,j4) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j3,j2,j4,j1) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j3,j1,j4,j2) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j3,j1,j2,j4) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j3,j4,j1,j2) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j3,j4,j2,j1) = iv_map4(j1,j2,j3,j4)

!       iv_map4(j4,j2,j3,j1) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j4,j2,j1,j3) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j4,j3,j1,j2) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j4,j3,j2,j1) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j4,j1,j3,j2) = iv_map4(j1,j2,j3,j4)
!       iv_map4(j4,j1,j2,j3) = iv_map4(j1,j2,j3,j4)


       endif
       enddo
      enddo
     enddo
    enddo

   if(myid .eq. 0) then
    print *, '-----------------------------------'
     print *, 'test iv_map4(j1,j2,j3,j4)'
    print *, '-----------------------------------'
   endif
    iv_cnt = 0
    do j1=1,3
     do j2=1,3
      do j3=1,3
       do j4=1,3
       if(iv_map4(j1,j2,j3,j4) .gt. 0) then
         iv_cnt = iv_cnt +1
       if(myid .eq. 0) then
          print *, 'iv_cnt=',iv_cnt
          print *, j1,j2,j3,j4, j1*j2*j3*j4
          print *, 'iv_map4=',iv_map4(j1,j2,j3,j4)
          print *, '-----------------------------------'
      endif
       endif
       enddo
      enddo
     enddo
    enddo
    
    iv_map4(:,:,:,:) = iv_map4(:,:,:,:) + 20    !n_v3 = 35

   call MPI_Barrier(MPI_COMM_WORLD, ierr)

   




!Maxwellian
    delta0i(:) = 0.
    delta0i(i_zero) = 1.0

!kdelta(j1,j2)
    kdelta(:,:) = 0.
    do j1=1,3
     do j2=1,3
      if(j2 .eq. j1) then
       kdelta(j1,j2) = 1.0
      endif
     enddo
    enddo

!cross-dot product eps(j1,j2,j3)
    eps(:,:,:)= 0.0


    eps(1,2,3) = 1.0
    eps(1,3,2) = -1.0

    eps(2,3,1) = +1.0
    eps(2,1,3) = -1.0

    eps(3,1,2) = +1.0
    eps(3,2,1) = -1.0
    

    if (myid .eq. 0)  print *, 'M6D_setup_grids done'

    
  

end subroutine M6D_setup_grids
