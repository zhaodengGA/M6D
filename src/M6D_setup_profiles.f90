!---------------------------------------------------------
! M6D_setup_profiles.f90
!
!
!--------------------------------------------------------
! PURPOSE:

! setup radial profiles and fixed magnetic geometry with B_r = 0
!   
!---------------------------------------------------------


subroutine M6D_setup_profiles(myid)

    use MPI

    use M6D_use_grid     !

    use M6D_use_profiles  !



!
    implicit none
    integer,intent(in) :: myid

  real :: TioTe
  real :: Bunit(-n_g:n_r-1+n_g)
  real :: denom

!REW 1.09.16 
  real :: toroidal_on
  real :: cylinder_on

  integer :: kp  !k inner-loop lable

!hardwire
   toroidal_on = 1.0 !normal
!   toroidal_on = 0.0
   cylinder_on = 1.0 !normal
!   cylinder_on = 0.0

   if(i_geom .eq. 1) toroidal_on=0.0

   if (myid .eq. 0) print *, 'toroidal_on HARDWIRED=',toroidal_on
   if (myid .eq. 0) print *, 'cyclinder_on HARDWIRED=',cylinder_on

!hardwire
   TioTe = 1.0
   if (myid .eq. 0) print *, 'TioTe HARDWIRED to 1.0'
!hardwire
   Bunit(:) = 1.0  !in units of B_star and later probably a function of r
   if (myid .eq. 0) print *, 'Bunit(:) HARDWIRED to 1.0'
!density profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'density profile den0(r)'
    endif

   do p=-n_g,n_r-1+n_g
    den0(p)=exp(-(r_hat(p)-(r_hat_a+r_hat_b)/2.)*aoLn)
    if (myid .eq. 0)  print *, p,' ',den0(p)
   enddo

! temperature profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'temperature profile temp0(r) ','TioTe=',TioTe
    endif

   do p=-n_g,n_r-1+n_g
    temp0(p)=TioTe*exp(-(r_hat(p)-(r_hat_a+r_hat_b)/2.)*aoLT)
    if (myid .eq. 0) print *, p,' ',temp0(p)
   enddo

! thermal velocity profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'thermal velocity profile v_th0(r)' 
    endif

   do p=-n_g,n_r-1+n_g
    v_th0(p) = sqrt(2.*temp0(p))
    if (myid .eq. 0) print *, p,' ',v_th0(p)
   enddo

! nu_hat collisionality profile
    if (myid .eq. 0) then 
    print *, '----------------------------------------------------------'
    print *, 'nu_hat collisionality profile nu_hat(r)'
    endif

   do p=-n_g,n_r-1+n_g
    nu_hat(p) = nu_mid*den0(p)*TioTe**2/temp0(p)**2
    if (myid .eq. 0) print *, p,' ',nu_hat(p)
   enddo


! q profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'q profile qpro(r) ', 's_hat=',s_hat
    endif
   do p=-n_g,n_r-1+n_g
    qpro(p)  = q_mid*(2.*r_hat(p)/(r_hat_a+r_hat_b))**s_hat
   if (myid .eq. 0)  print *, p,' ',qpro(p)
   enddo

! u0 profiles
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'u0_r, u0_theta, u0_phi  profiles zeroed' 
    print *, 'u0(j1,p) vector filled for [r,theta] grid'
    endif
     u0_r(:) = 0.0
     u0_theta(:) = 0.0
     u0_phi(:) = 0.0

     u0(1,:) = u0_r(:)
     u0(2,:) = u0_theta(:)
     u0(3,:) = u0_phi(:)


! Er0 profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'Er0  profile zeroed'
    print *, 'E0(j1,p) vector filled fro [r,theta] grid'
    endif
     Er0(:) = 0.0
     E0(1,:) = Er0(:)
     E0(2,:) = 0.0
     E0(3,:) = 0.0


! Rmaj r-theta profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'Rmag r-theta  profile '
    endif

  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    Rmaj(p,k)  = Rmaj0_hat*(1.0+toroidal_on*r_hat(p)/Rmaj0_hat*cos(theta(k)))
    if(n_pk .eq. 1) Rmaj(0,0) = Rmaj0_hat
   enddo
  enddo

!1.28.16  insure theta cyclic
 if(n_theta .gt. 1) then
  Rmaj(:,n_theta) = Rmaj(:,0)
  Rmaj(:,-1) = Rmaj(:,n_theta-1)
 endif


  k=0
  if(n_theta .gt. 1) k = n_theta/2
  do p=-n_g,n_r-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',Rmaj(p,k)
  enddo

  p=0
  if(n_r .gt. 1) p = n_r/2
  do k=-n_g,n_theta-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',Rmaj(p,k)
  enddo

! abs_grad_r  r-theta profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'abs_grad_r r-theta  profile '
    endif

  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    abs_grad_r(p,k)  = 1.0
   enddo
  enddo

!1.28.16  insure theta cyclic
 if(n_theta .gt. 1) then
  abs_grad_r(:,n_theta) = abs_grad_r(:,0)
  abs_grad_r(:,-1) = abs_grad_r(:,n_theta-1)
 endif


  k=0
  if(n_theta .gt. 1) k = n_theta/2
  do p=-n_g,n_r-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',abs_grad_r(p,k)
  enddo

  p=0
  if(n_r .gt. 1) p = n_r/2
  do k=-n_g,n_theta-1+n_g
  if (myid .eq. 0)  print *, p,' ',k,' ',abs_grad_r(p,k)
  enddo


! abs_grad_theta  r-theta profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'abs_grad_theta r-theta  profile '
    endif

 if(i_geom .eq. 0) then
  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    abs_grad_theta(p,k)  = 1.0/r_hat(p)
   enddo
  enddo
 endif

 if(i_geom .eq. 1) then
  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    abs_grad_theta(p,k)  = 1.0/r_hat(n_r/2)
   enddo
  enddo
 endif

  if(cylinder_on .lt. 0.5) then
   p=n_r/2
   abs_grad_theta(:,:) = 1./r_hat(p)
  endif

!1.28.16  insure theta cyclic
 if(n_theta .gt. 1) then
  abs_grad_theta(:,n_theta) = abs_grad_theta(:,0)
  abs_grad_theta(:,-1) = abs_grad_theta(:,n_theta-1)
 endif

  k=0
  if(n_theta .gt. 1) k = n_theta/2
  do p=-n_g,n_r-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',abs_grad_theta(p,k)
  enddo

  p=0
  if(n_r .gt. 1) p = n_r/2
  do k=-n_g,n_theta-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',abs_grad_theta(p,k)
  enddo

! B_theta  r-theta profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'B_theta r-theta  profile '
    endif

 if(i_geom .eq. 0) then
  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    B_theta(p,k)  = Bunit(p)*r_hat(p)/Rmaj(p,k)/qpro(p)*abs_grad_r(p,k)
   enddo
  enddo
 endif
 
 if(i_geom .eq. 1) then
  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g
    B_theta(p,k)  =  0.0
   enddo
  enddo
 endif

 

  if(cylinder_on .lt. 0.5) then
   p=n_r/2
   B_theta(:,:)  = Bunit(p)*r_hat(p)/Rmaj(:,:)/qpro(p)*abs_grad_r(:,:)
  endif

!1.28.16  insure theta cyclic
 if(n_theta .gt. 1) then
  B_theta(:,n_theta) = B_theta(:,0)
  B_theta(:,-1) = B_theta(:,n_theta-1)
 endif

  k=0
  if(n_theta .gt. 1) k = n_theta/2
  do p=-n_g,n_r-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',B_theta(p,k)
  enddo

  p=0
  if(n_r .gt. 1) p = n_r/2
  do k=-n_g,n_theta-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',B_theta(p,k)
  enddo

! B_phi  r-theta profile
    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'B_phi r-theta  profile '
    endif

  do k=-n_g,n_theta-1+n_g
   do p=-n_g,n_r-1+n_g

   B_phi(p,k)  = Bunit(p)*Rmaj0_hat/Rmaj(p,k)/sqrt(1.0-r_hat(p)**Rmaj0_hat**2)

   if(n_theta .gt. 1) then
!denom
    denom = 0.
    do kp=0,n_theta-1
    denom = denom+(theta(k+1)-theta(k))/2./pi*  &
      0.5*(1./Rmaj(p,kp+1)/abs_grad_theta(p,kp+1)/abs_grad_r(p,kp+1)+  &
           1./Rmaj(p,kp)/abs_grad_theta(p,kp)/abs_grad_r(p,kp))
    enddo
     B_phi(p,k)  = Bunit(p)*r_hat(p)/Rmaj(p,k)/denom
   endif
    if(n_pk .eq. 1) B_phi(0,0) =  Bunit(0)
    if(toroidal_on .eq. 0.0) B_phi(p,k) =Bunit(0)
   enddo
  enddo

!1.28.16  insure theta cyclic
 if(n_theta .gt. 1) then
  B_phi(:,n_theta) = B_phi(:,0)
  B_phi(:,-1) = B_phi(:,n_theta-1)
 endif

  k=0
  if(n_theta .gt. 1) k = n_theta/2
  do p=-n_g,n_r-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',B_phi(p,k)
  enddo

   p=0
   if(n_r .gt. 1) p=n/2
  do k=-n_g,n_theta-1+n_g
   if (myid .eq. 0) print *, p,' ',k,' ',B_phi(p,k)
  enddo



 if (n_pk.eq. 1) then
   if (myid .eq. 0) print *, 'n_pk=',n_pk,' B_phi=Bunit=1.0 &  B_theta=0.0'
   B_phi(:,:) = 1.0
   B_theta(:,:) = 0.0
 endif     

   B0(1,:,:) =0.0
   B0(2,:,:) = B_theta(:,:)
   B0(3,:,:) = B_phi(:,:)
   if (myid .eq. 0) print *, 'B0(j1,p,k) vector filled for [r,theta] grid'

!set up M00, M01, M02, M03, M04  n=0 radial profiles
  
   do p=-n_g,n_r-1+n_g
    M00(p) = den0(p)
    do j1=1,3 
     M01(j1,p) = den0(p)*v_th0(p)*u0(j1,p)
     do j2=1,3
      M02(j2,j1,p) = den0(p)*v_th0(p)**2* &
       (u0(j2,p)*u0(j1,p)+kdelta(j2,j1)/2.) 
      do j3=1,3
       M03(j3,j2,j1,p) = den0(p)*v_th0(p)**3* &
        (u0(j3,p)*u0(j2,p)*u0(j1,p)+ &
          kdelta(j2,j1)*u0(j3,p)/2.+kdelta(j3,j1)*u0(j2,p)/2.+kdelta(j2,j3)*u0(j1,p)/2.)
       do j4=1,3
        M04(j4,j3,j2,j1,p) = den0(p)*v_th0(p)**4* &
        (u0(j4,p)*u0(j3,p)*u0(j2,p)*u0(j1,p)+ &
          u0(j2,p)*u0(j1,p)*kdelta(j4,j3)/2.+&
           u0(j3,p)*u0(j1,p)*kdelta(j2,j4)/2.+&
            u0(j3,p)*u0(j2,p)*kdelta(j4,j1)/2.+&
             u0(j3,p)*u0(j4,p)*kdelta(j2,j1)/2.+&
              u0(j2,p)*u0(j4,p)*kdelta(j3,j1)/2.+&
               u0(j1,p)*u0(j4,p)*kdelta(j3,j2)/2.+&
!no such term           kdelta(j4,j3)*kdelta(j3,j2)*kdelta(j2,j1)*3./4.+&
            kdelta(j4,j3)*kdelta(j2,j1)/4.+&
             kdelta(j4,j2)*kdelta(j3,j1)/4.+&
              kdelta(j4,j1)*kdelta(j2,j3)/4.)
       enddo
      enddo
     enddo
    enddo
   enddo

!set up test wave numbers
!   exp(i*(kx*x+ky*y+kz*z -omega*time)
    
!    kz=10./Rmaj0_hat  !for n=10
!    ky= 2./r_hat(n_r/2)  ! ky_hat = 0.4 for  omega_star = 10.
!    
!    kx=ky

!better first test fro i_geom = 1
! B = B_z   and B_x,B_y = 0,0
    
    kz=0.

!    kz = 0.1
!     kz=0.5/r_hat(n_r/2)  !test vs kx ky

!    ky=2./r_hat(n_r/2)  ! ky_hat = 0.4 for  omega_star = 10.
!    ky=0.2/r_hat(n_r/2)  ! ky_hat = 0.4 for  omega_star = 100.
!    ky=0.05/r_hat(n_r/2)  ! ky_hat = 0.1 for  omega_star = 100.
     ky=0.5/r_hat(n_r/2)  ! ky_hat = 0.1 for  omega_star = 10.
!TEST     ky = -ky
!     ky=1.0/r_hat(n_r/2)  ! ky_hat = 0.2 for  omega_star = 10.
!      ky=1.5/r_hat(n_r/2)  ! ky_hat = 0.3 for  omega_star = 10.
!    ky = 0.
!     kx=0.5/r_hat(n_r/2)  ! kx_hat = 0.1 for  omega_star = 10.
    kx = 0.

!test
    kx=ky
    ky=0.
     
!     ky=0

    

!    i_kxkykz =  0  ! 1=on   0=0ff
     i_kxkykz =  1  ! 1=on   0=0ff

!   i_kxkykz =  1  overrides  divM1, divM2, divM3 and get E 
    if(myid .eq. 0) then
     print *, 'n_v3=',n_v3
     print *, 'i_geom=',i_geom
     print *, 'i_kxkykz=',i_kxkykz
     print *, 'i_kxkykz=1 only makes sense for i_geo=1'
     print *, 'n_v3=20 divM4 programmed only for i_kxkykz=1'
     print *, 'n_v3=35 divM4 and divM5 programmed only for i_kxkykz=1'
     print *, 'kx=',kx
     print *, 'ky=',ky
     print *, 'kz=',kz
    endif
    if(n_v3 .gt. 10 .and. i_kxkykz .eq. 0) then
     if(myid .eq. 0) print *, 'STOP n_v3 .gt. 10 .and. i_kxkykz .eq. 0'
    endif
    if(n_v3 .gt. 10 .and. i_kxkykz .eq. 0) stop

     
   

    if (myid .eq. 0) print *, 'M6D_setup_profiles done'

end subroutine M6D_setup_profiles
