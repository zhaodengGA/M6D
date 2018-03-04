!---------------------------------------------------------
! M6D_allocate_big.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!    allocate main matrices used many places
!   
!---------------------------------------------------------


subroutine M6D_allocate_big(myid)

    use MPI

    use M6D_use_grid     !

    use M6D_use_profiles  !

    use M6D_use_matrix  !

    use M6D_use_advancematrix !

    use M6D_use_dynamic


    implicit none
    integer,intent(in) :: myid

    allocate(r_hat(-n_g:n_r-1+n_g))
    allocate(theta(-n_g:n_theta-1+n_g))
    allocate(n_tor(0:n_n-1))
    allocate(delta0n(0:n_n-1))
    allocate(v(3,1:n_v3))
    allocate(delta0i(0:n_v3))

    allocate(p_pk_map(1:n_pk))
    allocate(k_pk_map(1:n_pk))
    allocate(pk_p_k_map(0:n_r-1,0:n_theta-1))

!general geometry divergence of tensors
    allocate(q1(-n_g:n_r-1+n_g))     !r in [r,theta] or R in [R,Z]
    allocate(q2(-n_g:n_theta-1+n_g)) !theta in [r,theta] or Z in [R,Z]
    allocate(q3(0:n_n-1))            !phi  in toroidal symmetry
!Lame' coefficents
    allocate(h(3,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!Christofell coefcicents of the 2nd kind
    allocate(Chris(3,3,3,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_grid allocated'
    print *, '----------------------------------------------------------'
    endif
   
    allocate(den0(-n_g:n_r-1+n_g))
    allocate(temp0(-n_g:n_r-1+n_g))
    allocate(v_th0(-n_g:n_r-1+n_g))

    allocate(nu_hat(-n_g:n_r-1+n_g))

    allocate(u0_r(-n_g:n_r-1+n_g))
    allocate(u0_theta(-n_g:n_r-1+n_g))
    allocate(u0_phi(-n_g:n_r-1+n_g))
!general
    allocate(u0(3,-n_g:n_r-1+n_g))

    allocate(Er0(-n_g:n_r-1+n_g))
!general
     allocate(E0(3,-n_g:n_r-1+n_g))

    allocate(qpro(-n_g:n_r-1+n_g))


    allocate(Rmaj(-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(abs_grad_r(-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(abs_grad_theta(-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))

    allocate(B_theta(-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(B_phi(-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
!general
    allocate(B0(3,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))

! static toroidally symemtric "F0" moments
    allocate(M00(-n_g:n_r-1+n_g))
    allocate(M01(3,-n_g:n_r-1+n_g))
    allocate(M02(3,3,-n_g:n_r-1+n_g))
    allocate(M03(3,3,3,-n_g:n_r-1+n_g))
    allocate(M04(3,3,3,3,-n_g:n_r-1+n_g))
! OK      for 13-moments
! need M05 for 40-moments
! need M06 for 121-moments

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_profiles allocated'
    print *, '----------------------------------------------------------'
    endif

    allocate(mat(1:n_v3,1:n_v3))
    allocate(mat_inv(1:n_v3,1:n_v3))

    allocate(mat0(1:n_v3))

    allocate(cmat(1:n_v3,1:n_v3))
    allocate(cmat_inv(1:n_v3,1:n_v3))

! for 5x5 collision

    allocate(mat5(5,5))
    allocate(mat5_inv(5,5))

    allocate(cmat5(5,5))
    allocate(cmat5_inv(5,5))

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_matrix allocated'
    print *, '----------------------------------------------------------'
    endif

! Msup moments to and from GRBF w's  matricies
! for 13-moments
! Msup=[M0,M1(1),M1(2),M1(3),M2(1,1),M2(1,2),M2(1,3),M2(2,1),M2(2,2),M2(2,3),M2(3,1),M2(3,2),M2(3,3)]
    allocate(G(1:n_v3,1:n_v3,1:n_pk))      !Msup from w
    allocate(Ginv(1:n_v3,1:n_v3,1:n_pk))   !w from Msup

 if(n_v3 .eq. 10)   allocate(G3(1:n_v3,3,3,3,1:n_pk))
 if(n_v3 .eq. 20)   allocate(G4(1:n_v3,3,3,3,3,1:n_pk))
 if(n_v3 .eq. 35)   allocate(G5(1:n_v3,3,3,3,3,3,1:n_pk))

!inverse collsion  matricies dependent on M0's
    allocate(Cinv(5,5,1:n_pk))

!dispersion matrix for IC modes
   allocate(Dmat(n_n,1:n_v3,n_v3))
    

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6DD_use_advancematrix allocated'
    print *, '----------------------------------------------------------'
    endif

    allocate(Msup(1:n_v3,0:n_n-1,1:n_pk))

    !mpi
    allocate(Msup_myid(1:n_v3,0:n_n-1,0:kimax-1))

    if (myid==0) then !mpi   
    allocate(Msup_myid_tot(1:n_v3,0:n_n-1,0:kimax-1,0:numprocs-1))
    endif

! dynamic  moments
    allocate(M0(0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(M1(3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(M2(3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(M3(3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
 if(n_v3 .ge. 20)    allocate(M4(3,3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
 if(n_v3 .ge. 35)    allocate(M5(3,3,3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))

    allocate(M0_sav(0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(M1_sav(3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
    allocate(M2_sav(3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
 if(n_v3 .gt. 10)    allocate(M3_sav(3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
 if(n_v3 .gt. 20)    allocate(M4_sav(3,3,3,3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))
! OK      for 10-moments
! need M4 for 20-moments
! need M5 for 35-moments


    allocate(E(3,0:n_n-1,-n_g:n_r-1+n_g,-n_g:n_theta-1+n_g))

    allocate(phi_mode(0:n_n-1))

    allocate(den_mode(0:n_n-1))


    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_dynamic allocated'
    print *, '----------------------------------------------------------'
    

    print *, 'M6D_allocate_big done'
    endif

end subroutine M6D_allocate_big
