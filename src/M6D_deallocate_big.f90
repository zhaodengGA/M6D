!---------------------------------------------------------
! M6D_deallocate_big.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!    deallocate main matrices used many places
!   
!---------------------------------------------------------


subroutine M6D_deallocate_big(myid)

    use MPI

    use M6D_use_grid     !

    use M6D_use_profiles !

    use M6D_use_matrix  !

    use M6D_use_advancematrix !

    use M6D_use_dynamic


    implicit none
    integer,intent(in) :: myid

!deallocate(r_hat)
    deallocate(r_hat)
    deallocate(theta)
    deallocate(n_tor)
    deallocate(delta0n)
    deallocate(v)
    deallocate(delta0i)

    deallocate(p_pk_map)
    deallocate(k_pk_map)
    deallocate(pk_p_k_map)

!general geometry divergence of tensors
    deallocate(q1)     !r in [r,theta] or R in [R,Z]
    deallocate(q2) !theta in [r,theta] or Z in [R,Z]
    deallocate(q3)            !phi  in toroidal symmetry
!Lame' coefficents
    deallocate(h)
!Christofell coefcicents of the 2nd kind
    deallocate(Chris)

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_grid deallocated'
    print *, '----------------------------------------------------------'
    endif
   
    deallocate(den0)
    deallocate(temp0)
    deallocate(v_th0)

    deallocate(nu_hat)

    deallocate(u0_r)
    deallocate(u0_theta)
    deallocate(u0_phi)
!general
    deallocate(u0)

    deallocate(Er0)
!general
    deallocate(E0)

    deallocate(qpro)


    deallocate(Rmaj)
    deallocate(abs_grad_r)
    deallocate(abs_grad_theta)

    deallocate(B_theta)
    deallocate(B_phi)
!general
    deallocate(B0)

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_profiles deallocated'
    print *, '----------------------------------------------------------'
    endif

    deallocate(mat)
    deallocate(mat_inv)

    deallocate(mat0)

    deallocate(cmat)
    deallocate(cmat_inv)

! for 5x5 collision

    deallocate(mat5)
    deallocate(mat5_inv)

    deallocate(cmat5)
    deallocate(cmat5_inv)

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_matrix deallocated'
    print *, '----------------------------------------------------------'
    endif

! Msup moments to and from GRBF w's  matricies
! for 13-moments
! Msup=[M0,M1(1),M1(2),M1(3),M2(1,1),M2(1,2),M2(1,3),M2(2,1),M2(2,2),M2(2,3),M2(3,1),M2(3,2),M2(3,3)]
    deallocate(G)      !Msup from w
    deallocate(Ginv)   !w from Msup

 if(n_v3 .eq. 10)    deallocate(G3)
 if(n_v3 .eq. 20)   deallocate(G4)
 if(n_v3 .eq. 35)   deallocate(G5)

!inverse collsion  matricies dependent on M0's
    deallocate(Cinv)

    deallocate(Dmat)
    

    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6DD_use_advancematrix deallocated'
    print *, '----------------------------------------------------------'
    endif

    deallocate(Msup)

    !mpi
    deallocate(Msup_myid)

    if (myid==0) then !mpi   
    deallocate(Msup_myid_tot)
    endif

! dynamic  moments
    deallocate(M0)
    deallocate(M1)
    deallocate(M2)
    deallocate(M3)
 if(n_v3 .ge. 20)    deallocate(M4)
 if(n_v3 .ge. 35)    deallocate(M5)

    deallocate(M0_sav)
    deallocate(M1_sav)
    deallocate(M2_sav)
 if(n_v3 .gt. 10)   deallocate(M3_sav)
 if(n_v3 .gt. 20)    deallocate(M4_sav)
! OK      for 10-moments
! need M4 for 20-moments
! need M5 for 35-moments

! static toroidally symemtric "F0" moments
    deallocate(M00)
    deallocate(M01)
    deallocate(M02)
    deallocate(M03)
    deallocate(M04)  !13-mom collision
! OK      for 10-moments
! need M04 for 20-moments
! need M05 for 35-moments


    deallocate(E)

    deallocate(phi_mode)

    deallocate(den_mode)


    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'myid=',myid
    print *, 'M6D_use_dynamic deallocated'
    print *, '----------------------------------------------------------'
    

    print *, 'M6D_deallocate_big done'
    endif

end subroutine M6D_deallocate_big
