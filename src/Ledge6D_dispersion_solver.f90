!---------------------------------------------------------
!  Ledge6D_dispersion_solver.f90
!
!--------------------------------------------------------
! PURPOSE:
!   find all complex eigenvalues  (frequency and growth rates) for IC test 
!   
!---------------------------------------------------------


subroutine Ledge6D_dispersion_solver(myid)

    use MPI

    use Ledge6D_use_grid     !


    use Ledge6D_use_advancematrix
!
   implicit none
   integer,intent(in) :: myid

   integer Nm

   integer jv
   integer iv
 
   complex*16 :: omega_root
  

  complex*16, ALLOCATABLE, DIMENSION(:,:) :: Matrixtest

  complex*16, ALLOCATABLE, DIMENSION(:) :: eigenOtest


  print *,  ' -xi*omega_root*w(j) = Sum_i [Dmat(n,j,i)*w(i)]'

! Nm=2
  Nm = n_v3



  ALLOCATE(Matrixtest(1:Nm,1:Nm))

  ALLOCATE(eigenOtest(1:Nm))


!  Matrixtest(1,1)=1
!  Matrixtest(1,2)=0
!  Matrixtest(2,1)=0
!  Matrixtest(2,2)=2

    print *, '-----------------------------------------------'
    print *, Nm, 'roots with omega_root values'

 do n=0,n_n-1
    print *, '-----------------------------------------------'
    print *, 'n=',n
    print *, '-----------------------------------------------'

  do jv=1,Nm
   do iv = 1,Nm
     Matrixtest(jv,iv)=Dmat(n,jv,iv)
   enddo
  enddo
 


  call eigenvaluesolver(Nm,Matrixtest,eigenOtest)

   

!  write(*,*)"Nm=",Nm

!  write(*,*)"eigenOtest=",eigenOtest

!  print *, 'eigensolver test'

!  print *, "Nm=",Nm

!  print *, "eigenOtest=",eigenOtest

! -xi*omega_root = eigenOtest  !hence  omega = xi*eigen0test

    do jv=1,Nm
     omega_root = xi*eigenOtest(jv)
     print *,n_tor(n),' ', jv, ' ',omega_root
    enddo
    print *, '-----------------------------------------------'

  enddo ! n

    print *, 'Ledge6D_dispersion_solver done'

end subroutine Ledge6D_dispersion_solver
