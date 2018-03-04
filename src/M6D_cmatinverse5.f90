
subroutine M6D_cmatinverse5

!---------------------------------------------------------------
! PURPOSE:  complex square matrix inversion
! From ZHAO Deng   10.21.15
!---------------------------------------------------------------


    use M6D_use_grid     


    use M6D_use_matrix


  implicit none



   integer Nm

   integer jv

   integer iv


  complex*16, ALLOCATABLE, DIMENSION(:,:) :: Matrixtest


 ! Nm=2

 ! Nm = n_v3
   Nm = 5



  ALLOCATE(Matrixtest(1:Nm,1:Nm))


  do jv=1,Nm
   do iv = 1,Nm

     Matrixtest(jv,iv)=cmat5(jv,iv)

   enddo
  enddo



  call cmatrix_inverse(Nm,Matrixtest)


  do jv=1,Nm
   do iv = 1,Nm

     cmat5_inv(jv,iv) = Matrixtest(jv,iv)

   enddo
  enddo


end subroutine M6D_cmatinverse5


