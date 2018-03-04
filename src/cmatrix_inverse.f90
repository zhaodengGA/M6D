
  !Inverse a complex matrix, the number of rows of the matrix should be equal to the number of columns.
  
   
Subroutine cmatrix_inverse(Nm,Matrixinv)


  implicit none

  integer Nm
  integer info
  integer IPIV(1:Nm)

  complex*16  Matrixinv(1:Nm,1:Nm)
  complex*16  WORKinv(1:4*Nm)
   

  call zgetrf( Nm, Nm, Matrixinv, Nm, IPIV, info)
  if(info/=0) print *, 'Error occured in zgetrf!', 'info=',info, 'Nm=', Nm
  call zgetri(Nm, Matrixinv, Nm, IPIV, WORKinv, Nm, info )  
  if(info/=0) print *, 'Error occured in zgetri!', 'info=',info, 'Nm=', Nm
      

  return

  

end Subroutine cmatrix_inverse


