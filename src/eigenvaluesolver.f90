Subroutine eigenvaluesolver(Nm,MatrixEG,eigenV)

  



  implicit none

  integer Nm

  integer info

  complex*16  DUMMYEG(1,1)



  complex*16  MatrixEG(1:Nm,1:Nm)

  complex*16  eigenV(1:Nm)

  complex*16  WORKEV(1:4*Nm)

 

  call ZGEEV('N', 'N', Nm, MatrixEG, Nm, eigenV, DUMMYEG, Nm, DUMMYEG, Nm, WORKEV, Nm*2, WORKEV, info)
  if(info .ne. 1) print *, 'Error in ZGEEV', 'info=',info



  !eigenV(:)=eigenV(:)/(0,-1)



  return

  

end Subroutine eigenvaluesolver







