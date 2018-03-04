
 complex(kind=8) function det(N, mat)
     implicit none
     integer(kind=8), intent(in) :: N 
     complex(kind=8), intent(inout), dimension(:,:) :: mat
     integer(kind=8) :: i, info
     integer, allocatable :: ipiv(:)
     real(kind=8) :: sgn

     allocate(ipiv(N))
     ipiv = 0

     call zgetrf(N, N, mat, N, ipiv, info)

!     det = ONE
      det = 1.0

     do i = 1, N
         det = det*mat(i, i)
     end do

!     sgn = ONE
     sgn = 1.0

     do i = 1, N
         if(ipiv(i) /= i) then
             sgn = -sgn
         end if
     end do

     det = sgn*det   

 end function det

