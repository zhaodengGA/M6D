!---------------------------------------------------------
! M6D_setup_advancematrix.f90
!
!
!--------------------------------------------------------
! PURPOSE:

! setup  G, Ginv, G3 matices
!   
!---------------------------------------------------------


subroutine M6D_setup_advancematrix(myid)

    use MPI

    use M6D_use_grid     !

    use M6D_use_profiles  !

    use M6D_use_matrix  !

    use M6D_use_advancematrix !



!
    implicit none
    integer,intent(in) :: myid
 
    real, dimension(n_v3) :: Gs0
    real, dimension(n_v3,3) :: GS1
    real, dimension(n_v3,3,3) :: GS2  
    real, dimension(n_v3,3,3,3) :: GS3
    real, dimension(n_v3,3,3,3,3) :: GS4
    real, dimension(n_v3,3,3,3,3,3) :: GS5

    real, dimension(n_v3,n_v3) :: GinvG
    integer :: ji
!    complex function :: det

! only n=0 real quantities 

    do pk = 1,n_pk
      p=p_pk_map(pk)
      k=k_pk_map(pk)

     Gs0(:) = 1.0 !all i filled

     do i=1,n_v3

      do j1=1,3
       Gs1(i,j1) = v_th0(p)*(v(j1,i)+u0(j1,p))
      enddo

      do j2=1,3
       do j1=1,3
       Gs2(i,j2,j1) =  &
         v_th0(p)**2*((v(j2,i)+u0(j2,p))*(v(j1,i)+u0(j1,p))+kdelta(j2,j1)/2.)
       enddo
      enddo

      do j3=1,3
       do j2=1,3
        do j1=1,3
        Gs3(i,j3,j2,j1) = &
          v_th0(p)**3*((v(j3,i)+u0(j3,p))*(v(j2,i)+u0(j2,p))*(v(j1,i)+u0(j1,p)) &
                  
              +(v(j3,i)+u0(j3,p))*kdelta(j2,j1)/2.  &
              +(v(j2,i)+u0(j2,p))*kdelta(j1,j3)/2.  &
              +(v(j1,i)+u0(j1,p))*kdelta(j3,j2)/2. )
        enddo
       enddo
      enddo
!  Gs3 must be 1<->2,1<->3,2<->3

    do j4=1,3
     do j3=1,3
       do j2=1,3
        do j1=1,3
        Gs4(i,j4,j3,j2,j1) = &
         v_th0(p)**4*((v(j4,i)+u0(j4,p))*(v(j3,i)+u0(j3,p))*(v(j2,i)+u0(j2,p))*(v(j1,i)+u0(j1,p)) &

              +(v(j2,i)+u0(j2,p))*(v(j1,i)+u0(j1,p))*kdelta(j4,j3)/2.  &
              +(v(j3,i)+u0(j3,p))*(v(j1,i)+u0(j1,p))*kdelta(j4,j2)/2.  &
              +(v(j3,i)+u0(j3,p))*(v(j2,i)+u0(j2,p))*kdelta(j4,j1)/2.  &
                   
              +(v(j3,i)+u0(j3,p))*(v(j4,i)+u0(j4,p))*kdelta(j2,j1)/2.  &
              +(v(j2,i)+u0(j2,p))*(v(j4,i)+u0(j4,p))*kdelta(j3,j1)/2.  &
              +(v(j1,i)+u0(j1,p))*(v(j4,i)+u0(j4,p))*kdelta(j3,j2)/2.  &

!no such term               + kdelta(j4,j3)*kdelta(j3,j2)*kdelta(j2,j1)*3./4. &
                
                +kdelta(j4,j3)*kdelta(j2,j1)/4. &
                +kdelta(j4,j2)*kdelta(j3,j1)/4. &
                +kdelta(j4,j1)*kdelta(j2,j3)/4. )
                 
 
        enddo
       enddo
      enddo
     enddo
! G4 must be  j symmetric

     if(n_v1 .eq. 1 .and. n_v3 .ge. 10) then
       G(1,i,pk) =  Gs0(i)

       G(2,i,pk) =  Gs1(i,1)
       G(3,i,pk) =  Gs1(i,2)
       G(4,i,pk) =  Gs1(i,3)

       G(5,i,pk) =  Gs2(i,1,1)
       G(6,i,pk) =  Gs2(i,1,2)
       G(7,i,pk) =  Gs2(i,1,3)
!       G(8,i,pk) =  Gs2(i,2,1) !same as 6
       G(8,i,pk) =  Gs2(i,2,2)
       G(9,i,pk) = Gs2(i,2,3)
!       G(11,i,pk) = Gs2(i,3,1) !same as 7
!       G(12,i,pk) = Gs2(i,3,2) !same as 9
       G(10,i,pk) = Gs2(i,3,3)
     endif !10

     if(n_v3 .ge. 20) then
       G(11,i,pk) = Gs3(i,1,1,1)
       G(12,i,pk) = Gs3(i,1,1,2)
       G(13,i,pk) = Gs3(i,1,1,3)
       G(14,i,pk) = Gs3(i,1,2,2)
       G(15,i,pk) = Gs3(i,1,2,3)
       G(16,i,pk) = Gs3(i,1,3,3)
       G(17,i,pk) = Gs3(i,2,2,2)
       G(18,i,pk) = Gs3(i,2,2,3)
       G(19,i,pk) = Gs3(i,2,3,3)
       G(20,i,pk) = Gs3(i,3,3,3)
     endif
  
     if(n_v3 .eq. 10)  G3(i,:,:,:,pk) = Gs3(i,:,:,:)  !G3 used only fo n_v3=10
     if(n_v3 .eq. 20)  G4(i,:,:,:,:,pk) =Gs4(i,:,:,:,:) 

     enddo !i

!  w's to Msup matrix
     mat(:,:) = G(:,:,pk)  ! real matrix

! from Ledge6D_cmatinverse     
    cmat(:,:) = mat(:,:)
    call M6D_cmatinverse
    mat_inv(:,:) = cmat_inv(:,:)
! from Ledge6D_cmatinverse

!  Msup to w's matrix
     Ginv(:,:,pk) = mat_inv(:,:)

!print Ginv test
    if(p .eq. n_r/2 .and.  k .eq.  n_theta/2 ) then
     if (myid .eq. 0) then
!      print *, 'det(mat)=', det(n_v3,cmat)
      print * , 'G'
      do j=1,n_v3
       print *, '------------------------------------------------------'
       do i=1,n_v3
        print *, 'j=',j,'i=',i, 'G=', mat(j,i)
       enddo
      enddo
!      print *, mat(:,:)
      print * ,'Ginv'
!      print *, Ginv(:,:,pk)
      do j=1,n_v3
       print *, '------------------------------------------------------'
       do i=1,n_v3
        print *, 'j=',j,'i=',i, 'Ginv=', mat_inv(j,i)
       enddo
      enddo
!inverse check
      GinvG(:,:) = 0.
     do i=1,n_v3
      do j=1,n_v3
       do ji=1,n_v3
       GinvG(j,i) =GinvG(j,i) +mat(j,ji)*mat_inv(ji,i)
       enddo
      enddo
     enddo
  
     print *,  'GinvG'
      do j=1,n_v3
       print *, '------------------------------------------------------'
       do i=1,n_v3
        print *, 'j=',j,'i=',i, 'GinvG=',GinvG(j,i)
       enddo
      enddo
      
     endif
    endif

!set collision matrix

    do j1=1,3
     mat5(1,j1) = M02(1,j1,p)
     mat5(2,j1) = M02(2,j1,p)
     mat5(3,j1) = M02(3,j1,p)
    enddo
! 9 done
   
    do j1=1,3
     mat5(j1,4)  = M01(j1,p)
    enddo
! 12 done

 
    mat5(:,5) = 0.0
    do j1=1,3
     do j2=1,3
      mat5(j1,5) = mat5(j1,5) + M03(j1,j2,j2,p)/2.
     enddo 
    enddo
! 15 done

     do j1=1,3
      mat5(4,j1) = M01(j1,p)
     enddo
! 18 done 

     mat5(4,4) = M00(p)
! 19 done


     mat5(4,5) = 0.0
     do j1=1,3
      mat5(4,5) = mat5(4,5) + M02(j1,j1,p)/2.
     enddo
! 20 done

    do j1 =1,3
     mat5(5,j1) =0.
     do j2 =1,3
      mat5(5,j1) = mat5(5,j1) + M03(j1,j2,j2,p)/2.
     enddo
    enddo
! 23 done

    mat5(5,4) = 0.0
    do j1=1,3
     mat5(5,4) = mat5(5,4) + M02(j1,j1,p)/2.
    enddo
! 24 done

    mat5(5,5) = 0.
    do j2=1,3
     do j1=1,3
      mat5(5,5) = mat5(5,5) + M04(j1,j1,j2,j2,p)/4.
     enddo
    enddo
! 25 done or 5x5 matrix


! from M6D_cmatinverse     
    cmat5(:,:) = mat5(:,:)
    call M6D_cmatinverse5
    mat5_inv(:,:) = cmat5_inv(:,:)
! from M6D_cmatinverse

!  Msup to w's matrix
     Cinv(:,:,pk) = mat5_inv(:,:)

!print Cinv test
    if(p .eq. n_r/2 .and.  k .eq.  n_theta/2 ) then
     if (myid .eq. 0) then
      print * ,'C'
!      print * , mat5(:,:)
      do j=1,5
       print *, '------------------------------------------------------'
       do i=1,5
        print *, 'j=',j,'i=',i, 'C=', mat5(j,i)
       enddo
      enddo
      print * ,'Cinv'
!      print *, Cinv(:,:,pk)
      do j=1,5
       print *, '------------------------------------------------------'
       do i=1,5
        print *, 'j=',j,'i=',i, 'Cinv=', mat5_inv(j,i)
       enddo
      enddo

     endif
    endif
     
    enddo !pk



    if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    endif


    if (myid .eq. 0) print *, 'M6D_setup_advancematrix done'

end subroutine M6D_setup_advancematrix
