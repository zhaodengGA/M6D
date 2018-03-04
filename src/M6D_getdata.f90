!----i-----------------------------------------------------
! M6D_getdata.f90
!
!
!--------------------------------------------------------
! PURPOSE:
!
!  get data at a data step and write to files X.out
!   
!---------------------------------------------------------


subroutine M6D_getdata(myid)

    use MPI

    use M6D_use_grid     !

    use M6D_use_profiles  !

    use M6D_use_dynamic

!
  implicit none
    integer,intent(in) :: myid

!----------------------------------------------------------------------------------

    

 integer :: p_data
 integer :: k_data

 real :: den
 real :: gamma_r
 
 complex :: phi_mode_prev(0:n_n-1)
 complex :: den_mode_prev(0:n_n-1)
 complex :: omega_mode(0:n_n-1)
 complex :: omega1x_mode(0:n_n-1)

 complex :: den_mode3D(0:n_n-1,0:n_r-1,0:n_theta-1)

!Temporary variables for adjusting the position in the output files
 integer :: integtemp
 real :: realtemp
 complex :: complextemp

!=================================================================================
if (myid .eq. 0) then  
  if((n_step .eq. 0) .and. (restart .eq. 0)) then !creat output files at n_step=0

    open(unit=1,file='time.out',status='replace')
    close(1)

    open(unit=2,file='den.out',status='replace')
    close(2)

    open(unit=3,file='gamma_r.out',status='replace')
    close(3)

    open(unit=4,file='omega_mode.out',status='replace')
    close(4)

    open(unit=10,file='omega1x_mode.out',status='replace')
    close(10)

    open(unit=5,file='phi_mode.out',status='replace')
    close(5)

    open(unit=6,file='den_mode.out',status='replace')
    close(6)

    open(unit=9,file='den_mode3D.out',status='replace')
    close(9)

    open(unit=7,file='Msup_odd.backup',status='replace')  !ZD_add
    write(7,*) 0.0000000000001
    write(7,*) 0.0
    close(7)

    open(unit=8,file='Msup_even.backup',status='replace')
    write(8,*) 0.0000000000002
    write(8,*) 0.0
    close(8)

  endif


!  outboard (theta = 0)  moments and fluxes at midradius


  p_data = n_r/2
  k_data = n_theta/2
  if(n_r .eq. 1) p_data = 0
  if(n_theta .eq. 1) k_data = 0

  p=p_data
  k=k_data
  pk=pk_p_k_map(p,k)

  den = 0.0
  gamma_r = 0.0
! n loop
   do n=0,n_n-1



!     den=den + real(w(i,n,pk))*(2.-delta0n(n))
      den = M0(n,p,k) - delta0n(n)*M00(p)
!     gamma_r = gamma_r+v_th0(p)*(v_r(i)+u0_r(p))*real(w(i,n,pk))*(2.-delta0n(n))
      gamma_r = M1(1,n,p,k)
      

!      den_mode(n) = Sum(w(:,n,pk))
      den_mode(n) = M0(n,p,k) - delta0n(n)*M00(p)
!      den_mode_prev(n) = Sum(w_sav(:,n,pk))
      den_mode_prev(n) = M0_sav(n,p,k) - delta0n(n)*M00(p)
!     since M0_sav is at start of step and M0 at end this should be OK

!    phi_mode(n) = 0.0
!    M1x test
    phi_mode(n) = M1(1,n,p,k)


    
    omega_mode(n) = xi*(den_mode(n)-den_mode_prev(n))*2./&
                                 (den_mode(n)+den_mode_prev(n))/del_t

    omega1x_mode(n) = xi*(M1(1,n,p,k)-M1_sav(1,n,p,k))*2./&
                           (M1(1,n,p,k)+M1_sav(1,n,p,k))/del_t

!TEST print
!    print *, 'time=',time
!    print *, 'M1=',M1(1,n,p,k)
!    print *, 'M1_sav=',M1_sav(1,n,p,k)
!    print *, 'omega1x_mode=',omega1x_mode(n)
!    print *,'-----------------------------------------------'

   enddo !n loop
!    den = den -den0(p)*(1.0-delta)

    time_prev = time

!  compute 3D complex density amlitude
    do n=0,n_n-1
     do p=0,n_r-1
      do k=0,n_theta-1
       pk=pk_p_k_map(p,k)
!        den_mode3D(n,p,k) = Sum(w(:,n,pk))
         den_mode3D(n,p,k) =  M0(n,p,k) - delta0n(n)*M00(p)
      enddo
     enddo
    enddo


  if((restart .eq. 0) .or. (restart .eq. 1)) then
    open(unit=1,file='time.out',status='old',position='append')
     write(1,*) time
    close(1)

    open(unit=2,file='den.out',status='old',position='append')
     write(2,*) den
    close(2)

    open(unit=3,file='gamma_r.out',status='old',position='append')
     write(3,*) gamma_r
    close(3)
 
    open(unit=4,file='omega_mode.out',status='old',position='append')
     do n=0,n_n-1
      write(4,*) n_tor(n),omega_mode(n)
     enddo
    close(4)

    open(unit=10,file='omega1x_mode.out',status='old',position='append')
     do n=0,n_n-1
      write(10,*) n_tor(n),omega1x_mode(n)
     enddo
    close(10)

    open(unit=5,file='phi_mode.out',status='old',position='append')
     do n=0,n_n-1
      write(5,*) n_tor(n),phi_mode(n)
     enddo
    close(5)

    open(unit=6,file='den_mode.out',status='old',position='append')
     do n=0,n_n-1
      write(6,*) n_tor(n),den_mode(n)
     enddo
    close(6)

   open(unit=9,file='den_mode3D.out',status='old',position='append')
     do n=0,n_n-1
      do p=0,n_r-1
       do k=0,n_theta-1
      write(9,*) den_mode3D(n,p,k)
       enddo
      enddo
     enddo
    close(9)


    elseif(restart .eq. 2) then
    if(n_step .eq. min(n_step_odd,n_step_even)) then 
! restart and  roll back when initialize
      open(unit=1,file='time.out',status='old',position='rewind')
      do i=1,n_step/n_data
        read(1,*) realtemp
      enddo
      write(1,*) time
      close(1)

      open(unit=2,file='den.out',status='old',position='rewind')
      do i=1,n_step/n_data
        read(2,*) realtemp
      enddo
      write(2,*) den
      close(2)

      open(unit=3,file='gamma_r.out',status='old',position='rewind')
      do i=1,n_step/n_data
        read(3,*) realtemp
      enddo
      write(3,*) gamma_r
      close(3)

      open(unit=4,file='omega_mode.out',status='old',position='rewind')
      do i=1,n_n*n_step/n_data
        read(4,*) integtemp,complextemp
      enddo
      do n=0,n_n-1
        write(4,*) n_tor(n),omega_mode(n)
      enddo
      close(4)

     open(unit=10,file='omega1x_mode.out',status='old',position='rewind')
      do i=1,n_n*n_step/n_data
        read(10,*) integtemp,complextemp
      enddo
      do n=0,n_n-1
        write(10,*) n_tor(n),omega1x_mode(n)
      enddo
      close(10)

      open(unit=5,file='phi_mode.out',status='old',position='rewind')
      do i=1,n_n*n_step/n_data
        read(5,*) integtemp,complextemp
      enddo
      do n=0,n_n-1
        write(5,*) n_tor(n),phi_mode(n)
      enddo
      close(5)

      open(unit=6,file='den_mode.out',status='old',position='rewind')
      do i=1,n_n*n_step/n_data
        read(6,*) integtemp,complextemp
      enddo
      do n=0,n_n-1
        write(6,*) n_tor(n),den_mode(n)
      enddo
      close(6)

     open(unit=9,file='den_mode3D.out',status='old',position='rewind')
      do i=1,n_pk*n_n*n_step/n_data
        read(9,*) complextemp
      enddo
      do n=0,n_n-1
       do p=0,n_r-1
        do k=0,n_theta-1
        write(9,*) den_mode3D(n,p,k)
        enddo
       enddo
      enddo
      close(9)


    elseif(n_step .gt. min(n_step_odd,n_step_even)) then
    !output the data at the end of each file.   
      open(unit=1,file='time.out',status='old',position='append')
      write(1,*) time
      close(1)

      open(unit=2,file='den.out',status='old',position='append')
      write(2,*) den
      close(2)

      open(unit=3,file='gamma_r.out',status='old',position='append')
      write(3,*) gamma_r
      close(3)

      open(unit=4,file='omega_mode.out',status='old',position='append')
      do n=0,n_n-1
        write(4,*) n_tor(n),omega_mode(n)
      enddo
      close(4)

      open(unit=10,file='omega1x_mode.out',status='old',position='append')
      do n=0,n_n-1
        write(10,*) n_tor(n),omega1x_mode(n)
      enddo
      close(10)

      open(unit=5,file='phi_mode.out',status='old',position='append')
      do n=0,n_n-1
        write(5,*) n_tor(n),phi_mode(n)
      enddo
      close(5)

      open(unit=6,file='den_mode.out',status='old',position='append')
      do n=0,n_n-1
        write(6,*) n_tor(n),den_mode(n)
      enddo
      close(6)

      open(unit=9,file='den_mode3D.out',status='old',position='append')
      do n=0,n_n-1
       do p=0,n_r-1
        do k=0,n_theta-1
        write(9,*) den_mode3D(n,p,k)
        enddo
       enddo
      enddo
      close(9)

    endif


  endif

    if(n_step .eq. 0) print *, 'M6D_getdata done n_step=0'
    if(n_step .eq. 1) print *, 'M6D_getdata done n_step=1'


endif



end subroutine M6D_getdata
