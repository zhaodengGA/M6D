!---------------------------------------------------------
! M6D_read_input.f90
!
! PURPOSE:
!  Reads all input parameters from M6D.input
!
!---------------------------------------------------------

subroutine M6D_read_input(myid)

    use MPI
    use M6D_use_grid     !

    implicit none
    integer :: i_read_default
    character(len=80) :: comment
    character(len=79):: section_name
    integer ::error
    integer,intent(in) :: myid

  i_read_default = 0 !1=yes 0=no

    restart =0
    IC_flag=0
    n_data=5
    i_solver=1
    imp=1.0
!    CY=1.0
!    BC=1.0
    
    n_step_max =20
    n_g =1
!    n_r =100
!    n_theta =100
    n_r =4
    n_theta =4
    n_n =3
    n_min  = 0
    n_del  = 5
    n_v1 =1  
    n_v3 =10 ! only 10, 20, 35 moments permitted

    omega_star =100.
    r_hat_a =0.45
    r_hat_b =0.55
    Rmaj0_hat =3.0
    q_mid =2.0
    s_hat =1.0
    aoLn =1.0
    aoLT =3.0
    nu_mid =0.1

    v_max =2.0

    delta =1.0    !1.0 linear  0.0 nonlinear
    del_t = 0.01


  if(i_read_default .eq. 0) open(unit=1,file='M6D.input',status='old')
  !----------------------------------------------------------
  ! Order and variable format  must match here.

  if(i_read_default .eq. 0) then
  section_name='aaa'
  Do While (.Not.(section_name == '##Control_parameter'))
  read(1,"(A19)" ,iostat=error)section_name
  section_name=Trim(section_name)
  End Do
  read(1,"(8x,i2)" ,iostat=error) restart
  read(1,"(7x,i10)" ,iostat=error) n_data
  read(1,"(8x,i2)" ,iostat=error) IC_flag
  read(1,"(9x,i2)" ,iostat=error) i_solver
  read(1,"(4x,f8.3)" ,iostat=error) imp
!  read(1,"(3x,f8.3)" ,iostat=error) CY
!  read(1,"(3x,f8.3)" ,iostat=error) BC

  section_name='aaa'
  Do While (.Not.(section_name == '##Grid_parameter'))
  read(1,"(A16)" ,iostat=error)section_name
  section_name=Trim(section_name)
  End Do
  read(1,"(11x,i12)" ,iostat=error) n_step_max
  read(1,"(6x,f15.8)" ,iostat=error) del_t
  read(1,"(4x,i7)" ,iostat=error) n_g
  read(1,"(4x,i7)" ,iostat=error) n_r
  read(1,"(8x,i7)" ,iostat=error) n_theta
  read(1,"(4x,i7)" ,iostat=error) n_n
  read(1,"(6x,i7)" ,iostat=error) n_min
  read(1,"(6x,i7)" ,iostat=error) n_del
  read(1,"(5x,i8)" ,iostat=error) n_v1
  read(1,"(5x,i8)" ,iostat=error) n_v3

  section_name='aaa'
  Do While (.Not.(section_name == '##Physics_parameter'))
  read(1,"(A19)" ,iostat=error)section_name
  section_name=Trim(section_name)
  End Do
  read(1,"(11x,f15.8)" ,iostat=error) omega_star
  read(1,"(8x,f15.8)" ,iostat=error) r_hat_a
  read(1,"(8x,f15.8)" ,iostat=error) r_hat_b
  read(1,"(10x,f15.8)" ,iostat=error) Rmaj0_hat
  read(1,"(6x,f15.8)" ,iostat=error) q_mid
  read(1,"(6x,f15.8)" ,iostat=error) s_hat
  read(1,"(5x,f15.8)" ,iostat=error) aoLn
  read(1,"(5x,f15.8)" ,iostat=error) aoLT
  read(1,"(7x,f15.8)" ,iostat=error) nu_mid
  read(1,"(6x,f15.8)" ,iostat=error) v_max
  read(1,"(6x,f15.8)" ,iostat=error) delta
  read(1,"(8x,f15.8)" ,iostat=error) delta_i

  section_name='aaa'
  Do While (.Not.(section_name == '##Other_parameter'))
  read(1,"(A17)" ,iostat=error)section_name
  section_name=Trim(section_name)
  End Do
!  read(1,"(6x,f15.8)" ,iostat=error) UPdis
!  read(1,"(8x,f15.8)" ,iostat=error) UPdis_r
!  read(1,"(13x,i2)" ,iostat=error) i_UPdis_diag
!  read(1,"(6x,f15.8)" ,iostat=error) r_Lax
   read(1,"(7x,i2)" ,iostat=error) i_geom

  close(1) 
  endif

  i_geom= 1 !HARDWIRE  read not working?
  BC = 1.   !HARDWIRE   BC=0 0-value radial BC    BC=1 is 0-grad radial BC


!IC_flag  n_pk =1  reset
    if(IC_flag .eq. 1) then
     n_r = 1
     n_theta = 1
     n_g = 0
!     r_hat_b = r_hat_a
    endif
    
  if (myid .eq. 0) then
    print *, 'input transfered'
    print *, 'restart=',restart
    print *, 'n_data=',n_data
    print *, 'IC_flag=',IC_flag
    print *, 'i_solver=',i_solver
    print *, 'imp=',imp
!    print *, 'CY=',CY
!    print *, 'BC=',BC

    print *, 'n_step_max=',n_step_max
    print *, 'del_t=',del_t
    print *, 'n_g=',n_g
    print *, 'n_r=',n_r
    print *, 'n_theta=',n_theta
    print *, 'n_n=',n_n
    print *, 'n_min=',n_min
    print *, 'n_del=',n_del
    print *, 'n_v1=',n_v1
    print *, 'n_v3=',n_v3

    print *, 'omega_star=',omega_star
    print *, 'r_hat_a=',r_hat_a
    print *, 'r_hat_b=',r_hat_b
    print *, 'Rmaj0_hat=',Rmaj0_hat
    print *, 'q_mid=',q_mid
    print *, 's_hat=',s_hat
    print *, 'aoLn=',aoLn
    print *, 'aoLT=',aoLT
    print *, 'nu_mid=',nu_mid
    print *, 'v_max=',v_max
    print *, 'delta=',delta
    print *, 'delta_i=',delta_i

!    print *, 'UPdis=',UPdis
!    print *, 'UPdis_r=',UPdis_r
!    print *, 'i_UPdis_diag=',i_UPdis_diag
!    print *, 'r_Lax=',r_Lax
    print *, 'i_geom=',i_geom
    print *, 'BC=',BC
  endif


    n_pk = n_r*n_theta
    if (myid .eq. 0) then
    print *, 'n_pk=n_r*n_theta=',n_pk
    endif

    if (myid .eq. 0) then
      if (numprocs .gt. n_pk) then
      print *,'!!!'
      print *,'The number of processors should not be larger than n_r*n_theta!'
      print *,''
      stop
      endif
    endif

    if (mod(n_pk,numprocs) .eq. 0) then
    kimax=n_pk/numprocs
    else
    kimax=n_pk/numprocs+1
    endif


!   if (n_n .gt. 1) then
!     if (myid .eq. 0) then
!      print *, 'special q_mid reduction for k_theta_hat spectrun'
!      print *, 'q_mid=2. --->2.0/4.  makes k_theta_hat(4) = 0.4 test case'
!     endif
!    q_mid = q_mid/4.0
!   endif

     if (myid .eq. 0) then
        print *, 'M_read_input done'
     endif

end subroutine M6D_read_input
