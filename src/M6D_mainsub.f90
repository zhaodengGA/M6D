!---------------------------------------------------------
! M6D_mainsub.f90
!
!
!--------------------------------------------------------
! PURPOSE:

! test code for exploring GRBF 6D  kinetics  moments with ions and 
!   adiabatic electrons
!   
!---------------------------------------------------------


subroutine M6D_mainsub(myid)

    use MPI

    use M6D_use_grid

    implicit none

    integer,intent(in) :: myid
     


    call M6D_read_input(myid)

    call M6D_allocate_big(myid)

    call M6D_setup_grids(myid)

    call M6D_setup_profiles(myid) 
    
    call M6D_setup_divgeo(myid)

    call M6D_setup_advancematrix(myid)

!!!!    if (myid .eq. 0)  call M6D_dispersion_solver(myid)

!  if(i_kxkykz .eq. 1 .and. myid .eq. 0) call M6D_local_ICmode(myid)

!   if(myid .eq. 0) print *, 'STOP after M6D_local_ICmode'
!   stop 

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    call M6D_initialize(myid)    !time=0.0   n_step=0  *.out files openned


   if (myid .eq. 0) then
    print *, '----------------------------------------------------------'
    print *, 'HARDWIRE n_data=',n_data,'  n_step_max =',n_step_max
    print *, 'i_solver=',i_solver
   endif



 if(i_solver .eq. 1) then
   if (myid .eq. 0) then
   print *, '----------------------------------------------------------'
   print *, 'start explicit time stepping'
   endif


  if(n_n .lt. 2) then  ! only for testing linear time stepping


   do n_step = n_step_start,n_step_max



    call M6D_explicitadvance(myid)



    if(modulo(n_step,n_data) .eq. 0) then
             call M6D_backup(myid)   
             call M6D_getdata(myid)
!NOTready    call M6D_renorm(myid)
    endif

   enddo  !n_step
  endif ! n_n < 2 test
 endif !i_solver = 1

    call M6D_deallocate_big(myid)


   
    if (myid .eq. 0) then
    print *, 'M6D_mainsub done'
    endif

    stop


end subroutine M6D_mainsub
