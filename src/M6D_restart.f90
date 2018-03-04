!-----------------------------------------------------
! M6D_restart.f90
!
!--------------------------------------------------------
! PURPOSE:
!
!  read the backup Msup at M6D_initialize, when restart the program (restart=1 or 2).
!   
!---------------------------------------------------------


subroutine M6D_restart(myid)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles  

    use M6D_use_dynamic


   implicit none
    integer,intent(in) :: myid

 if (myid==0) then

  if(restart==1)then  !restart = 1 ; restart from the last backup point
    open(unit=7,file='Msup_odd.backup',status='old',position='rewind')
    read(7,*) time_odd
    read(7,*) n_step_odd
    close(7)

    open(unit=8,file='Msup_even.backup',status='old',position='rewind')
    read(8,*) time_even
    read(8,*) n_step_even
    close(8)

    if(n_step_max .LE. max(n_step_odd,n_step_even)) then
      print *,'You should set restart=0 to restart the program, or',&
              ' set the n_step_max much bigger for restart!!!'
      print *,'New n_step_max must be larger than the last backup point',&
               'of last run when you try to restart from backup point'
      stop
    endif

    if(max(n_step_odd,n_step_even) .eq. 0) then
      print *,'Exit the program! ',&
             'Since the code has never made backup at last run.'
      stop
    elseif(max(n_step_odd,n_step_even) .gt. 0) then
      if(n_step_odd .eq. max(n_step_odd,n_step_even)) then
        open(unit=7,file='Msup_odd.backup',status='old',position='rewind')
        read(7,*) time
        read(7,*) n_step
        read(7,*) Msup
        close(7)
      elseif(n_step_even .eq. max(n_step_odd,n_step_even)) then
        open(unit=8,file='Msup_even.backup',status='old',position='rewind')
        read(8,*) time
        read(8,*) n_step
        read(8,*) Msup
        close(8)
      endif
      n_step_start=n_step+1
      print *,'Restart M6D code from: ', 'time=',time,&
             '  n_step=',n_step_start
    endif

  else if(restart==2)then  !restart = 2 ; restart and roll back for a backup point
    open(unit=7,file='Msup_odd.backup',status='old',position='rewind')
    read(7,*) time_odd
    read(7,*) n_step_odd
    close(7)

    open(unit=8,file='Msup_even.backup',status='old',position='rewind')
    read(8,*) time_even
    read(8,*) n_step_even
    close(8)

    if(n_step_max .LE. min(n_step_odd,n_step_even)) then
      print *,'You should set restart=0 to restart the program, or',&
              ' set the n_step_max much bigger for restart!!!'
      print *,'New n_step_max must be larger than the last backup point',&
               'of last run when you try to restart from backup point'
      stop
    endif

    if(min(n_step_odd,n_step_even) .eq. 0) then
      print *,'Exit the program! ',&
             'Since the code has only made backup once, you should not',&
             'roll back when you restart!'
      stop
    elseif(min(n_step_odd,n_step_even) .gt. 0) then
      if(n_step_odd .eq. min(n_step_odd,n_step_even)) then
        open(unit=7,file='Msup_odd.backup',status='old',position='rewind')
        read(7,*) time
        read(7,*) n_step
        read(7,*) Msup
        close(7)
      elseif(n_step_even .eq. min(n_step_odd,n_step_even)) then
        open(unit=8,file='Msup_even.backup',status='old',position='rewind')
        read(8,*) time
        read(8,*) n_step
        read(8,*) Msup
        close(8)
      endif
      n_step_start=n_step+1
      print *,'Roll back! Restart M6D code from: ', 'time=',time,&
             ' n_step=',n_step_start
    endif

  endif

 endif


  call MPI_BCAST(time,1,MPI_DOUBLE,0,MPI_COMM_WORLD,ierr) !0 mark of the root process

  call MPI_BCAST(n_step,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  call MPI_BCAST(n_step_start,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  call MPI_BCAST(Msup,n_v3*n_n*n_pk,MPI_DOUBLE_COMPLEX,0,MPI_COMM_WORLD,ierr)


  if(myid==0) then
    print *, 'M6D_restart done'
  endif

end subroutine M6D_restart
