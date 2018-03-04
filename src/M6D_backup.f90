!-----------------------------------------------------
! M6D_backup.f90
!
!--------------------------------------------------------
! PURPOSE:
!
!  backup the data at each output step for restart the program.
!   
!---------------------------------------------------------


subroutine M6D_backup(myid)

    use MPI

    use M6D_use_grid     

    use M6D_use_profiles  

    use M6D_use_dynamic


   implicit none
    integer,intent(in) :: myid

 if (myid==0) then

    open(unit=7,file='Msup_odd.backup',status='old',position='rewind')
    read(7,*) time_odd
    close(7)

    open(unit=8,file='Msup_even.backup',status='old',position='rewind')
    read(8,*) time_even
    close(8)


  if(time_odd .eq. min(time_odd,time_even)) then
    open(unit=7,file='Msup_odd.backup',status='replace')
    write(7,*) time
    write(7,*) n_step
    write(7,*) Msup
    close(7)
    time_odd=time
    n_step_odd=n_step
  elseif(time_even .eq. min(time_odd,time_even)) then
    open(unit=8,file='Msup_even.backup',status='replace')
    write(8,*) time
    write(8,*) n_step
    write(8,*) Msup
    close(8)
    time_even=time
    n_step_even=n_step
  endif

 endif

  if(myid==0) then
    print *, 'M6D_backup done','   n_step=',n_step,' time=',time
  endif

end subroutine M6D_backup
