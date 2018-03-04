!------------------------------------------------------------------
! M6D_driver.f90
!
! PURPOSE:
!  driver calls M6D_mainsub
!------------------------------------------------------------------

program M6D_driver

    use MPI

    use M6D_use_grid

  !---------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------

    !MPI
    integer :: myid
!    integer :: numprocs
    integer :: rc

    !MPI initial
    call MPI_INIT( ierr )
    call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
    call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )

    if (myid .eq. 0) then
    print *, '----------------------------------------'
    print *, 'M6D_driver calling m6D_mainsub'
    print *, '----------------------------------------'
    endif



   call M6D_mainsub(myid) 




    call MPI_FINALIZE(rc)

end program M6D_driver
