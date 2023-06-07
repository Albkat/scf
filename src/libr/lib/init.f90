subroutine lib_init(prog,ntimer,verbose)

    use environment, only : init
    use global
    use time

    implicit none
    character(len=*), intent(in) :: prog
    integer, intent(in) :: ntimer
    logical, intent(in) :: verbose
   
    !> initilize the system time
    call start_timing_run
    call init_timing(ntimer,level=verbose)
    call start_timing(1)


    allocate(persistentEnv)
    call init(persistentEnv)

end subroutine