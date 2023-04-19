subroutine lib_init(prog,ntimer,verbose)

    use environment, only : init
    use global
    implicit none
    character(len=*), intent(in) :: prog
    integer, intent(in) :: ntimer
    logical, intent(in) :: verbose
    
    allocate(persistentEnv)

    call init(persistentEnv)

end subroutine