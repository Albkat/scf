subroutine raise(mode,mess)
    use global, only : strict, persistentEnv
    implicit none
    character, intent(in) :: mode 
    character(len=*), intent(in) :: mess

    select case(mode)
    case('S','s')
        !! save to message buffer
        if(strict) then
            call persistentEnv%error(mess)
            call persistentEnv%terminate("Global environment terminated")
        else 
            call persistentEnv%warning(mess)
        endif
    case('F','f')
        !! flush message buffer
        call persistentEnv%checkpoint(mess)
    case ('W','w')
        !! print warning directly
        if(strict) then
            call persistentEnv%error(mess)
            call persistentEnv%terminate("Global environment terminated")
        else
            call persistentEnv%warning(mess)
        endif
    case ('E','e')
        call persistentEnv%error(mess)
        call persistentEnv%terminate('Global environment terminated')
    end select
    
end subroutine raise

subroutine terminate(sig)
    use iso_fortran_env, only : stderr => error_unit
    use global, only : name
    implicit none
    integer, intent(in) :: sig

    integer, parameter :: par_exit_success = 0
    integer, parameter :: par_exit_failure = 1
    integer, parameter :: par_exit_external = -1

    if (.not.allocated(name)) name = 'program'
    select case(sig)
    case(par_exit_success)
        write(stderr, '("normal termination of",1x,a)') name 
        stop
    case(par_exit_external)
        write(stderr, '("external termnation of",1x,a)') name
        error stop 
    case(par_exit_failure)
        write(stderr, '("abnormal termination of",1x,a)') name
        error stop 
    end select

end subroutine terminate