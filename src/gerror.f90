subroutine raise(mode,mess)
    implicit none
    character, intent(in) :: mode 
    character(len=*), intent(in) :: mess

    select case(mode)
    case('S','s')
        !! save to message buffer
        if(strict) then
            call persistentEnv%error(mess)
        else 
            call persistentEnv%warning(mess)
        endif
    end select
    
end subroutine raise