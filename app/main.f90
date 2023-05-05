module scf_main
    use environment, only : type_environment
    use setmod
    use cml_parser, only : type_parser
    implicit none
    private
    public :: scfMain

contains

subroutine scfMain(env,args)
    character(len=*), parameter :: source = "app_main"

    type(type_environment), intent(inout) :: env
        !! instance of the calc env

    type(type_parser), intent(inout) :: args
        !! cml parser
    !----------------------------------------------
    !> read the commnd line arguments
    !----------------------------------------------
    call parse(env,args)  
end subroutine scfMain

subroutine parse(env, args)
    character(len=*), parameter :: source = "app_main_parse"

    type(type_environment), intent(inout) :: env
        !! instance of calc env

    type(type_parser), intent(inout) :: args
        !! instance of parser

    integer :: nFlags
        !! number of flags
    character(len=:), allocatable :: flag
        !! raw flag 
    
    nFlags = args%countFlags()
    call args%nextFlag(flag)

    do while(allocated(flag))
        !> change flags from - to --
        if(len(flag) > 2 .and. flag(1:1) == '-' .and. flag(1:2) /= '--') then
            call env%warning("the use of '"//flag//"' is discouraged, "//&
                & "please use '-"//flag//"' next time", source)
            flag = "-"//flag
        endif
        select case(flag)
        case default
            call env%warning("Unknown option '"//flag//"' provided", source)
        case('--rhf')
            call set_runtyp('rhf')

        end select
        call args%nextFlag(flag)
    end do

end subroutine parse

endmodule scf_main