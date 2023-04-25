module scf_main
    use environment, only : type_environment
    use parser, only : type_parser
    implicit none
    private
    public :: scfMain

contains

subroutine scfMain(env,parser)
    character(len=*), parameter :: source = "app_main"

    type(type_environment), intent(inout) :: env
        !! instance of the calc env

    type(type_parser), intent(inout) :: parser
        !! cml parser
    !----------------------------------------------
    !> read the commnd line arguments
    !----------------------------------------------
    call parse(env,parser)  

end subroutine scfMain

subroutine parse(env, parse)
    character(len=*), parameter :: source = "app_main_parse"

    type(type_environment), intent(inout) :: env
        !! instance of calc env

    type(type_parser), intent(inout) :: parser
        !! instance of parser

end subroutine parse

endmodule scf_main