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


end subroutine scfMain
  
endmodule scf_main