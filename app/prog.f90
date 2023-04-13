program hf
  use parser, only : type_parser,init
  use submodules
  implicit none
  type(type_parser) :: parser
    !! Instance of the cml parser
  integer :: rMode
    !! runMode of program


  call init(parser)  
    !! Intialize argument parser from cml

  call getRunmode(parser,rMode)

  print *, "Terminated normally"
contains
subroutine getRunmode(parser,rMode)
  
  type(type_parser), intent(inout) :: parser
    !! cml parser

  integer, intent(out) :: rMode
    !! run mode of the program

  character(len=:), allocatable :: argument
    !! first cml arg

  rMode = submodule%scf

  call parser%nextArg(argument)

end subroutine getRunmode
end program hf