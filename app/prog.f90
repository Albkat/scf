program hf
  use parser, only : type_parser,init
  use submodules
  use environment

  implicit none
  type(type_parser) :: parser
    !! Instance of the cml parser
  
  type(type_environment) :: env
    !! Instance of environment
  
  integer :: rMode
    !! runMode of program


  call init(parser)  
    !! Intialize argument parser from cml

  call init(env)
    !! initialiaze environment

  call getRunmode(parser,rMode)
    !! Get rMode (invalid, scf)

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


  if (allocated(argument)) then

    rMode = getsubmod(argument)

    !> In case if it is not a submodule (e.g. str.xyz), we reset parser & use default
    if(rMode == submodule%invalid) then

      call parser%reset
      rMode = submodule%scf

    endif

  endif

end subroutine getRunmode
end program hf