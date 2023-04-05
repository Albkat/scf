module parser
    use, intrinsic :: iso_fortran_env, only : wp=>real64
    implicit none
    private

    public :: type_parser, init

    !> command line argument
    type :: type_arg
        character(len=:), allocatable :: input
    end type type_arg

    !> cml parser
    type :: type_parser
        private
        type(type_arg), allocatable :: arg(:)
            !!cml args 
    end type type_parser

    interface init
        module procedure :: init_parser
        module procedure :: init_arg
    end interface

contains
subroutine init_parser(self)
    implicit none
    type(type_parser), intent(out) :: self
        !! instance of the parser
    integer :: nargs,idarg
        !! number of arguments

    nargs = command_argument_count()
        !! count arguments
    allocate(self%arg(nargs))

    do idarg=1,nargs
        call init(self%arg(idarg),idarg)
    enddo

    print*,nargs

end subroutine init_parser

subroutine init_arg(self,arg)
    implicit none
    type(type_arg), intent(out) :: self
        !! instance of the argument
    
    integer, intent(in) :: arg
        !! position

    

end subroutine

end module parser