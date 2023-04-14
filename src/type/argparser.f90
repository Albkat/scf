!> Cml parser for hf program
module parser
    use, intrinsic :: iso_fortran_env, only : wp=>real64
    use chartools
    implicit none
    private

    public :: type_parser, init

    !> command line argument
    type :: type_arg
        character(len=:), allocatable :: input
            !! raw value
        logical :: unused
            !! if argument processed
        logical :: isFile
            !! if argument is file
        logical :: isFlag
            !! if argument is flag
    end type type_arg

    !> cml parser
    type :: type_parser
        private
        type(type_arg), allocatable :: arg(:)
            !!cml args 

        integer :: flag_pos
            !! position of the flag iterator

        integer :: flag_start
            !! start of cml args
        
        integer :: flag_end
            !! end of cml args
    contains
        procedure, private :: findArg
        procedure, private ::  getArg
        procedure :: nextArg
        procedure :: reset

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
    integer :: nargs
        !! number of arguments
    integer :: idarg
        !! argument position

    nargs = command_argument_count()
        !! count arguments
    
    allocate(self%arg(nargs))

    do idarg=1,nargs
        call init(self%arg(idarg),idarg)
    enddo

    self%flag_start = 1
    
    idarg = self%getArg('--')
    
    !> To assign the end of flags
    if (idarg > 0) then
        self%flag_end = idarg
    else
        self%flag_end = nargs
    endif
    
    self%flag_pos = 0


end subroutine init_parser

!> Initialize arguments
subroutine init_arg(self,arg)
    implicit none
    type(type_arg), intent(out) :: self
        !! instance of the argument
    
    integer, intent(in) :: arg
        !! position
    
    integer :: len_arg

    character(len=*), parameter :: numbers = &
        '0123456789.eE+-'
    character(len=*), parameter :: flagcharts = &
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-'
    
    self%unused = .true.    

    call get_command_argument(arg, length=len_arg)
    allocate(character(len=len_arg) :: self%input) 

    call get_command_argument(arg,value=self%input)
    
    inquire(file=self%input,exist=self%isFile)
    self%isFlag = index(self%input,"-") == 1 &
        & .and. verify(self%input,flagcharts) == 0 &
        & .and. verify(self%input,numbers) > 0

end subroutine

!> Return filename
subroutine nextArg(self,argument)
    
    class(type_parser), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: argument
    integer :: idarg

    idarg = self%flag_pos +1
    if(idarg <= self%flag_end)then
        if(self%arg(idarg)%unused .and. .not.self%arg(idarg)%isFlag) then
            self%flag_pos = idarg
            argument = self%arg(idarg)%input
            self%arg(idarg)%unused = .false.
        endif
    endif

end subroutine nextArg

!> Search for argument, return the position and mark the arg as read
function getArg(self, argument,iS,iE) result(position)
    
    class(type_parser), intent(inout) :: self
        !! instance of the parser

    character(len=*), intent(in) :: argument
        !! cml arge

    integer, intent(in), optional :: iS
        !! start pos
    
    integer, intent(in), optional :: iE
        !! end position
    
    integer :: position
        !! position

    position = self%findArg(argument)
    if (position > 0) then
        self%arg(position)%unused = .false.
    endif

end function getArg


!> Basic search of arg list
function findArg(self,argument, iS, iE) result(position)

    class(type_parser), intent(inout) :: self
        !! instance of the parser

    character(len=*), intent(in) :: argument
        !! arg

    integer, intent(in), optional :: iS
        !! start pos
    
    integer, intent(in), optional :: iE
        !! end pos

    integer :: position
        !! pos of arg

    integer :: idarg, jS, jE

    if (present(iS)) then
        jS= max(1,iS)
    else
        jS = 1
    endif

    if (present(iE)) then
        jE = min(iE,size(self%arg))
    else 
        jE = size(self%arg)
    endif

    position = 0
    
    do idarg = jS, jE
        if (self%arg(idarg)%unused) then
            if (to_lowercase(argument) == to_lowercase(self%arg(idarg)%input)) then
                position = idarg
                exit
            endif
        endif
    enddo
end function findArg

subroutine reset(self)

    class(type_parser), intent(inout) :: self
    
    self%arg%unused = .true.
    self%flag_pos = 0

end subroutine reset
end module parser