module environment
    use, intrinsic :: iso_fortran_env, only : stdout => output_unit
    use chartools, only : rdarg
    implicit none
    private

    public :: init, type_environment

    type :: type_environment

        integer :: unit
            !! I/O unit

        integer :: number_log
            !! number of logged messages

        type(type_message), allocatable :: log(:)
            !! meassage log

        character(len=:), allocatable :: whoami
            !! executable name

        logical :: strict
            !! handle warnings as errors
    contains
        
        procedure :: error
            !! add an error to the message log

        procedure :: warning
            !! add an warning to the messsage log

        procedure :: terminate => terminateRun
            !! forceful termination

        procedure :: show
            !! show and clear error log

        procedure :: check
            !! check status of env for errors

        procedure :: checkpoint
            !! check ststus of env
    
        end type type_environment

    type :: type_message
        logical :: error
        character(len=:), allocatable :: message
    end type type_message

    interface init
        module procedure :: init_env
    end interface 
    
    !> overloaded resize interface
    interface resize
        module procedure :: resizeMessage
    end interface

    integer, parameter :: initial_size = 20
        !! size of log(:)

contains

!> construction of the env
subroutine init_env(self,strict)

    type(type_environment), intent(out) :: self
        !! calc env

    logical, intent(in), optional :: strict
        !! handle warnings as errors

    integer :: err

    self%unit = stdout
    self%number_log = 0
    allocate(self%log(initial_size))

    call rdarg(0, self%whoami, err)

end subroutine init_env

!> create and push back a new error to the message log
subroutine error(self, message, source)
    
    class(type_environment), intent(inout) :: self
        !! instance of the calc env

    character(len=*), intent(in) :: message
        !! error 

    character(len=*), intent(in), optional :: source

    !> to increase number of logs 
    if (self%number_log >= size(self%log)) then
        call resize(self%log)
    endif

    self%number_log = self%number_log + 1
    if (present(source)) then
        self%log(self%number_log) = type_message(.true., source // ': ' // message)
    else
        self%log(self%number_log) = type_message(.true., message)
    endif

end subroutine error

subroutine warning(self, message, source)

    class(type_environment), intent(inout) :: self
        !! instance of a clac env

    character(len=*), intent(in) :: message
        !! warning

    character(len=*), intent(in), optional :: source
        !! source of the warning

    if (self%number_log >= size(self%log)) then
        call resize(self%log)
    endif

    self%number_log = self%number_log + 1
    if (present(source)) then
        self%log(self%number_log) = type_message(self%strict, source // ": " // message)
    else
        self%log(self%number_log) = type_message(self%strict, message)
    endif

end subroutine warning

subroutine resizeMessage(log,n)

    type(type_message), allocatable, intent(inout) :: log(:)
        !! message list 

    integer, intent(in), optional :: n
        !! desired size

    type(type_message), allocatable :: tmp(:)
        !! buffer list
    integer :: length, current_length
        !! local var

    current_length = size(log)
    if (current_length > 0) then
        if(present(n)) then
            if(n <= current_length) return
            length = n
        else 
            length = current_length + current_length/2 + 1
        endif
        allocate(tmp(length))
        tmp(:current_length) = log(:current_length)
        deallocate(log)
        call move_alloc(tmp,log)
    else 
        if (present(n)) then
            length = n
        else 
            length = 64
        endif
        allocate(log(length))
    endif

end subroutine resizeMessage

subroutine terminateRun(self, message, code)
    
    class(type_environment), intent(inout) :: self
        !! instance of a calc env

    character(len=*), intent(in) :: message
        !! message if error

    integer, intent(in), optional :: code
        !! exit code for termination

    interface
        subroutine terminate(code)
            integer, intent(in) :: code
        end subroutine terminate
    end interface

    call self%error(message)

    call self%show("Program stopped due to fatal error", .true.)

    if (present(code)) then
        call terminate(code)
    else 
        call terminate(1)
    endif

end subroutine terminateRun

!> show the log
subroutine show(self, message, isError)

    class(type_environment), intent(inout) ::  self
        !! instance of a calc env

    character(len=*), intent(in) :: message
        !! message in case of error

    logical, intent(in), optional :: isError
        !! recommendation for terminating run

    integer :: iLog
    logical :: isError0

    if (self%number_log > 0) then
        if (present(isError)) then
            isError0 = isError
        else 
            call self%check(isError0)
        endif
        
        if (isError0) then
            write(self%unit, '(72("#"), /, "[ERROR]", 1x, a)') message
        else
            write(self%unit, '(72("#"), /, "[WARNING]", 1x, a)') message
        endif

        do iLog = self%number_log, 1, -1
            write(self%unit, '("-", i0, "-", 1x, a)') iLog, self%log(iLog)%message
            deallocate(self%log(iLog)%message)
        enddo

        write(self%unit, '(72("#"))')
        self%number_log = 0
    endif
end subroutine show

subroutine check(self, terminate)

    class(type_environment), intent(in) :: self
        !! instance of a calc env

    logical, intent(out) :: terminate
        !! recommendation for terminating run

    terminate = any(self%log(:self%number_log)%error)

end subroutine check

subroutine checkpoint(self, message)

    class(type_environment), intent(inout) :: self
        !! instance of a calc env

    character(len=*), intent(in) :: message
        !! error message

    logical :: isTerminate
        !! if terminate

    integer :: iLog
        !! counter

    !> if any errors
    call self%check(isTerminate)

    if (isTerminate) then
        call self%terminate(message)
    endif

end subroutine checkpoint
end module environment