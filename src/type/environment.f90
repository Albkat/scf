module environment
    use, intrinsic :: iso_fortran_env, only : stdout => output_unit
    use chartools, only : rdarg, rdvar
    use io, only : type_io, init_ => init
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
        
        character(len=:), allocatable :: hostname
            !! name of hosting machine

        character(len=:), allocatable :: home
            !! name of homedir
        
        character(len=:), allocatable :: path
            !! path variable

        character(len=:), allocatable :: scfhome
            !! scf prog directory
        
        character(len=:), allocatable :: scfpath
            !! scf prog location
        
        logical :: strict
            !! handle warnings as errors

        type(type_io) :: io
            !! input/output handling
    contains
        
        procedure :: error
            !! add an error to the message log

        procedure :: warning
            !! add an warning to the messsage log

      !> forcefully terminate
      procedure :: terminate => terminateRun

      !> print error log
      procedure :: show

        procedure :: check
            !! check status of env for errors

      !> check status of environment
      procedure :: checkpoint
    
   end type type_environment

   !> wrapper for error messages 
   type :: type_message
      logical :: error
      character(len=:), allocatable :: message
   end type type_message

    interface init
        module procedure :: init_env
    end interface 
    
   !> resize log(:)
   interface resize
      module procedure :: resizeMessage
   end interface

   !> log(:) size
   integer, parameter :: initial_size = 20

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
    call rdvar('HOSTNAME', self%hostname, err)
    call rdvar('HOME', self%home, err)
    call rdvar('PATH',self%path, err)
    call rdvar('SCFHOME',self%scfhome,err)
    if (.not.allocated(self%scfhome)) self%scfhome = ''
    if (err /= 0 .or. len(self%scfhome) <= 0) then
        self%scfhome = self%home
    endif
    call rdvar('SCFPATH', self%scfpath,err)
    if (.not.allocated(self%scfpath)) self%scfpath = ''
    if (err /= 0 .or. len(self%scfpath) <= 0 ) then
        self%scfpath = self%scfhome
    endif

    if (present(strict)) then
        self%strict = strict
    else 
        self%strict = .false.
    endif

    call init_(self%io)

end subroutine init_env

!> create and push back a new error to the message log
subroutine error(self, message, source)
    
   !> instance of calculation environment
   class(type_environment), intent(inout) :: self

   !> error message
   character(len=*), intent(in) :: message

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

!> forcefully terminate
subroutine terminateRun(self, message, code)
    
   !> instance of calculation environment
   class(type_environment), intent(inout) :: self

   !> raw text message 
   character(len=*), intent(in) :: message

   !> exit code for termination
   integer, intent(in), optional :: code

   ! explicit interface to global terminate routine !
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

!>  print error log
subroutine show(self, message, isError)

   !> instance of calculation environment
   class(type_environment), intent(inout) ::  self

   !> message text
   character(len=*), intent(in) :: message

   !> if error 
   logical, intent(in), optional :: isError

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

!> check status of environment
subroutine checkpoint(self, message)

   !> instance of calculation environment
   class(type_environment), intent(inout) :: self

   !> error message
   character(len=*), intent(in) :: message

   !> if terminate
   logical :: isTerminate

   !> counter
   integer :: iLog

   ! check for any errors !
   call self%check(isTerminate)

   ! terminate if any !
   if (isTerminate) then
      call self%terminate(message)
   endif

end subroutine checkpoint
end module environment