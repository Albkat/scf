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

    end type type_environment

    type :: type_message

    end type type_message

    interface init
        module procedure :: init_env
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

end subroutine init_env

end module environment