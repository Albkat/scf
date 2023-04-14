module environment
    implicit none
    private

    public :: init, type_environment

    type :: type_environment

    end type type_environment

    interface init
        module procedure :: init_env
    end interface 

contains

!> construction of the env
subroutine init_env(self,strict)

    type(type_environment), intent(out) :: self
        !! calc env

    logical, intent(in), optional :: strict
        !! handle warnings as errors

    integer :: err

end subroutine init_env

end module environment