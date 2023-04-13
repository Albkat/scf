module submodules
    implicit none

    private
    public :: submodule, getsubmod

    type :: type_submodule
        integer :: invalid = 0
            !! Unkown run mode
        integer :: scf = 1
            !! main run mode
    end type type_submodule

    type(type_submodule), parameter :: submodule = type_submodule()

contains
!> translate first arg in a submodule enumerator
function getsubmod(arg) result(mod)

    character(len=*), intent(in) :: arg
        !! string identifying the submodule
    integer :: mod
        !! identifier of the module

    select case(arg)
    case default
        mod = submodule%invalid
    case ('scf')
        mod = submodule%scf
    end select
end function getsubmod

end module submodules