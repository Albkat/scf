module parameters
    implicit none
    public 
    !> Runtyps
    integer, parameter :: p_run_rhf = 0

    !> settings for calculation
    type type_set
        integer :: runtyp = 0
    end type type_set
    type(type_set) :: set

contains

end module parameters