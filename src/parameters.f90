module parameters
    implicit none
    public 
    !> Runtyps
    integer, parameter :: p_run_rhf = 0
    integer, parameter :: p_run_uhf = 1
    integer, parameter :: p_run_mp2 = 2
    integer, parameter :: p_run_opt = 3
    integer, parameter :: p_run_mpop = 4

    !> settings for calculation
    type type_set
        integer :: runtyp = 0
    end type type_set
    type(type_set) :: set

contains

end module parameters