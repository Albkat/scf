module parameters
    implicit none
    public 
    !> Runtyps
    integer, parameter :: p_run_rhf = 0
    integer, parameter :: p_run_uhf = 1
    integer, parameter :: p_run_mp2 = 2
    integer, parameter :: p_run_opt = 3
    integer, parameter :: p_run_mpop = 4
    
    !> printlevel
    integer, parameter :: less = -1
    integer, parameter :: normal = 0
    integer, parameter :: verbose = 1



    !> settings for calculation
    type type_set
        integer :: runtyp = 0
        !-------------------------
        !           general
        !-------------------------
        integer :: max_cycles = 100

        !-------------------------
        !           scf
        !-------------------------
        integer :: maxscfiter = 100
    end type type_set
    type(type_set) :: set

contains

end module parameters