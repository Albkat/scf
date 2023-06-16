!> molecular information wrapper
!   contains information about geometry, atom types,
!   nuclear and total charge, atomic masses

module molecule
    use iso_fortran_env, only : wp => real64
    implicit none
    public :: type_molecule

    private

    type :: type_molecule
        !! molecular structure information

        integer :: n = 0
            !! number of atoms
        
        integer :: uhf = 0
            !! number of unpaired electrons

        integer, allocatable :: at(:)
            !! ordinal numbers
        
        real(wp), allocatable :: xyz(:,:)
            !! cartesian coordinates in Bohr

        real(wp) :: charge = 0.0_wp
    
    end type type_molecule

end module molecule