!> molecular information wrapper
!   contains information about geometry, atom types,
!   nuclear and total charge, atomic masses

module molecule
    use iso_fortran_env, only : wp => real64
    use transformation, only : toSymbol
    implicit none
    public :: type_molecule, init

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

    interface init
        module procedure :: initMoleculeNumbers ! 1 station
        !module procedure :: initMoleculeSymbols ! 2 station
    endinterface init
contains

subroutine initMoleculeNumbers(mol, at, xyz)
    
    type(type_molecule), intent(out) :: mol 
        !! instance of mol structure -> result
    integer, intent(in) :: at(:)
        !! atomic numbers
    real(wp), intent(in) :: xyz(:,:)
        !! cartesian coordinates

    character(len=4), allocatable :: sym(:)
        !! symbols
    integer :: nAt

    nAt = min(size(at, dim=1), size(xyz, dim=2))
        !! choose the minimal number of elements 
    allocate(sym(nAt))
    sym(:) = toSymbol(at(:nAt))

    !call init(mol,at,sym,xyz)

end subroutine initMoleculeNumbers

end module molecule