!> molecular information wrapper
!   contains information about geometry, atom types,
!   nuclear and total charge, atomic masses

module molecule
    use iso_fortran_env, only : wp => real64
    use transformation, only : toSymbol, symbol_length
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
        
        real(wp), allocatable :: z(:)
            !! nuclear charges
        
        real(wp), allocatable :: xyz(:,:)
            !! cartesian coordinates in Bohr
        
        character(len=symbol_length), allocatable ::sym(:) 
            !! element symbol

        real(wp) :: charge = 0.0_wp
    contains
        procedure :: allocate => allocate_molecule    
        procedure :: deallocate => deallocate_molecule    
    end type type_molecule

    interface init
        module procedure :: initMoleculeNumbers ! 1 station
        module procedure :: initMolecule ! 2 station
    endinterface init
contains

subroutine initMoleculeNumbers(mol, at, xyz)
    
    type(type_molecule), intent(out) :: mol 
        !! instance of mol structure -> result
    integer, intent(in) :: at(:)
        !! atomic numbers
    real(wp), intent(in) :: xyz(:,:)
        !! cartesian coordinates

    character(len=symbol_length), allocatable :: sym(:)
        !! symbols
    integer :: nAt

    nAt = min(size(at, dim=1), size(xyz, dim=2))
        !! choose the minimal number of elements 
    allocate(sym(nAt))
    sym(:) = toSymbol(at(:nAt))
    
    call init(mol,at,sym,xyz)
        !! got to initMoleculeSymbols

end subroutine initMoleculeNumbers

subroutine initMolecule(mol,at,sym,xyz)

    type(type_molecule), intent(out) :: mol
        !! instance of mol structure
    integer, intent(in) :: at(:)
        !! atomic number
    character(len=*), intent(in) :: sym(:)
        !! element symbols
    real(wp), intent(in) :: xyz(:,:)
        !! cartesian coordinates

    integer :: numAt

    numAt = min(size(at), size(sym), size(xyz, 2))

    call mol%allocate(numAt)


end subroutine initMolecule

subroutine allocate_molecule(self,numAt)

    class(type_molecule), intent(inout) :: self
        !! mol str info
    integer, intent(in) :: numAt
        !! total min number of atoms

    call self%deallocate
    
    self%n = numAt
    allocate( self%at(numAt), source=0 )
    allocate( self%sym(numAt), source='    ' )
    allocate( self%xyz(3,numAt), source=0.0_wp )
    allocate( self%z(numAt), source=0.0_wp )

end subroutine allocate_molecule    

subroutine deallocate_molecule(self)
    
    class(type_molecule), intent(inout) :: self
        !! mol str info

    self%n = 0
    self%charge = 0.0_wp
    if (allocated(self%at))     deallocate( self%at )
    if (allocated(self%z))      deallocate( self%z )
    if (allocated(self%sym))    deallocate( self%sym )
    if (allocated(self%xyz))    deallocate( self%xyz )

end subroutine  deallocate_molecule

end module molecule