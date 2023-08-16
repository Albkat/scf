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
      
      integer :: nel = 0
         !! number of electrons 

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

      integer :: chrg
         !! overall charge

      real(wp),allocatable :: dist(:,:)
         !! interatomic distances

      integer :: ftype
         !! file type

   contains
      procedure :: allocate => allocate_molecule    
      procedure :: deallocate => deallocate_molecule    
      procedure :: set_nuclear_charges 
      procedure :: calculate_distance
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
        !! go to initMoleculeSymbols

end subroutine initMoleculeNumbers

subroutine initMolecule(mol,at,sym,xyz)

    use parameters

    type(type_molecule), intent(out) :: mol
        !! instance of mol structure
    integer, intent(in) :: at(:)
        !! atomic number
    character(len=*), intent(in) :: sym(:)
        !! element symbols
    real(wp), intent(in) :: xyz(:,:)
        !! cartesian coordinates

    integer :: numAt, i

    numAt = min(size(at), size(sym), size(xyz, 2))

    call mol%allocate(numAt)
    
    mol%at(:)=at(:numAt)
    mol%sym(:)=sym(:numAt)
    mol%xyz(:,:)=xyz(:,:numAt)

    mol%chrg=set%chrg
    mol%uhf=set%uhf
    
    call mol%set_nuclear_charges
    call mol%calculate_distance

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
    allocate( self%dist(numAt,numAt), source=0.0_wp)

end subroutine allocate_molecule    

subroutine deallocate_molecule(self)
    
    class(type_molecule), intent(inout) :: self
        !! mol str info

    self%n = 0
    self%chrg = 0
    if (allocated(self%at))     deallocate( self%at )
    if (allocated(self%z))      deallocate( self%z )
    if (allocated(self%sym))    deallocate( self%sym )
    if (allocated(self%xyz))    deallocate( self%xyz )
    if (allocated(self%dist))   deallocate( self%dist )

end subroutine  deallocate_molecule

subroutine set_nuclear_charges(self)
    class(type_molecule), intent(inout) :: self
        !! mol str information
    integer :: i
    do i = 1, self%n
        self%z(i) = real(self%at(i) - real(ncore(self%at(i))))
        !if (self%at(i) > 57 .and. self%at(i) < 72) self%z(i) = 3.0_wp
    enddo
contains
elemental integer function ncore(at)
    integer, intent(in) :: at
    if (at.le.2) then
        ncore=0
    elseif(ncore.le.10) then
        ncore=2
    elseif(ncore.le.18) then
        ncore=10
    elseif(ncore.le.29) then
        ncore=18
    elseif(ncore.le.36) then
        ncore=28
    elseif(ncore.le.47) then
        ncore=36
    elseif(ncore.le.54) then
        ncore=46
    elseif(ncore.le.71) then
        ncore=54
    elseif(ncore.le.79) then
        ncore=68
    elseif(ncore.le.86) then
        ncore=78
    endif
end function ncore
end subroutine set_nuclear_charges

subroutine calculate_distance(self)


    class(type_molecule),intent(inout) :: self
        !! mol str info
    integer :: i,j

    do i=1, self%n
        do j=1, i-1
            self%dist(j,i) = sqrt(sum((self%xyz(:,j)-self%xyz(:,i))**2))
            self%dist(i,j) = self%dist(j,i)
        enddo
        self%dist(i,i) = 0.0_wp
    enddo

end subroutine calculate_distance

end module molecule