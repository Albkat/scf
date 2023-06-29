module in
    use basis, only : basisset
    use molecule, only : type_molecule
    use chartools, only : next_line, read_next_token, type_token
    implicit none
    private
    public :: read_in
contains 

subroutine read_in(self, unit, error)

    type(type_molecule), intent(out) :: self 
        !! instance of mol str data
    
    integer, intent(in) :: unit
        !! file handle

    character(len=:), allocatable, intent(out) :: error
        !! error msg

    type(type_token) :: tnat 
    character(len=:), allocatable :: fline
    integer :: pos, lnum, stat
    integer :: n,nel
    
    lnum=0
    call next_line(unit, fline, pos, lnum, stat, error)
        !! retract line from unit without advance, pos = 0
    if (allocated(error)) return
    call read_next_token(fline, pos, tnat, n, stat, error)
        !! number of atoms
    call read_next_token(fline, pos, tnat, nel, stat, error)
        !! number of electrons
    call read_next_token(fline, pos, tnat, nel, stat, error)


end subroutine read_in

end module in