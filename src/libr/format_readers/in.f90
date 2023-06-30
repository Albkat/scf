module in
    use basis, only : basisset
    use molecule, only : type_molecule
    use chartools, only : next_line, read_next_token, type_token
    use iso_fortran_env,  only : wp => real64
    implicit none
    private
    public :: read_in
contains 

subroutine read_in(self, unit, error)
    character(len=*), parameter :: source = "format_readers/read_in"

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
    integer :: nbf, i
    integer :: rows
    real(wp), allocatable :: xyz(:,:),z(:), x,y,zz

    lnum=0
    call next_line(unit, fline, pos, lnum, stat, error)
        !! retract line from unit without advance, pos = 0
    if (allocated(error)) return
    
    call read_next_token(fline, pos, tnat, n, stat, error)
        !! number of atoms
    if (n < 1) then
        error = "Please check your input, expected positive number of atoms"     
        return
    endif
    
    call read_next_token(fline, pos, tnat, nel, stat, error)
        !! number of electrons
    if (nel < 0) then
        error = "Please check your input, number of electrons is expected to be a positive integer"
        return
    endif
    
    call read_next_token(fline, pos, tnat, nbf, stat, error)
        !! number of Bf2

    if (nbf < 0) then
        error="Please check your input, number of basis functions is expected to be a positive integer"
        return
    endif
    allocate(xyz(3,n))
    allocate(z(n))
    allocate(basisset%zeta(nbf)) 
    allocate(basisset%aoat(nbf)) 

    rows = n+nbf
    do i= 1, n
        call next_line(unit,fline, pos, lnum, stat)
        if (is_iostat_end(stat)) exit 
        if (stat /= 0) then
            error = "could not read geometry from in file"
            return
        endif
        call read_next_token(fline,pos, tnat,x,stat)
        print*,x
    enddo





end subroutine read_in

end module in