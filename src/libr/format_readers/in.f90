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
   integer :: n, nel
   integer :: nbf, i, atom_nbf, j 
   integer :: rows
   real(wp),allocatable :: xyz(:,:), charge(:)
   real(wp) :: x, y, z

   lnum=0

   ! retract line from unit without advance, pos = 0 !
   call next_line(unit, fline, pos, lnum, stat, error)
   if (allocated(error)) return
   
   ! number of atoms !
   call read_next_token(fline, pos, tnat, n, stat, error)
   if (allocated(error)) then
      return
   else if (n < 1) then
      error = "Please check your input, expected positive number of atoms"     
      return
   endif
   
   ! number of electrons !
   call read_next_token(fline, pos, tnat, nel, stat, error)
   if (allocated(error)) then
      return
   else if (nel < 0) then
      error = "Please check your input, number of electrons is expected to be a positive integer"
      return
   endif
   
   ! number of Bf2 !
   call read_next_token(fline, pos, tnat, nbf, stat, error)
   if (allocated(error)) then
      return
   else if (nbf < 0) then
      error="Please check your input, number of basis functions is expected to be a positive integer"
      return
   endif

   
   allocate(xyz(3,n))
   allocate(charge(n))
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

      call read_next_token(fline,pos,tnat,x,stat,error)
      if (allocated(error)) return

      
      call read_next_token(fline,pos,tnat,y,stat,error)
      if (allocated(error)) return
      
      call read_next_token(fline,pos,tnat,z,stat,error)
      if (allocated(error)) return

      call read_next_token(fline,pos,tnat,charge(i),stat,error)
      if (allocated(error)) return

      call read_next_token(fline,pos,tnat,atom_nbf,stat,error)
      if (allocated(error)) return
      
      if (atom_nbf.ge.0) then
         do j=1,atom_nbf
            call next_line(unit,fline,pos,lnum,stat)
         enddo
      else
         error = "The number of basis functions for atom cannot be negative"
         return
      endif
      print*,atom_nbf
      stop
   enddo





end subroutine read_in

end module in