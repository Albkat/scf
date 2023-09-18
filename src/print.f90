module print_
   implicit none

contains

!> print header
subroutine scf_header(unitID)
    
    implicit none
    integer, intent(in) :: unitID
        !! I/O unit

    write(unitID, '(a)') &
    "   --------------------------------------------   ", &
    "   |               =============               |  ", &
    "   |                   S C F                   |  ", &
    "   |               =============               |  ", &
    "   |                A.Katbashev                |  ", &
    "   |                   QC II                   |  ", &
    "   --------------------------------------------   "
    !call scf_version(unitID)

end subroutine scf_header

!IMPLEMENT!
!----------------------------
!> print version 
!----------------------------
!subroutine scf_version(unitID)
    
!    implicit none
!    integer, intent(in) :: unitID
        !! I/O unit

!    include 'scf_version.fh'

!end subroutine scf_version

subroutine disclaimer(unitID)

    implicit none
    integer, intent(in) :: unitID
        !! I/O unit

    write(unitID,'(a)') &
    "scf is free software: you can modify and redistribute ", &
    "it under the terms of the GNU GPL as published by the ", &
    "Free Software Foundation either version 3, or any later.", &
    ""
end subroutine disclaimer

subroutine date(unitID,mode)

    use time, only : print_timestring
    implicit none
    !> dummy arg list
    integer, intent(in) :: unitID
        !! I/O unit
    character, intent(in) :: mode 
        !! start/current/end

    !> local vars
    character(len=*), parameter :: outfmt='('' * '',a,1x,a)'

    
    selectcase(mode)
    case('S','s')
        write(unitID,outfmt) 'Launch Time', print_timestring(mode)
    case('E','e')
        write(unitID,outfmt) 'Termination Time', print_timestring(mode)
    case default
        write(unitID,outfmt) 'Current Time', print_timestring(mode)
    endselect

end subroutine date

subroutine print_setup(iunit,n,fname)

   use systools, only : rdvar

   !> I/O unit
   integer, intent(in) :: iunit

   !> number of atoms
   integer, intent(in) :: n

   !> coordinate file
   character(len=*), intent(in) :: fname

   !> dummy raw string 
   character(len=:), allocatable :: rawstr

   !> length of the CLI command
   integer :: l
   
   !> error handling
   integer :: err

   ! header !
   write(iunit,'(a)')
   call generic_header(iunit,'Calculation Settings', 43, 3)

   ! program call !
   if (allocated(rawstr)) deallocate(rawstr)
   call get_command(length=l)
   allocate( character(len=l) :: rawstr )
   call get_command(rawstr)
   write(iunit, '(3x,a,"::",1x,a)')    'program call    ', rawstr

   ! hostname !
   call rdvar('HOSTNAME',rawstr,err)
   if (err.eq.0) &
      write(iunit,'(3x,a,"::",1x,a)')  'hostname        ', rawstr

   ! coordinate file name !
   write(iunit,'(3x,a,"::",1x,a)')     'coordinate file ', fname
   

end subroutine print_setup

subroutine generic_header(iunit,string,width,offset)

   !> I/O unit
   integer, intent(in) :: iunit

   !> header text
   character(len=*), intent(in) :: string

   !> total header width
   integer, intent(in) :: width

   !> indentation
   integer, intent(in) :: offset
   
   !> length of raw text
   integer :: strlen

   !> no. characters to place before and after header text
   integer :: ifront, iback

   !> header formatted string
   character(len=width) :: strformat
   character(len=2*width) :: outstr

   !> dummys 
   character(len=width) :: idum1
   character(len=width) :: idum2

   strlen = len(string)
   ifront = (width - strlen) / 2
   iback  = width - ifront - strlen 

   write(idum1,*) width
   write(idum2,*) offset

   write(strformat, '(i0,"x,a,", i0,"x")') ifront, iback 
   write(outstr,'("|",'//strformat//',"|")') string
   
   write(iunit, '('//idum2//'x,1x,'//idum1//'("-"),1x)')
   write(iunit, '('//idum2//'x,a)')outstr 
   write(iunit, '('//idum2//'x,1x,'//idum1//'("-"),1x)')

end subroutine generic_header

endmodule print_