!----------------------------
!> print header
!----------------------------
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

!############IMPLEMENT
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
    "it under the terms of the GBU GPL as published by the ", &
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