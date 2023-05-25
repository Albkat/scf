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
    call scf_version(unitID)

end subroutine scf_header


!----------------------------
!> print version
!----------------------------
subroutine scf_version(unitID)
    
    implicit none
    integer, intent(in) :: unitID
        !! I/O unit

    include 'scf_version.fh'

end subroutine scf_version