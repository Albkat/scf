subroutine open_file(unitID,fname,status)
    use global, only : persistentEnv
    implicit none
    integer, intent(out) :: unitID
    character(len=*), intent(in) :: fname
    character(len=1), intent(in) :: status

    select case(status)
    case default
        unitID=-1
    case('R','r')
        call persistentEnv%io%readFile(unitID,fname)
    case('W','w')
        call persistentEnv%io%writeFile(unitID,fname)
    end select

end subroutine open_file

subroutine close_file(unitID)
    
    use global, only : persistentEnv
    implicit none
    integer, intent(in) :: unitID

    call persistentEnv%io%closeFile(unitID)

endsubroutine close_file
!---------------------------------------------------
!> split file name into path, basename and extension
!---------------------------------------------------
subroutine generateMeta(fname, directory, base, ext)

    implicit none
    character(len=*), intent(in) :: fname
        !! file name
    
    character(len=:), allocatable, intent(out) :: directory
        !! path to the file, empty if no path found

    character(len=:), allocatable, intent(out) :: base
        !! base name of file

    character(len=:), allocatable, intent(out) :: ext 
        !! extension of the file; empty if no ext

    integer :: iDot, iSlash 

    iDot = index(fname, '.',back=.true.)
    iSlash = index(fname, '/',back=.true.)

    if (iSlash > 0) then
        !allocate(directory)
       
        directory = fname(:iSlash)
    else
        directory = ''
    endif

    if (iDot > iSlash .and. iDot > 0) then
        ext = fname(iDot+1:)
    else 
        iDot = len(fname) +1
        ext = ''
    endif

    if (iDot > iSlash) then
        base = fname(iSlash+1:iDot-1)
    else
        base = ''
    endif

endsubroutine generateMeta
