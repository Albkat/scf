module file

    implicit none
    private
    public :: open_file, close_file, generateMeta, getFiletype, filetype
    type :: filetypes

        integer :: unknown = 0
            !! unknown type

        integer :: in = 1
            !! QC2 file in
    
    end type filetypes

    type(filetypes), parameter :: filetype = filetypes()
        !! create instance of filetypes 
contains
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

function getFiletype(str) result(ftype)
    use chartools, only : to_lowercase

    character(len=*), intent(in) :: str
        !! input file

    integer :: ftype
        !! file type
     
    integer :: iDot, iSlash 
        !! postion of dot and slash
    
    ftype = filetype%unknown
        !! default 
    iDot    = index(str,'.',back=.true.)
        !! find dot
    iSlash  = scan(str,'\/',back=.true.)
        !! find \/

    if (iDot > iSlash .and. iDot>0) then
        !! if the fot comes after slash
        select case(to_lowercase(str(iDot+1:)))
        case('in')
            ftype = filetype%in
        endselect
        if (ftype /= filetype%unknown) return
            !! end here if extension is present
    else 
        idot= len(str)+1
            !! for h2
    endif


end function getFiletype
end module file