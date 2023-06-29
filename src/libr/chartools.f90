module chartools
    use iso_fortran_env, only : wp => real64
    implicit none
    private

    public :: to_lowercase, rdarg, rdvar, next_line, read_next_token
    public :: type_token

    integer, parameter :: offset = iachar('A') - iachar('a')
    !> text token
    type type_token
        integer :: first
            !! begin of sequence
        integer :: last
            !! end of sequence
    end type type_token

    interface read_next_token
        module procedure :: read_next_token_int
        module procedure :: read_next_token_real
    end interface
    
    interface read_token
        module procedure :: read_token_int
        module procedure :: read_token_real 
    end interface
contains
function to_lowercase(str) result(lowercase)
    
    character(len=*), intent(in) :: str
        !! input

    character(len=len_trim(str)) :: lowercase
        !! lowercase version of str

    integer :: ilen, iquote, i, iav, iqc

    ilen = len_trim(str)
    iquote = 0
    lowercase = str

    do i = 1, ilen
        iav = iachar(str(i:i))
        if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
            iquote = 1
            iqc = iav
            cycle
        endif
        if (iquote == 1 .and. iav == iqc) then
            iquote = 0
            cycle
        endif
        if (iquote==1) cycle
        if (iav >= iachar('A').and. iav <= iachar('Z')) then
            lowercase(i:i) = achar(iav - offset)
        else
            lowercase(i:i) = str(i:i)
        endif
    end do
end function to_lowercase

subroutine rdarg(i,argument,iostat)
    
    integer, intent(in) :: i 

    character(len=:), allocatable, intent(out) :: argument
        !! raw argument value

    integer, intent(out), optional :: iostat
        !! if error 
    
    integer :: l, err

    if (allocated(argument)) deallocate(argument)
    call get_command_argument(i,length=l, status= err)
    if(err.ne.0) then
        if (present(iostat)) then
            iostat = err
            return
        else
            call raise('E','Command argument corrupted')
        endif
    endif

    allocate( character(len=l) :: argument, stat=err )
    if (err.ne.0) then
        if (present(iostat)) then
            iostat=err
            return
        else
            call raise('E','could not be allocated')
        endif
    endif
    call get_command_argument(i,argument,status=err)
    if (err.ne.0) then
        if (present(iostat)) then
            iostat = err
            return
        else 
            call raise ('E', 'Command argument corrupted')
        endif
    endif
end subroutine rdarg

subroutine rdvar(name, var, iostat)
    character(len=*), intent(in) :: name
        !! name of the ENV var
    character(len=:), allocatable, intent(out) :: var
        !! the raw name of ENV var
    integer, intent(out), optional :: iostat
        !! error

    integer :: l,err
        !! local veriables

    if (allocated(var)) deallocate(var)
    call get_environment_variable(name, length=l, status=err)
    if (err.ne.0) then
        if (present(iostat)) then
            iostat = err
            return
        else
            call raise('E','System variable unassigned')
        endif
    endif
    
    allocate ( character(len=l) :: var, stat=err )
    if (err.ne.0) then
        if (present(iostat)) then
            iostat = err
            return
        else
            call raise('E', 'could not be allocated')
        endif
    endif

    if (l.gt.0) then
        call get_environment_variable(name, var, status=err)
        if (err.ne.0) then
            if (present(iostat)) then
                iostat = err
                return
            else 
                call raise ('E','System variable corrupted')
            endif
        endif
    endif

    if (present(iostat)) iostat = 0
end subroutine rdvar

!> convinience fuction to read a line adn update associated descriptors
subroutine next_line(unit, line, pos, lnum, iostat, iomsg)
    
    integer, intent(in) :: unit
        !! I/O unit

    character(len=:), allocatable, intent(out) :: line
        !! line to read

    integer, intent(out) :: pos
        !! current position in line

    integer, intent(inout) :: lnum
        !! current line number 

    integer, intent(out) :: iostat
        !! status of I/O

    character(len=:), allocatable, optional :: iomsg
        !! error msg

    pos = 0
    call getline(unit,line,iostat,iomsg)
    if(iostat == 0) lnum = lnum + 1
        !! to pass to next line
end subroutine next_line

subroutine getline(unit,line,iostat,iomsg)

    integer,intent(in) :: unit
        !! I/O unit

    character(len=:), allocatable, intent(out) :: line
        !! line to read

    integer, intent(out) :: iostat
        !! status of I/O

    character(len=:), allocatable, optional :: iomsg
        !! error message

    integer, parameter :: strsize = 512
    character(len=strsize) :: msg
    character(len=strsize) :: buff
    integer :: stat
    integer :: size


    allocate(character(len=0) :: line)
        !! empty line
    do
        read(unit, '(a)',advance='no',iostat=stat, iomsg=msg,size=size) &
            & buff 
        if (stat > 0) exit
            !! if any errors -> exit
        line = line // buff(:size)
        if (stat < 0) then
            
            !> if end of records encountered 
            if (is_iostat_eor(stat)) then
                stat = 0
            endif

            exit 
        endif
    enddo 

    if (stat/=0) then
        if (present(iomsg)) iomsg = trim(msg)
    endif
    iostat = stat
            
end subroutine getline

subroutine read_next_token_int(line,pos,token,val,iostat, iomsg)
    
    character(len=*), intent(in) :: line
    integer, intent(inout) :: pos
    type(type_token), intent(inout) :: token
    integer, intent(out) :: val
    integer, intent(out) :: iostat
    character(len=:), allocatable, intent(out), optional :: iomsg

    call next_token(line, pos, token)
        !! get token(postions)
    call read_token(line, token, val, iostat, iomsg)

end subroutine read_next_token_int
subroutine read_next_token_real(line,pos,token,val,iostat, iomsg)
    
    character(len=*), intent(in) :: line
    integer, intent(inout) :: pos
    type(type_token), intent(inout) :: token
    real(wp), intent(out) :: val
    integer, intent(out) :: iostat
    character(len=:), allocatable, intent(out), optional :: iomsg

    call next_token(line, pos, token)
        !! get token(postions)
    call read_token(line, token, val, iostat, iomsg)

end subroutine read_next_token_real

!> advance pointer to next token
subroutine next_token(str,pos,token)

    character(len=*), intent(in) :: str
        !! string to check
    integer, intent(inout) :: pos
        !! current position in str
    type(type_token), intent(out) :: token
        !! tokem found
    
    integer :: start
    
    if (pos >= len(str)) then
        token = type_token(len(str)+1,len(str)+1)
            !! no token if pos bigger
        return
    end if

    do while(pos < len(str))
        pos = pos + 1
            !! move character by character
        select case(str(pos:pos))
        case(' ', achar(9), achar(10), achar(13))
            continue
        !> if something not a space tab, newline, or carriage return encountered -> exit
        case default
            exit 
        end select
    enddo

    start = pos
    do while(pos < len(str))
        pos = pos + 1
        select case(str(pos:pos))
        case(' ', achar(9), achar(10),achar(13))
            pos = pos - 1
            exit    
        case default
            continue
        end select
    enddo

    token = type_token(start,pos)

end subroutine next_token
subroutine read_token_int(line, token, val, iostat, iomsg)

    character(len=*), intent(in) :: line
    type(type_token), intent(in) :: token
    integer, intent(out) :: val
    integer, intent(out) :: iostat
    character(len=:), allocatable, optional, intent(out) :: iomsg

    character(len=512) :: msg

    if (token%first > 0 .and. token%last <= len(line)) then
        read(line(token%first:token%last), *, iostat=iostat, iomsg=msg) val
    else 
        iostat = 1
        msg = 'No input found'
    endif
    
    if (present(iomsg)) iomsg = trim(msg)

end subroutine read_token_int

subroutine read_token_real(line, token, val, iostat, iomsg)

    character(len=*), intent(in) :: line
    type(type_token), intent(in) :: token
    real(wp), intent(out) :: val
    integer, intent(out) :: iostat
    character(len=:), allocatable, optional, intent(out) :: iomsg

    character(len=512) :: msg

    if (token%first > 0 .and. token%last <= len(line)) then
        read(line(token%first:token%last), *, iostat=iostat, iomsg=msg) val
    else 
        iostat = 1
        msg = 'No input found'
    endif

    if (present(iomsg)) iomsg = trim(msg)

end subroutine read_token_real
end module chartools