module chartools
    use gerror
    implicit none
    private

    public :: to_lowercase, rdarg, rdvar

    integer, parameter :: offset = iachar('A') - iachar('a')
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
end module chartools