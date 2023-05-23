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