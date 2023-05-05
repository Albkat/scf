module setmod
    use environment, only : type_environment
    use parameters
    implicit none
contains
subroutine set_runtyp(typ)
    implicit none
    character(len=*), intent(in) :: typ
    logical, save :: set1=.true.
    if (.not.set1) then
        call raise('S','Runtype already set and locked, please use a composite runtype instead')
        return
    endif
    select case(typ)
    case default 
        !! if something unexpected internally happens
        call raise('E', typ//" is not valid runtyp (internal error)")
    case('rhf')
        set%runtyp = p_run_rhf
    end select
end subroutine set_runtyp
end module setmod