module setmod
    use environment, only : type_environment
    use parameters
    use reading, only : get_value
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

subroutine set_chrg(env,chrg)
    
    !> consts
    character(len=*), parameter :: source = 'set_chrg'

    !> dummy args list
    type(type_environment), intent(inout) :: env
        !! calc env
    character(len=*), intent(in) :: chrg
        !! charge
    
    !> local vars 
    integer :: idum
        !! tmp for chrg
    logical, save :: set1 = .true.
        !! to intialize charge only once

    if (set1) then
        if (get_value(env,chrg,idum)) then
            set%chrg = idum
        else 
            call env%error ('Charge is not correctly specified',source)
        endif
    endif
    set1 = .false. 

endsubroutine set_chrg

end module setmod