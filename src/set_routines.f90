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

!-----------------------------
!> unrestricted case
!-----------------------------
subroutine set_spin(env,val)
    
    !> consts
    character(len=*), parameter :: source = "set_spin"

    !> dummy args list
    type(type_environment), intent(inout) :: env
        !! calc env
    character(len=*), intent(in) :: val
        !! raw value of the str

    !> local vars
    logical, save :: set1 = .true.
        !! to remember the first assighned value
    integer :: idum
        !! tmp for integer

    if (set1) then
        if (get_value(env,val,idum)) then
            set%nalphabeta = idum
        else 
            call env%error('Spin could not be read from the .UHF file', source)
        endif
    endif

    set1=.false.

end subroutine set_spin

end module setmod