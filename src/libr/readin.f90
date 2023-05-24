module reading
    
    use environment, only : type_environment
    implicit none
    
    !> Return logical
    interface get_value
        module procedure :: get_intvalue
    end interface

contains

function get_intvalue(env,val,dummy) result(stat) 
    
    !> consts
    character(len=*), parameter :: source = 'readin_get_intvalue'

    !> dummy args list
    type(type_environment), intent(inout) :: env
        !! calc env
    character(len=*), intent(in) :: val
        !! input value
    integer, intent(out) :: dummy
        !! output value
    logical :: stat

    !> local vars
    integer :: err
        !! error handling

    read(val,*,iostat=err) dummy
    if (err==0) then
        stat = .true.
    else 
        call env%warning('could not parse "'//val//'"', source)
        stat =.false.
    endif

end function get_intvalue

endmodule reading
