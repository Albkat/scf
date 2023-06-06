module time
    implicit none
contains
function print_timestring(mode) result(str)
    implicit none
    !> consts
    character(len=*), parameter :: fmt= &
    & '(a,''/'',a,''/'',a,1x,a'':'',a,'':'',a)'
        !! output format
    
    !> dummy arg list
    character(len=*), intent(in) :: mode
        !! time point 'S', 'E', or other
    character(len=31), intent(out) :: str
        !! time string

    !> local vars
    character(len=8) :: date
    character(len=10) :: time
    character(len=5) :: zone
    
    select case(mode)
    case('S','s')
        write(str,fmt) &
        &   start_date(:4), start_date(5:6), start_date(7:), &
        &   start_time(:2), start_time(3:4), start_time(5:)
    case ('E','e')
        write(str,fmt) &
        &   stop_date(:4), stop_date(5:6), stop_date(7:), &
        &   stop_time(:2), stop_time(3:4), stop_time(5:)
    case default
        call date_time(date,time,zone)
        write(str,fmt) &
        &   date(:4), date(5:6), date(7:), &
        &   time(:2), time(3:4), time(5:)
    endselect

end function print_timestring 
end module time