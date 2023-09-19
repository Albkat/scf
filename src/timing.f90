module time
    use, intrinsic :: iso_fortran_env, only : int64 , wp => real64
    implicit none
    intrinsic :: date_and_time

    public :: print_timestring, start_timing_run, start_timing
    public :: init_timing, stop_timing
    private :: verbose

    character(len=8) :: start_date, stop_date
    character(len=10) :: start_time, stop_time
    character(len=5) :: start_zone, stop_zone
    integer(int64) :: start_values(8), stop_values(8)
    
    real(wp), allocatable :: timing_wall(:)
    real(wp), allocatable :: timing_cpu(:)
    logical :: verbose = .false.
    integer :: timing_max

contains

!------------------------------------------
!> get launch time
!------------------------------------------
subroutine start_timing_run
    call date_and_time(start_date,start_time, start_zone, start_values)
end subroutine start_timing_run

!------------------------------------------
!> allocate timer
!------------------------------------------
subroutine init_timing(ntimer, level)
    
    implicit none
    integer, intent(in) :: ntimer
    logical, intent(in), optional :: level
    
    if (allocated(timing_wall)) deallocate(timing_wall)
    if (allocated(timing_cpu)) deallocate(timing_cpu)
    if (present(level)) verbose = level

    timing_max = ntimer
    allocate(timing_wall(ntimer),   &
    &       timing_cpu(ntimer),     &
    &       source=0.0_wp)

end subroutine init_timing

!> start timing for time diff
subroutine start_timing(element)

    implicit none
    integer, intent(in) :: element
    real(wp) :: time_cpu
    real(wp) :: time_wall
    call timing(time_cpu, time_wall)
    timing_wall(element) = timing_wall(element) - time_wall
    timing_cpu(element) = timing_cpu(element) - time_cpu

end subroutine start_timing

!> stop timer
subroutine stop_timing(element)

   implicit none
   integer, intent(in) :: element
   real(wp) :: time_cpu
   real(wp) :: time_wall
   call timing(time_cpu,time_wall)
   timing_cpu(element) = timing_cpu(element) + time_cpu
   timing_wall(element) = timing_wall(element) + time_wall

end subroutine

!------------------------------------------
!> print the timing as a string
!------------------------------------------
function print_timestring(mode) result(str)
    implicit none
    !> consts
    character(len=*), parameter :: fmt= &
    & '(a,''/'',a,''/'',a,1x,''at'',1x,a,'':'',a,'':'',a)'
        !! output format
    
    !> dummy arg list
    character(len=*), intent(in) :: mode
        !! time point 'S', 'E', or other
    character(len=31):: str
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
        call date_and_time(date,time,zone)
        write(str,fmt) &
        &   date(:4), date(5:6), date(7:), &
        &   time(:2), time(3:4), time(5:)
    endselect

end function print_timestring 

subroutine timing(cpu,wall)

    implicit none
    real(wp), intent(out) :: cpu
    real(wp), intent(out) :: wall
    integer(int64) :: time_count, time_rate, time_max

    call system_clock(time_count,time_rate, time_max)
    call cpu_time(cpu)

    wall = real(time_count,wp) / real(time_rate,wp)

end subroutine timing

end module time