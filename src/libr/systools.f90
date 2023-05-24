module systools
    use iso_fortran_env, only : iostat_eor
    implicit none
contains

!> read line
subroutine get_line(unitID,text,stat)
    !> dummy args list
    integer, intent(in) :: unitID
    character(len=:), allocatable, intent(out) :: text
    integer, intent(out), optional :: stat

    !> local vars
    integer :: err
        !! error handling
    integer :: size
        !! size of the line
    integer, parameter :: length = 144
        !! max length of one line
    character(len=length) :: buffer
        !! tmp var for storing the str data
    
    text=''
    do 
        read(unitID,'(a)',advance='no',iostat=err,size=size) buffer
        if (err.gt.0) then
            if (present(stat)) stat = err
            return
        endif
        text = text//buffer(:size)
        if(err.lt.0) then
            if (err==iostat_eor) err = 0
            if (present(stat)) stat = err
            return
        endif
    end do
    

endsubroutine get_line
endmodule systools