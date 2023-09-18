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

subroutine rdvar(inp,out,iost)

   !> input raw value
   character(len=*), intent(in) :: inp

   !> output var
   character(len=:), allocatable :: out
   
   !> error handling
   integer, intent(out), optional :: iost

   !> local buffer
   integer :: err

   !> local buffer
   integer :: l
   
   ! deallocate dummy !
   if (allocated(out)) deallocate(out)

   ! retrieve envvar length !
   call get_environment_variable(inp,length=l,status=err)
   if (err.eq.0 .and. l.gt.0) then
      
      allocate( character(len=l) :: out)
      call get_environment_variable(inp,out,status=err)
      
      if (err.ne.0) then
         if (present(iost)) then
            iost = err
            return
         else 
            call raise('E', 'System variable '//inp//' is corrupted')
         endif
      endif
   
   else 
      if (present(iost)) then
         iost = err
         return
      else 
         call raise('E', 'System variable '//inp//' cannot be read')
      endif
   endif

   ! indicate success !
   if(present(iost)) iost = 0

end subroutine rdvar

endmodule systools