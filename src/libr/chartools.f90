module chartools
    implicit none
    private

    public :: to_lowercase

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
end module chartools