module io
    implicit none
    private

    public :: type_io, init

    type :: type_io
        character(len=:), allocatable :: namespace
            !! namesp for files
        type(type_filehandle), allocatable :: log(:)
            !! files
        integer :: count
            !! counter
    contains
        procedure :: readFile
        procedure :: writeFile
        procedure :: getName
        procedure :: pushBack
        procedure :: closeFile
        procedure :: findUnit
    end type type_io

    type :: type_filehandle
        
        character(len=:), allocatable :: name
        integer :: status
        integer :: unit
        logical :: open

    end type type_filehandle

    type :: type_filestatusenum
        integer :: readin = 1
        integer :: append = 2
        integer :: written = 3
        integer :: replaced = 4
        integer :: deleted = 5
        integer :: created = 6   
    end type type_filestatusenum

    type(type_filestatusenum), parameter :: fileStatus = type_filestatusenum()

    interface init
        module procedure :: init_io
    end interface init
contains
subroutine init_io(self, namespace)
    type(type_io), intent(out) :: self
        !! instance of io handler

    character(len=*), intent(in), optional :: namespace
        !! namespce for the files

    if (present(namespace)) then
        self%namespace = namespace
    endif

    self%count = 0
    allocate(self%log(20)) 

end subroutine init_io

subroutine readFile(self,unitID,fname,stat)
    class(type_io), intent(inout) :: self
    integer, intent(out) :: unitID
    character(len=*), intent(in) :: fname
    integer, intent(out), optional :: stat
    
    !> local vars
    integer :: error
    logical :: exist
    character(len=:), allocatable :: name

    unitID = -1
    error = 0
    
    !>check for namespaces
    call self%getName(fname,name)

    !$omp critical(io)
    inquire(file=name, exist=exist)

    if (exist) then
        open(file=name,newunit=unitID, status='old',action='read',iostat=error)
        if(error == 0) then
            call self%pushBack(type_filehandle(name,fileStatus%readin, unitID, .true.))
        else
            unitID = -1
        endif
    else 
        inquire(file=fname, exist=exist)
        if (exist) then
            open(file=fname, newunit=unitID, status='old', action='read', iostat=error)
            if (error == 0) then
                call self%pushBack(type_filehandle(fname, fileStatus%readin, unitID, .true.))
            else
                unitID = -1
            endif
        else 
            error = 1
        endif
    end if
    !$omp critical(io)

    if (present(stat)) then
        stat = error
    endif

        
end subroutine readFile

subroutine getName(self,fname,name)
    
    class(type_io), intent(inout) :: self
    character(len=*), intent(in) :: fname
    character(len=:), allocatable, intent(out) :: name

    if (allocated(self%namespace)) then
        if (index(fname, '/') == 0) then
            if(fname(1:1) == '.') then
                name = '.'//self%namespace//fname
            else
                name = self%namespace//'.'//fname
            endif
        else
            name = fname
        endif
    else
        name = fname
    end if
    
end subroutine getName

subroutine pushBack(self,filehandle)
    
    class(type_io), intent(inout) :: self
    type(type_filehandle), intent(in) :: filehandle
    
    !> local vars
    type(type_filehandle), allocatable :: tmp(:)
    integer :: n

    self%count = self%count + 1
    if (self%count > size(self%log)) then
        n = size(self%log)
        allocate(tmp(n))
        tmp = self%log
        deallocate(self%log)
        allocate(self%log(n + n/2 +1))
        self%log(1:n) = tmp
        deallocate(tmp)
    endif
    self%log(self%count) = filehandle

end subroutine pushBack

subroutine writeFile(self,unitID,fname,stat)
    
    class(type_io), intent(inout) :: self
    integer, intent(out) :: unitID
    character(len=*), intent(in) :: fname
    integer, intent(out), optional :: stat

    !> local vars
    character(len=:), allocatable :: name
    integer :: error
    logical :: exist

    unitID = -1
    error = 0

    call self%getName(fname,name)
    !$omp critical(io)
    inquire(file=name, exist=exist)

    open(file=name, newunit=unitID, action='write',iostat=error)
    if(error == 0) then
        if (exist) then
            call self%pushBack(type_filehandle(name, fileStatus%replaced, unitID, .true.))
        else 
            call self%pushBack(type_filehandle(name, fileStatus%written, unitID, .true.))
        endif
    else
        unitID = -1
    endif
    !$omp end critical(io)

    if (present(stat)) then
        stat = error
    endif

end subroutine writeFile

subroutine closeFile(self, unitID, iostat, remove)

    !> dummy args list
    class(type_io), intent(inout) :: self
        !! instance of io 
    integer, intent(in) :: unitID
        !! file to close
    integer, intent(out), optional :: iostat
        !! error handling
    logical, intent(in), optional :: remove
        !! if to delete

    !> local vars
    logical :: delete
    integer :: error,pos

    if(present(remove)) then 
        delete = remove
    else 
        delete = .false.
    endif

    error = 0

    call self%findUnit(unitID,pos)

    !$omp critical(io)
    if (pos>0) then
        if (delete) then
            close(unitID, iostat=error, status='delete')
        else
            close(unitID, iostat=error)
        endif
        if (error == 0) then
            self%log(pos)%open = .false.
            if (delete) then
                self%log(pos)%status = fileStatus%deleted
            endif
        endif
    else 
        error = 1
    endif
    !$omp end critical(io)

    if (present(iostat)) then
        iostat = error
    endif

end subroutine closeFile

subroutine findUnit(self,unitID,pos)

    !> dummy args list
    class(type_io), intent(in) :: self
        !! instance of io
    integer, intent(in) :: unitID
        !! file to search
    integer, intent(out) :: pos
        !! position of file in log
    
    !> local vars
    integer :: i

    pos=0

    do i = 1, self%count
        if (self%log(i)%open .and. self%log(i)%unit==unitID) then
            pos = i
            exit
        endif
    enddo

end subroutine findUnit

end module io