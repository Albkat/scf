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

    end type type_io

    type :: type_filehandle
        
        character(len=:), allocatable :: name
        integer :: status
        integer :: unit
        integer :: open

    end type type_filehandle

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
end module io