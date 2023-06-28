module reading
    
    use environment, only : type_environment
    use molecule, only : type_molecule
    implicit none
    
    !> Return logical
    interface get_value
        module procedure :: get_intvalue
    end interface

    abstract interface 
        !> read molecular structure data 
        subroutine structure_reader(self, unit, error)
        
            import :: type_molecule
            type(type_molecule), intent(out) :: self
                !! instance of mol str data
            
            integer, intent(in) :: unit
                !! file handle
            
            character(len=:), allocatable, intent(out) :: error
        end subroutine structure_reader

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

!> generic reader for mol str input  files
subroutine read_molecule(env,mol,unit,ftype)
    character(len=*), parameter :: source = 'readin_read_molecule'
        !! name of error producer

    class(type_environment), intent(inout) :: env
        !! instance of calc env

    class(type_molecule), intent(out) :: mol 
        !! mol str data

    integer, intent(in) :: unit
        !! I/O unit for input file

    integer, intent(in) :: ftype
        !! extension

    type(type_molecule) :: tmp
    integer :: ft 

    procedure(structure_reader), pointer :: reader

    call get_structure_reader(reader,ftype)
    


endsubroutine read_molecule

subroutine

endmodule reading
