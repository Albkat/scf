module scf_main
    use environment, only : type_environment
    use setmod
    use cml_parser, only : type_parser
    use systools, only : get_line
    use molecule
    use file
    use reading, only : read_molecule
    use print_
    implicit none
    private
    public :: scfMain

contains

subroutine scfMain(env,args)
   
   implicit none
   character(len=*), parameter :: source = "app_main"

   type(type_environment), intent(inout) :: env
      !! instance of the calc env

   type(type_parser), intent(inout) :: args
      !! cml parser

   !> local var
   integer :: nFiles
      !! number of files provided
   integer :: iFile
      !! file iterator
   logical :: h2
      !! to get h2.in molecule for debugging
   character(len=:), allocatable :: file_name
      !! file name of the mol geo
   character(len=:), allocatable :: dir, base,  ext
      !! for meta data
   character(len=:), allocatable :: dummy 
      !! tmp variable to store string data
   integer :: fileID 
      !! the random unit number for I/O operations
   integer :: err 
      !! unit for error handling
   integer :: ftype
   !> wrapper types to bundle information together
   type(type_molecule) :: mol
      !!  mol str info 

   !-----------------------------------------------------
   !> read command line arguments and configuration files
   !-----------------------------------------------------
   call parse(env,args,h2) 

   nFiles = args%countFiles()
   !> Check how many files, or should default h2.in be used
   select case(nFiles)
   case(0)
      if (.not. h2) then
         call env%error("No input file given, so there is nothing to do", source)
      else
         file_name = 'h2'
      endif
   case(1:)
      do iFile =1 ,nFiles-1
         call args%nextFile(file_name)
         call env%warning("Input file '"//file_name//"' will be ignored", source)
      enddo
      call args%nextFile(file_name)
   end select

   call env%checkpoint("CML parsing failed")

   !> Reading the dot(.) files
   !> .CHRG
   call open_file(fileID,'.CHRG','r')
   
   if (fileID.ne.-1) then
      call get_line(fileID,dummy,stat=err)
      if (err /= 0) then
         call env%error(".CHRG is empty", source)
      else 
         call set_chrg(env,dummy)
         call close_file(fileID)
      endif
   endif

   !> .UHF
   call open_file(fileID,'.UHF','r')

   if (fileID.ne.-1) then
      call get_line(fileID,dummy,stat=err)
      if (err /= 0) then
         call env%error('.UHF is empty')
      else 
         call set_spin(env,dummy)
         call close_file(fileID)
      endif
   endif

   call env%checkpoint("Reading file from file unsuccessful! Please check .CHRG or .UHF")

   !----------------------------------------------------------------
   !> First user interaction: print the banner
   !----------------------------------------------------------------
   call scf_header(env%unit)

   ! to switch responsibility from me onto you !
   call disclaimer(env%unit)

   ! print current time !
   call date(env%unit,'S')
   
   !------------!
   ! READ INPUT ! 
   !------------!
   
   ! default, debug mode !
   if (h2) then 
      call get_h2(mol)
      call generateMeta(file_name, dir, base, ext)
   else 
      call generateMeta(file_name, dir, base, ext)
      ftype=getFiletype(file_name)
      call open_file(fileID, file_name, 'r')
      call read_molecule(env,mol,fileID,ftype)
      call close_file(fileID)
      call env%checkpoint("reading geometry input '"//file_name//"' failed")
   endif

   !----------------------!
   ! CALCULATION SETTINGS !
   !----------------------!
   
   call print_setup(env%unit,mol%n,file_name)
   
   !-------------------------!
   ! CLASSICAL CONTRIBUTIONS !
   !-------------------------!

   !call nuclear_repulsion()
     
end subroutine scfMain


subroutine parse(env, args, h2)
    character(len=*), parameter :: source = "app_main_parse"

    type(type_environment), intent(inout) :: env
        !! instance of calc env

    type(type_parser), intent(inout) :: args
        !! instance of parser

    logical, intent(out) :: h2
        !! debugging with h2

    !> local var
    integer :: nFlags
        !! number of flags
    character(len=:), allocatable :: flag
        !! raw flag 
    
    h2=.false.

    nFlags = args%countFlags()
    call args%nextFlag(flag)

    do while(allocated(flag))
        !> change flags from - to --
        if(len(flag) > 2 .and. flag(1:1) == '-' .and. flag(1:2) /= '--') then
            call env%warning("the use of '"//flag//"' is discouraged, "//&
                & "please use '-"//flag//"' next time", source)
            flag = "-"//flag
        endif
        select case(flag)
        case default
            call env%warning("Unknown option '"//flag//"' provided", source)
        
        !> rhf calculation
        case('--rhf')
            call set_runtyp('rhf')
        
        !> help
        case('--help','-h')
            call help(env%unit)
            call terminate(0)
        !> h2.in as an input
        case("--h2")
            h2 = .true.

        end select
        call args%nextFlag(flag)
    end do

end subroutine parse

endmodule scf_main