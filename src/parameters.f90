module parameters
   
   use iso_fortran_env, only : wp => real64
   
   implicit none
   public 
   
   ! drivers !
   integer, parameter :: p_ext_eht = 0

   ! runtyps !
   integer, parameter :: p_run_rhf = 0
   integer, parameter :: p_run_uhf = 1
   integer, parameter :: p_run_mp2 = 2
   integer, parameter :: p_run_opt = 3
   integer, parameter :: p_run_mpop = 4
   
   ! printlevel !
   integer, parameter :: less = -1
   integer, parameter :: normal = 0
   integer, parameter :: verbose = 1



   !> settings for calculation
   type type_set
      
      ! RUNTYPES !
      integer :: runtyp = p_run_rhf
      integer :: mode = p_ext_eht
      
      ! GENERAL !
      integer :: chrg = 0 
      integer :: uhf = 0
      integer :: nalphabeta = 0
      integer :: printlevel = normal
      real(wp) :: accuracy = 1.0_wp

      ! SCF !
      integer :: maxscfiter = 100
      
      ! OPTIMIZAION !
      integer :: max_cycles = 100

   end type type_set
   type(type_set) :: set

contains

end module parameters