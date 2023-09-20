 module setup
   
   use iso_fortran_env, only : wp => real64
   use environment, only : type_environment
   use calculator_, only : type_calculator
   use molecule, only : type_molecule
   use scf_calculator, only : type_scf_calculator, newSCFCalculator
   use parameters
   implicit none
   
   private
   public :: newCalculator
contains

!> calculator allocator
subroutine newCalculator(env,mol,calc,acc)

   !> error prone procedure
   character(len=*), parameter :: source = "setup_newCalculator"

   !> calculation environment
   type(type_environment), intent(inout) :: env

   !> molecular structure data
   type(type_molecule), intent(in) :: mol 
   
   !> polymorphic calculator object
   class(type_calculator), allocatable, intent(out) :: calc

   !> scf accuracy
   real(wp), intent(in) :: acc
   
            !--------!
            ! BUFFER !
            !--------!

   !> buffer calculator for SCF
   type(type_scf_calculator), allocatable :: scf

   !> termination variable
   logical :: exitRun

   select case(set%mode)
   case default
      call env%error("Unknown calculator type", source)
      return

   case(p_ext_eht)

      allocate(scf)
      
      call newSCFCalculator(env, mol, scf, acc)
      
      call env%check(exitRun)
      if (exitRun) then
         call env%error("Could not allocate new calculator",source)
         return
      endif

      call move_alloc(scf,calc)
   
   end select

end subroutine newCalculator

 end module setup