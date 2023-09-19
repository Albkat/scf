!> abstract calculator 
module calculator
   use iso_fortran_env, only : wp => real64
   use environment, only : type_environment
   use molecule, only : type_molecule
   use results, only : scf_results
   implicit none

   public :: type_calculator
   private

   !> base calculator
   type, abstract :: type_calculator

      real(wp) :: accuracy

   contains

      !> single-point calculation
      procedure(singlepoint), deferred :: singlepoint

   endtype type_calculator

   abstract interface
      subroutine singlepoint(env, mol, pr, res)
         import :: type_calculator, type_environment, type_molecule
         import :: scf_results
         
         !> calculator instance
         class(type_calculator), intent(inout) :: self

         !> computational environment
         type(type_environment), intent(inout) :: env

         !> molecular structure
         type(type_molecule), intent(inout) :: mol 

         !> print level for IO
         integer, intent(in) :: pr
         
         !> detailed results 
         type(scf_results), intent(out) :: res

      end subroutine singlepoint
   end interface
end module calculator



