module scf_calculator
   use iso_fortran_env, only : wp => real64
   use environment, only : type_environment
   use molecule, only : type_molecule
   use calculator_, only : type_calculator
   use results
   use basis, only : basisset
   use scf_, only : scf

   implicit none

   public :: newSCFCalculator, type_scf_calculator
   private

   !> calculator interface for SCF calculation
   type, extends(type_calculator) :: type_scf_calculator

      !> max number of SCF cycles
      integer :: maxiter 

      !> minimal basis set 


   contains
      
      !> perform SP calculation
      procedure :: singlepoint
   
   end type type_scf_calculator
contains

!> allocate new scf calculator
subroutine newSCFCalculator(env, mol, calc, acc)

   use parameters

   !> error producing procedure
   character(len=*), parameter :: source = "scf__newSCFCalculator"

   !> calculation environment
   type(type_environment), intent(inout) :: env

   !> molecular data str
   type(type_molecule), intent(in) :: mol

   !> polymophic calculator object
   type(type_scf_calculator), intent(out) :: calc 

   !> SCF accuracy
   real(wp), optional, intent(in) :: acc 

   ! SCF accuracy !
   if (present(acc)) then
      calc%accuracy = acc
   else 
      calc%accuracy = 1.0_wp
   endif

   calc%maxiter = set%maxscfiter
   !-----------!
   ! BASIS SET !
   !-----------!

   ! expand Slater function !
   !call basisset%newBasisset()

end subroutine newSCFCalculator

!> SCF SP calculation
subroutine singlepoint(self,env,mol,pr,res)

   !> error producing procedure
   character(len=*), parameter :: source = "scf__singlepoint"

   !> calculator instance 
   class(type_scf_calculator), intent(inout) :: self

   !> calculation environment
   type(type_environment), intent(inout) :: env 
   
   !> molecular data str
   type(type_molecule), intent(inout) :: mol 

   !> print level
   integer, intent(in) :: pr 

   !> detailed results
   type(scf_results), intent(out) :: res

   call mol%calculate_distance

   call scf(env,mol,self%maxiter,pr, self%accuracy,res)

end subroutine singlepoint

end module scf_calculator