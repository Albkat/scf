module scf_
   use iso_fortran_env, only : wp => real64
   use environment, only : type_environment
   use molecule, only : type_molecule
   use results, only : scf_results
   use parameters
   use basis, only : basisset
   implicit none
   public :: scf
   private
contains
subroutine scf(env,mol,iter,pr,acc,res)

   !> error producing procedure
   character(len=*), parameter :: source = "scf_scf"

   !> calculation environment
   type(type_environment), intent(inout) :: env 
   
   !> molecular data str
   type(type_molecule), intent(in) :: mol 

   !> max iteration number
   integer, intent(in) :: iter 
   
   !> print level
   integer, intent(in) :: pr 
   
   !> accuracy level level
   real(wp), intent(in) :: acc

   !> detailed results
   type(scf_results), intent(out) :: res

   character(len=*),parameter :: intfmt = &
      '(3x,":",2x,a,i18,4x,":")'
   
   character(len=*),parameter :: dblfmt = &
      '(3x,":",2x,a,f18.3,4x,":")'

   if (pr .eq. normal) then
      write(env%unit,'(/,3x,45("."))')
      write(env%unit,'(3x,":",19x,a,19x,":")') "SETUP"
      write(env%unit,'(3x,":",43("."), ":")')
      write(env%unit,intfmt) "# basis functions  ",basisset%nbf
      write(env%unit,intfmt) "# primit. orbitals ",basisset%nprim
      write(env%unit,intfmt) "# iterations       ",iter
      write(env%unit,dblfmt) "accuracy           ",acc
      !! ADD SCF convergence 
      write(env%unit,'(3x,45("."))')
   endif

   !------------------!
   ! REPULSION ENERGY !
   !------------------!

end subroutine scf
end module scf_