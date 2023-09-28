module scf_repulsion
   use iso_fortran_env, only : wp => real64
   use molecule, only : type_molecule
   use environment, only : type_environment
   use parameters

   implicit none
   public :: repulsion
   private

contains

!> Coloumb repulsion energy; EX 3
subroutine repulsion(env,mol, pr, rep)

   !> calculation environment
   type(type_environment), intent(inout) :: env 
   
   !> molecule structure data
   type(type_molecule), intent(in) :: mol 

   !> printlevel
   integer, intent(in) :: pr

   !> repulsion energy
   real(wp), intent(out) :: rep

   !> buffer variables
   real(wp), allocatable :: energies(:)
   real(wp) :: z ! product of charges
   integer :: i,j

   ! initialization !
   allocate(energies(mol%n))
   rep = 0.0_wp

   ! Jensen, (3.23) !
   if (mol%n > 1) then
      do i=1, mol%n
         do j =i+1, mol%n
            z = mol%z(i) * mol%z(j)
            rep = rep + (z / mol%dist(i,j))   
         enddo
      enddo
   endif 

   if (pr == normal) then
      write(env%unit,'(/,3x,a,5x,f15.3)') "Repulsion energy", rep
   endif
   
end subroutine repulsion

end module scf_repulsion