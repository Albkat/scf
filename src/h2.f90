!----------------------------
!> to get default h2 geometry
!----------------------------
subroutine get_h2(mol)
   use iso_fortran_env, only : wp => real64
   use basis, only : basisset 
   use molecule
   implicit none
   type(type_molecule), intent(inout) :: mol 
      !! molecular data wrapper

   integer, parameter :: nat = 2
   integer, parameter :: nbf = 2
   integer, parameter :: at(nat) = [1,1]
   integer, parameter :: aoat(nat) = [1,2]
   real(wp), parameter :: xyz(3,nat) = reshape(&
   &   [ 0.0_wp,  0.0_wp, -0.7_wp,  &
   &     0.0_wp,  0.0_wp,  0.7_wp], &
   &     shape(xyz))
   real(wp), parameter :: zeta(nbf) = [1.2, 1.2]

   
   call init(mol, at, xyz)
      !! initialize h2 geometry

   allocate(basisset%zeta(nbf))
   allocate(basisset%aoat(nbf))

   basisset%nbf   = nbf
   basisset%zeta  = zeta
   basisset%aoat  = aoat

end subroutine get_h2