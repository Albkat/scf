module basis
   use iso_fortran_env, only : wp => real64
   implicit none
   private
   public :: basisset, newBasisset

   !> basis set information(STO-6G)
   type type_basisset

      integer :: nbf = 0
         !! number of basis functions
      integer :: nprim = 6
         !! STO-6G
      real(wp), allocatable :: zeta(:)
         !! slater exponents 
      integer, allocatable :: aoat(:)
         !! BF -> atom
   contains

      procedure :: newBasisset

   end type type_basisset

   type(type_basisset) :: basisset

contains

subroutine newBasisset(self)

!> minimal basis set
class(type_basisset), intent(inout) :: self

end subroutine newBasisset

end module basis