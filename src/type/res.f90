module results
   use iso_fortran_env, only : wp => real64

   implicit none

   public :: scf_results

   type :: scf_results
      real(wp) :: e_rep = 0.0_wp
   end type scf_results
end module results