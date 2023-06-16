!----------------------------
!> to get default h2 geometry
!----------------------------
subroutine get_h2(mol)
    use iso_fortran_env, only : wp => real64
    use molecule
    implicit none
    type(type_molecule), intent(inout) :: mol 
        !! molecular data wrapper

    integer, parameter :: nat = 2
    integer, parameter :: at(nat) = [1,1]
    real(wp), parameter :: xyz(3,nat) = reshape(&
    &   [ 0.0_wp,  0.0_wp, -0.7_wp,  &
    &     0.0_wp,  0.0_wp,  0.7_wp], &
    &     shape(xyz))
    
    call init(mol, at, xyz)
        !! initialize h2 geometry
end subroutine get_h2