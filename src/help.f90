subroutine help(iunit)
    implicit none
    integer, intent(in) :: iunit
    write(iunit,'(a)') &
    "Usage: scf <geometry> [options]", &
    "",&
    "<geometry> is standard *.in format for QC II course", &
    "For full information visit: https://qc2-teaching.readthedocs.io/en/latest/",&
    "",&
    "Options:",&
    "",&
    "--rhf",&
    "   use restricted hartree-fock (default)",&
    "",&
    "--uhf",&
    "   use unrestricted hartree-fock",&
    "",&
    "--mp2",&
    "   use second-order Møller-Plesset",&
    "",&
    "--opt [coord/exp]",&
    "   call optimization (default=coord)",&
    "",&
    "--mpop",&
    "   perform Mulliken population analysis",&
    ""

end subroutine help