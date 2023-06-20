module transformation
    implicit none
    public :: toSymbol, symbol_length
    private

    integer, parameter :: symbol_length = 4
        !! maximum allowed element length

    !> Periodic system of elements
    character(len=2), parameter :: pse(118) = [ &
    & 'H ','He', &
    & 'Li','Be','B ','C ','N ','O ','F ','Ne', &
    & 'Na','Mg','Al','Si','P ','S ','Cl','Ar', &
    & 'K ','Ca', &
    & 'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn', &
    &           'Ga','Ge','As','Se','Br','Kr', &
    & 'Rb','Sr', &
    & 'Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd', &
    &           'In','Sn','Sb','Te','I ','Xe', &
    & 'Cs','Ba', &
    & 'La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb', &
    & 'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg', &
    &           'Tl','Pb','Bi','Po','At','Rn', &
    & 'Fr','Ra', &
    & 'Ac','Th','Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Md','No', &
    & 'Lr','Rf','Db','Sg','Bh','Hs','Mt','Ds','Rg','Cn', &
    &           'Nh','Fl','Mc','Lv','Ts','Og' ]

contains

!-------------------------------------------------
!> Just a wrapper for numberToSymbol
!-------------------------------------------------
elemental function toSymbol(number) result(symbol)

    integer, intent(in) :: number
    character(len=2) :: symbol

    call numberToSymbol(symbol,number)
    
end function toSymbol

!-------------------------------------------------
!> from number to periodic system of elements 
!-------------------------------------------------
elemental subroutine numberToSymbol(symbol,number)

    character(len=2), intent(out) :: symbol
        !! element symbol
    integer, intent(in) :: number
        !! atomic number

    if (number <= 0 .or. number > size(pse)) then
        symbol = '--'
    else
        symbol = pse(number)
    endif

end subroutine numberToSymbol

end module transformation