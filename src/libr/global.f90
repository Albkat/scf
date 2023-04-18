module global
    use environment, only : type_environment
    
    logical :: strict = .false.
        !! set strict to turn all warnings into hard errors
    
    type(type_environment), allocatable :: persistentEnv
        !! global env

    character(len=:), allocatable :: name
        !! name of the currently running program

end module global