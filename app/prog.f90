program hf
  use parser, only : type_parser,init
  implicit none
  type(type_parser) :: parser

  call init(parser)  
  print *, "hello from project scf"
end program hf
