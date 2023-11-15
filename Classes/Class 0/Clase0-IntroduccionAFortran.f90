
! ------------------------------------------------------------------------------------ !
! @file     : Clase 0
! @author   : alujan
! @brief    : Introducción al lenguaje FORTRAN
! ------------------------------------------------------------------------------------ !
! @authors
! 
! Introducción a la Física Computacional
! Facultad de Ciencias
! Ing. Física
!
! ------------------------------------------------------------------------------------ !
! @atention
! 
! El siguiente código no cuenta con derechos de autor, sus intenciones son en su 
! totalidad completamente educativas. Frente a cualquier duda, se recomienda
! remitirse directamente al autor.
!
! ------------------------------------------------------------------------------------ !

program introduction

! Declaring Variables
! The base syntax for this involves:
!
!   <variable_type> :: <variable_name>
!
! In FORTRAN there're 5 diferent types of Variables:
!   - integer   :: Representation of whole numbers, positive or negative
!   - real      :: Floating point data (Not whole)
!   - complex   :: Pair of numbers consisting of (real, imaginary) arguments
!   - character :: Text data
!   - logical   :: Boolean representation of values
!
! There's something aditional to know, the statement
  
  implicit none

! states that all variables are explicitly declared, without this statement,
! the variables will be implicitly typed according to the letter they begin.
! An example of this, can be defined as:
!
! implicit complex::(u, v, w), character*4::(c,s)
!
! This denotes that everything that starts with u, v, w are complex quantities,
! and c, s are characters of 4 elements. There are other types of declarations
! like undefined (A-Z), but this will be decoded later

  integer :: amount
  real :: pi
  complex :: frequency
  character :: initial
  logical :: truth

! Is important to state, FORTRAN is Case-Insensitive, then there's no
! differentiation between variable names. Now the assigment must be done
! separetly, using '=':

  amount = 10
  pi = 3.1415
  frequency = (1.0, -2.5)
  initial = 'w'
  truth = .true.

! This assigment implies a value that's not save during procedure calls, if the
! variable it's initialized in one line, it will retain a save value attribute:
!
! integer :: amount = 1
!
! Implies amount will retain it's value, it's not changing between declarations

! Input / Outputs
! There's a simple syntax for this actions. it's just defined the actions as
! follows:
!
!   print *, 'Text to be printed out'
!   read (*,*) variable_read
!
! Operations: Arithmetic
! This involves the use of operation format similar to python's formatting.
!
!   - Exponent **
!   - Multiplication *
!   - Division /
!   - Addition +
!   - Substraction -
!
! The operators are closed in their type. for example a division between 2
! real variables is real, but the division between 2 integer variables is an
! integer. There's a mixed mode Arithmetic where the operant is converted to
! the biggest variable type, the the division between a real and an integer
! variable is defined as an integer result.

end program introduction
