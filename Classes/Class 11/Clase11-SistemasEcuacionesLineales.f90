
! ------------------------------------------------------------------------------------ !
! @file     : Sistemas de ecuaciones lineales | F(X) = 0 
! @author   : alujan
! @brief    : Método de Regular Falsí
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
!

! Ejemplo de aplicación del método de reducción gaussiana

program gauss_linear_system 
  
  ! Import of modules
  use CP_methods

  ! Declaration of variables
  implicit none
  
  real(8), dimension(4, 4) :: A
  real(8), dimension(4) :: b
  real(8), dimension(4) :: X

  real(8), dimension(4, 5) :: extended

  integer :: i

  A = reshape((/ 4., -4., -5., 2., 3., 3., 5., -1., 2., 1., -1., 1., -1., 2., -1., 1. /), shape(A), order=[2, 1])

  b = (/ 7., 9., 4., 1. /) 

  X = gauss_elimination(A, b)

  write(*,*) X
  
end program gauss_linear_system 
