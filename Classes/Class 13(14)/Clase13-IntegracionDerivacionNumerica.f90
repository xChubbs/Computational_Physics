
! ------------------------------------------------------------------------------------ !
! @file     : Clase 13
! @author   : alujan
! @brief    : Derivación e integración Numérica
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

! Se define el concepto de derivación númerica. Para compilar las múltiples métodologias
! definiremos una función capaz de aceptar la cantidad de puntos investigada y con estos
! Realizaremos las estimaciones

program differentiation_example

  ! Call of modules
  use CP_methods

  ! Definition of variables
  implicit none

  real(8) :: x_value = 1.    ! Point to evaluate the derivative of the function
  real(8) :: h = 1E-5       ! Distance to consider

  write(*,*) derivative(f, x_value, 2, 5, h)    ! Example of evaluation

  ! Definition of functions
  contains

  real(8) function f(x)

    real(8), intent(in) :: x    ! Value to evaluate

    f = x**3    ! Definition of dummy function

  end function ! End of fuction
  
end program differentiation_example
