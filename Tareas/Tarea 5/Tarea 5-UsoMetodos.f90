
! ------------------------------------------------------------------------------------ !
! @file     : Tarea 05
! @author   : alujan
! @brief    : Preparación de examen
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

program free_fall

  use CP_methods

  implicit none

  ! Variables definition
  
  real(10) ::  tolerance = 1.E-5

  real :: m, x_initial = 50

  integer :: iterations = 1E6

  m = secant(f, 60., 55., tolerance, iterations)
  
  write(*,*) "Raiz hallada: ", m
  write(*,*) "Error asociado: ", tolerance
  write(*,*) "Iteraciones realizadas: ", iterations

  contains

  real function f(m)

    real, parameter :: v = 35.
    real, parameter :: g = 9.8
    real, parameter :: t = 9.
    real, parameter :: c = 15.

    real, intent(in) :: m

    f = v - m * g / c * (1 - exp(- c / m * t)) 

  end function

  real function df(m)

    real, parameter :: g = 9.8
    real, parameter :: t = 9.
    real, parameter :: c = 15.

    real, intent(in) :: m

    df = - g / c * (1 - exp(- c / m * t)) + m * g / c *  c / m**2 * t * exp(- c / m * t)

  end function


end program free_fall
