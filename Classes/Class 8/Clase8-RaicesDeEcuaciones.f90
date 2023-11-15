
! ------------------------------------------------------------------------------------ !
! @file     : Soluciones de ecuaciones f(x) = 0
! @author   : alujan
! @brief    : Método de Bisección
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

! El método de Bisección
!
! Método empleado para hallar raices de funciones, basandonos en el teorema de valor
! intermedio. Este toma un algoritmo:
!
!   1. Selección de valores x_i y x_d, de tal forma en que se presenta un cambio de
!      signo en el intervalo.
!   2. Se realiza un estimativo de la raiz tomando una partición del intervalo, 
!      comunmente empleado x_r = (x_i + x_d) / 2
!   3. Realizamos la siguiente evaluación para replantear el subintervalo en el que
!      existe la raiz:
!         - Si f(x_i)f(x_r) < 0 : Luego se debe realizar el cambio del extremo
!           derecho, o sea x_d = x_r
!         - Si f(x_i)f(x_r) > 0 : Luego se debe realizar el cambio del extremo
!           izquierdo, o sea x_i = x_r
!         - Si f(x_r) = 0 : Se halla la raiz, o sea x_r es la raiz, y se finaliza
!           el cálculo.
!   4. Si se cumplen las tolerancias o precisiones requeridas, se termina la
!      iteración

! Desarrollo del programa de bisection: Método y ejemplo de uso

program bisect_example
  
  ! Import of modules
  use CP_methods

  ! Declaration of variables
  implicit none

  ! Parameters definition for comparation
  real, parameter :: r_real = 2.36  ! Radious aproximation (A)
  
  ! Definition of variables to bisect
  real :: lInterval = 1, rInterval = 3    ! Inteval values to bisect

  real(10) :: epsilon    = 1E-6 ! Tolerance defined for the cero

  integer :: iterations = 1E4  ! Maximum of iterations permited

  ! Definition of variables to display
  real :: r_aproximate    ! Aproximated value

  ! Use of the method
  r_aproximate = bisect(f, lInterval, rInterval, epsilon, iterations)

  ! Display sequence
  write(*,*) "@ Example of use of the bisection method"

  write(*,*) ""

  write(*,*) " - The value of real radious considered is: r_real = ", r_real
  write(*,*) " - The value obtained it's: r_aproximated = ", r_aproximate

  write(*,*) ""

  write(*,*) "The bisection method, shows as:"
  write(*,'(A, ES15.3E3)') " - The aproximation obtained has error: epsilon = ", epsilon
  write(*,*) " - The iterations needed was: iterations = ", iterations

  
  write(*,*) ""

  write(*,*) " - The value obtained on the potencial is: V(r_aproximate) = ", V(r_aproximate)
  write(*,*) " - The expected value on the potencial is: V(r_real) = ", V(r_real)

  write(*,*) ""

  write(*,*) " - The value obtained on the potencial force is: f(r_aproximate) = ", f(r_aproximate)
  write(*,*) " - The expected value on the potencial force is: f(r_real) = ", f(r_real)

  ! Definition of functions
  contains

  ! Definition of the Potencial function
  real function V(r)

    ! Definition of function arguments
    real, intent(in) :: r     ! Radious value

    ! Definition of parameters on the problem
    real, parameter :: rho = .330, e_squared = 14.4   ! Parameters on the problem (A), (AeV)
    real, parameter :: alpha = 1.09E3   ! Kittel value (eV)

    ! Definition of function form
    V = -e_squared / r + alpha * exp(-r / rho)

  end function


  ! definition of the Force function
  real function f(r)

    ! Definition of function arguments
    real, intent(in) :: r     ! Radious value

    ! Definition of parameters on the problem
    real, parameter :: rho = .330, e_squared = 14.4   ! Parameters on the problem (A), (AeV)
    real, parameter :: alpha = 1.09E3   ! Kittel value (eV)

    real, parameter :: r_aproximate = 2.36  ! Radious aproximation (A)

    ! Definition of function form
    f = - e_squared / r ** 2 + alpha / rho * exp(-r / rho)

  end function

end program bisect_example 
