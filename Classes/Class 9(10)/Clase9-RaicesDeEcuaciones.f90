
! ------------------------------------------------------------------------------------ !
! @file     : Soluciones de ecuaciones f(x) = 0
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

program regula_falsi_example 
  
  ! Import of modules
  use CP_methods

  ! Declaration of variables
  implicit none

  ! Parameters definition for comparation
  real, parameter :: r_real = 2.36  ! Radious aproximation (A)
  
  ! Definition of variables to bisect
  real :: lInterval = 1., rInterval = 3.    ! Inteval values for the close juntion methods
  real :: x_aproximate = 2.                ! Initial aproximate for open juntion methods

  real(10) :: epsilon_1    = 1E-3 ! Tolerance defined for the zero: Bisection
  real(10) :: epsilon_2    = 1E-3 ! Tolerance defined for the zero: Regula Falsi
  real(10) :: epsilon_3    = 1E-3 ! Tolerance defined for the zero: Newton

  integer :: iterations_1 = 1E4  ! Maximum of iterations permited: Bisection
  integer :: iterations_2 = 1E4  ! Maximum of iterations permited: Regula Falsi
  integer :: iterations_3 = 1E4  ! Maximum of iterations permited: Newton

  ! Definition of variables to display
  real :: r_aproximate_1    ! Aproximated value: Bisection
  real :: r_aproximate_2    ! Aproximated value: Regula Falsi
  real :: r_aproximate_3    ! Aproximated value: Newton

  ! Use of the method: Bisect
  r_aproximate_1 = bisect(f, lInterval, rInterval, epsilon_1, iterations_1)

  ! Use of method: Regula Falsi
  r_aproximate_2 = regula_falsi(f, lInterval, rInterval, epsilon_2, iterations_2)

  ! Use of method: Newton
  r_aproximate_3 = newton(f, df, x_aproximate, epsilon_3, iterations_3)

  ! Display sequence
  write(*,*) "@ Example of use of the methods"

  write(*,*) ""

  write(*,*) " - The value of real radious considered is: r_real = ", r_real
  write(*,*) " - The value obtained with bisect it's: r_aproximated = ", r_aproximate_1
  write(*,*) " - The value obtained with regular falsi it's: r_aproximated = ", r_aproximate_2
  write(*,*) " - The value obtained with newton it's: r_aproximated = ", r_aproximate_3


  write(*,*) ""

  write(*,*) "The bisection method, shows as:"
  write(*,'(A, ES15.3E3)') " - The aproximation obtained with bisect has error: epsilon = ", epsilon_1
  write(*,*) " - The iterations needed was: iterations = ", iterations_2

  write(*,*) ""

  write(*,*) "The Regula Falsí method, shows as:"
  write(*,'(A, ES15.3E3)') " - The aproximation obtained with Regula Falsi has error: epsilon = ", epsilon_2
  write(*,*) " - The iterations needed was: iterations = ", iterations_2

  write(*,*) ""

  write(*,*) "The Newton method, shows as:"
  write(*,'(A, ES15.3E3)') " - The aproximation obtained with Newton has error: epsilon = ", epsilon_3
  write(*,*) " - The iterations needed was: iterations = ", iterations_3

  write(*,*) ""

  write(*,*) " - The value obtained on the potencial with bisect  is: V(r_aproximate) = ", V(r_aproximate_1)
  write(*,*) " - The value obtained on the potencial with regula falsi  is: V(r_aproximate) = ", V(r_aproximate_2)
  write(*,*) " - The value obtained on the potencial with Newton  is: V(r_aproximate) = ", V(r_aproximate_3)

  write(*,*) " - The expected value on the potencial is: V(r_real) = ", V(r_real)

  write(*,*) ""

  write(*,*) " - The value obtained on the potencial with bisect force is: f(r_aproximate) = ", f(r_aproximate_1)
  write(*,*) " - The value obtained on the potencial with regula falsi force is: f(r_aproximate) = ", f(r_aproximate_2)
  write(*,*) " - The value obtained on the potencial with Newton  is: f(r_aproximate) = ", f(r_aproximate_3)

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

  ! definition of the Force function
  real function df(r)

    ! Definition of function arguments
    real, intent(in) :: r     ! Radious value

    ! Definition of parameters on the problem
    real, parameter :: rho = .330, e_squared = 14.4   ! Parameters on the problem (A), (AeV)
    real, parameter :: alpha = 1.09E3   ! Kittel value (eV)

    real, parameter :: r_aproximate = 2.36  ! Radious aproximation (A)

    ! Definition of function form
    df = 2 * e_squared / r ** 3 - alpha / rho**2 * exp(-r / rho)

  end function

end program regula_falsi_example 
