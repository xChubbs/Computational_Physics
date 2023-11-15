
! ------------------------------------------------------------------------------------- !
! @file     : Examen 1. Fundamentos de Programación
! @author   : alujan
! @brief    : Parte práctica del examen
! ------------------------------------------------------------------------------------- !
! @authors
! 
! Introducción a la Física Computacional
! Facultad de Ciencias
! Ing. Física
!
! ------------------------------------------------------------------------------------- !
! @atention
! 
! El siguiente código no cuenta con derechos de autor, sus intenciones son en su 
! totalidad completamente educativas. Frente a cualquier duda, se recomienda
! remitirse directamente al autor.
!
! ------------------------------------------------------------------------------------- !
!

! --------------------------------- Main Program Body --------------------------------- !
program main

  implicit none   ! No implicit definitions

  ! Definition of parameters
  real, parameter :: L = 5      ! Inductance
  real, parameter :: C = 1.E-4  ! Capacitance

  real, parameter :: t_i = 0.05              ! Time reference
  real, parameter :: q_porcentual = 1E-2  ! Disipation of charge wanted

  ! Definitions of variables: Regula Falsi
  real :: R_left = 300, R_right = 400     ! Interval of aproximation
  real :: R_aprox_secant = 0.             ! Regula Falsi aproximation

  real(10):: delta_regula_falsi = 1E-5     ! Error permitted for aproximations
  integer :: iter_regula_falsi = 1E3       ! Maximum Iterations permitted


  ! Definition of variables: Secant
  real :: R_initial_0 = 300., R_initial_1 = 320.    ! Initial guess for R
  real :: R_aprox_regula_falsi = 0.                 ! Secant aproximation

  real(10):: delta_secant = 1E-5     ! Error permitted for aproximations
  integer :: iter_secant  = 1E3      ! Maximum Iterations permitted
  
  ! Definitions for control variables:
  integer :: control     ! Control flow
  integer :: iteration   ! Display iterations

  ! Welcome sequence:
  write(*,*) "Exam done by @alujan"

  write(*,*) " This program is an interative solution for the problem proposed"
  write(*,*) " First, we must define the values for the methods:"
  write(*,*) ""

  write(*,*) "  For this task, it's going to be convinient to make a small table of values to learn"
  write(*,*) "  about the behaviour of the function:"

  display_values: do iteration = 0, 400, 50

    ! Display of value:
    write(*,*) "   For x = ", iteration, " the function takes the value", f(real(iteration))

  end do display_values

    write(*,*) ""

  ! ----------------------------------- Regula Falsi Definitions & Iterations ----------------------------------- !
  write(*,*) " >> Regula-Falsi:"
  write(*,'(a)', advance='no') "  - Interval defined: "
  write(*,*) R_left, R_right
  write(*,'(a)', advance='no') "  - Tolerance admitted: "
  write(*,*) delta_regula_falsi
  write(*,'(a)', advance='no') "  - Iterations admitted: "
  write(*,*) iter_regula_falsi
  write(*,*) ""

  write(*,'(a)', advance='no') " Do you want to modify the default values defined ? (y : 1/ n : 0): "
  read(*,*) control

  write(*,*) ""

  ! Control flow to change terms 
  control_flow_regula_falsi: do while(control .EQ. 1)

    control = 0   ! Lower of flag

    ! Reading of values
    write(*,'(a)', advance='no') "  - Interval defined (do not exceed R = 425): "
    read(*,*) R_left, R_right
    write(*,'(a)', advance='no') "  - Tolerance admitted: "
    read(*,*) delta_regula_falsi
    write(*,'(a)', advance='no') "  - Iterations admitted: "
    read(*,*) iter_regula_falsi
    write(*,*) ""

    ! Control contaiment for change of values:
    write(*,'(a)', advance='no') " Do you want to modify the values defined ? (y : 1/ n : 0): "
    read(*,*) control

  end do control_flow_regula_falsi

  ! Call of the methods with values defined
  CALL regula_falsi(f, R_left, R_right, delta_regula_falsi, iter_regula_falsi, R_aprox_regula_falsi)

  ! Echo for the values obtained:
  write(*,*) ""
  write(*,*) " >> Values obtained:"
  write(*,'(a)', advance='no') "  - Aproximation obtained "
  write(*,*) R_aprox_regula_falsi
  write(*,'(a)', advance='no') "  - Error obtained: "
  write(*,*) delta_regula_falsi
  write(*,'(a)', advance='no') "  - Iterations needed for convergence: "
  write(*,*) iter_regula_falsi
  write(*,*) ""

  ! ------------------------------------- Secant Definitions & Iterations ------------------------------------- !
  write(*,*) " >> Secant:"
  write(*,'(a)', advance='no') "  - First iterations defined: "
  write(*,*) R_initial_0, R_initial_1
  write(*,'(a)', advance='no') "  - Tolerance admitted: "
  write(*,*) delta_secant
  write(*,'(a)', advance='no') "  - Iterations admitted: "
  write(*,*) iter_secant
  write(*,*) ""

  write(*,'(a)', advance='no') " Do you want to modify the default values defined ? (y : 1/ n : 0): "
  read(*,*) control

  write(*,*) ""

  ! Control flow to change terms 
  control_flow_secant: do while(control .EQ. 1)

    control = 0   ! Lower of flag

    ! Reading of values
    write(*,'(a)', advance='no') "  - First iterations defined (do not exceed R = 425): "
    read(*,*) R_initial_0, R_initial_1
    write(*,'(a)', advance='no') "  - Tolerance admitted: "
    read(*,*) delta_secant
    write(*,'(a)', advance='no') "  - Iterations admitted: "
    read(*,*) iter_secant
    write(*,*) ""

    ! Control contaiment for change of values:
    write(*,'(a)', advance='no') " Do you want to modify the values defined ? (y : 1/ n : 0): "
    read(*,*) control

  end do control_flow_secant

  ! Call of the methods with values defined
  CALL secant(f, R_initial_0, R_initial_1, delta_secant, iter_secant, R_aprox_secant)

  ! Echo for the values obtained:
  write(*,*) ""
  write(*,*) " >> Values obtained:"
  write(*,'(a)', advance='no') "  - Aproximation obtained "
  write(*,*) R_aprox_secant
  write(*,'(a)', advance='no') "  - Error obtained: "
  write(*,*) delta_secant
  write(*,'(a)', advance='no') "  - Iterations needed for convergence: "
  write(*,*) iter_secant
  write(*,*) ""

  ! Conclusions
  write(*,*) "A close comercial value for the resistance needed it's 330 Ohms, fo the aplication propuses, and"
  write(*,*) "The power disipation that's going to be have under this conditions are: ", q(330., t_i)
  ! End Sequence
  write(*,*) ""
  write(*,*) "@author notes:"
  write(*,*) " - The error was thought with the magnitude of the zero, because of the precision"
  write(*,*) "   used, the values can be modified and are prepared to abort on any failure"
  write(*,*) " - The problem was initially solved analitically, and it's found that only 1 rooth exists,"
  write(*,*) "   under smooth function conditions, low magnitude for the derivative and only 1 rooth, the methods "
  write(*,*) "   were expected to converge"

  ! ---------------- Definition of subroutines that contains the methods -------------- !
  contains

	  ! --------------------- Regula Falsí: Rooths f(x) = 0 ------------------- !
	  subroutine regula_falsi(f, lInterval, rInterval, delta, iterations, rooth)
	
	    ! Input description: Declaration of variables
	    implicit none
    
	    real, external :: f ! Function to Use Regula Falsi
	
	    real, intent(in) :: lInterval   ! Left of the interval
	    real, intent(in) :: rInterval   ! right of the interval
	
	    real(10), intent(inout) :: delta    ! Error obtained for the method
	
	    integer, intent(inout) :: iterations    ! Number of iterations

      real, intent(out) :: rooth    ! Rooth of the function
	
	    ! Method description: Declaration of variables
	    real     :: a = 0, b = 0    ! Handlers of the interval
	    real(10) :: epsilon  = 0    ! Error obtained on the method
	    real     :: rFalsi_k = 0    ! Iteration of step k
	
	    integer :: iter = 0   ! Iteration indexer
	    
	    ! Internal definitions
	    a = lInterval; b = rInterval
	
	    ! Method description: Error handling
	    if(.not. (f(a) * f(b) .LE. 0) ) then
        write(*,*) f(a) * f(b), a, b
        ERROR STOP " Error found on the arguments: f(a) * f(b) > 0"
      endif
	
	    ! Method description: Special cases:
	    if( abs(f(a)) .EQ. 0 ) then
        rooth = lInterval; iterations = 0; delta = 0
        return

	    else if( abs(f(b)) .EQ. 0 ) then
        rooth = rInterval; iterations = 0; delta = 0
        return

      endif
	
	    ! Method description: Iterations
	    regula_falsi_iter: do iter = 1, iterations
	
	      ! Iteration of step k
	      001 rFalsi_k = b - f(b) * (a - b) / (f(a) - f(b))
	
	      ! Cases handling: return point
	      if(f(rFalsi_k) .EQ. 0) then; EXIT;    ! Zero found
	
	      else if( f(rFalsi_k) * f(b) .LT. 0 ) then   ! Left inteval change
	        a = rFalsi_k
	
	      else    ! Right interval change
	        b = rFalsi_k
	
	      endif
	
	      epsilon = abs(b - a)    ! Computation of error
	  
	      ! Control flow: Exit by definitions
	      if(epsilon .LT. delta) then ! Steps close
          EXIT 

	      else if(abs(f(rFalsi_k)) .LT. delta) then  ! Zero close to each other
        
          epsilon = abs(f(rFalsi_k)) 
          EXIT
        endif  

	    end do regula_falsi_iter
	    
	    ! Compilation of results
	    iterations = iter;    ! Echo of the iterations needed
	
	    delta = epsilon       ! Echo of the error obtained
	
	    rooth = rFalsi_k     ! Echo of the value obtained

      return    ! Return to main
	
	  end subroutine  ! Regula Falsi
	 
    ! --------------------- Secant: Rooths f(x) = 0 ------------------- !
	  subroutine secant(f, x_prev, x_next, delta, iterations, rooth)
	
	    ! Input description: Declaration of variables
	    implicit none
	
	    real, external :: f ! Function to bisect
	
	    real, intent(in) :: x_prev   ! Left of the interval
	    real, intent(in) :: x_next   ! right of the interval
	
	    real(10), intent(inout) :: delta    ! Error obtained for the method
	
	    integer, intent(inout) :: iterations    ! Number of iterations

      real, intent(out) :: rooth    ! Rooth of the function
	
	    ! Method description: Declaration of variables
	    real     :: x0 = 0, x1 = 0    ! Handlers of the interval
	    real(10) :: epsilon  = 0    ! Error obtained on the method
	    real     :: secant_k = 0    ! Iteration of step k
	
	    integer :: iter = 2   ! Iteration indexer
	    
	    ! Method description: initialization
	    x0 = x_prev; x1 = x_next
	          
	    ! Method description: Special cases:
	    if( abs(f(x0)) .EQ. delta ) then
        rooth = x0; iterations = 0; delta = 0
        return

	    else if( abs(f(x1)) .EQ. delta ) then
        rooth = x1; iterations = 1; delta = abs(x1 - x0)
        return

      endif
	
	    ! Method description: Iterations
	    secant_iter: do iter = 2, iterations
	
	      ! Iteration of step k
	      secant_k = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))
	      
	      ! Control Flow: definition of next variables
	      002 x0 = x1; x1 = secant_k
	
	      epsilon = abs(x1 - x0)    ! Computation of error
	
	      ! Control flow: Exit by definitions
	      if(epsilon .LT. delta) then
          EXIT  ! Steps close

	      else if(abs(f(x1)) .LT. delta) then ! Zero close to each other

          epsilon = abs(f(x1))
          EXIT

        endif

	    end do secant_iter
	    
	    ! Compilation of results
	    iterations = iter;    ! Echo of the iterations needed
	
	    delta = epsilon       ! Echo of the error obtained
	
	    rooth = secant_k     ! Echo of the value obtained
	
      ! Return sequence to main
      return

    end subroutine  ! Bisection

    ! ---- Definition of the function with global variables ---- !
    real function q(R, t)

      ! Definition of variables
      real, intent(in) :: R     ! Resistance
      real, intent(in) :: t     ! Time

      003 q = exp(-R * t / (2.*L)) * cos(sqrt(1./(L*C) - (R/(2.*L))**2) * t)

    end function


    real function f(R)

      ! Definition of variables
      real, intent(in) :: R     ! Resistance

      003 f = exp(-R * t_i / (2*L)) * cos(sqrt(1./(L*C) - (R/(2.*L))**2) * t_i) - q_porcentual 

    end function


end program main
