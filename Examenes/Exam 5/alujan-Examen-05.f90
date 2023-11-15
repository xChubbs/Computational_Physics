1
! ------------------------------------------------------------------------------------ !
! @file     : examen 5
! @author   : @alujan
! @brief    : Ecuaciones diferenciales Ordinarias
! ------------------------------------------------------------------------------------ !
! @authors
! 
! introducción a la física computacional
! facultad de ciencias
! ing. física
!
! ------------------------------------------------------------------------------------ !
! @atention
! 
! el siguiente código no cuenta con derechos de autor, sus intenciones son en su 
! totalidad completamente educativas. frente a cualquier duda, se recomienda
! remitirse directamente al autor.
!
! ------------------------------------------------------------------------------------ !
! @Consideraciones del problema
!
! Veamos que se tiene una ecuación diferencial ordinaria de segundo orden no lineal para
! theta. Los métodos desarrollados en clase estan enfocados en la solución de problemas
! de ecuaciones diferenciales de primer orden con solución única, la verificación de la
! solución única para el problema se sale del alcance del curso, pero podemos verificar
! que el PVI propuesto tiene solución única, y para abordarlo es necesario generar un
! sistema de ecuaciones diferenciales de primer orden que sea equivalente al propuesto,
! para esto se propone:
!
!   Declaramos un vector Theta, tal que:  Theta = [theta, dtheta] = [θ_1, θ_2]
!
! Donde Theta corresponde a la variable dependiente de la ecuación diferencial, y 
! dtheta hace referencia a la derivada de theta con respecto al tiempo t.
!
!   Con esto podemos reducir la condición inicial a Theta_inicial = [pi / 4, 0],
!   y finalmente establecer las relaciones empleado el operador diferenciación como:
!       
!       Theta' = [theta, dtheta]' = [dtheta, d2theta]
!
!   Con esta definición es claro que podemos generar una función de campo que describa
!   el comportamiento de la derivada para esta situación puntual:
!
!     Theta' = F([theta, dtheta]) = [θ_2, - g / L * sin(θ_1)]
!
!   Veamos que esta definición permite generar un sistema vectorial de primer orden
!   equivalente al propuesto de segundo orden inicialmente.
!   
! Con lo anterior dicho, generamos la aproximación a la solución en el siguiente código. 
! 
! ------------------------------------------------------------------------------------ !
! @Consideraciones para la verificación del programa
!
! En el archivo generado theta_values.txt se genera una matriz que contiene los valores
! respectivamente por columnas de tiempo, theta y omega (d_theta), estos pueden
! visualizarse empleando un software como MATLAB, empleando un código como el presentado
! a continuación
!
!									% Clear of previous values
!									clear
!									
!									% Definition of intervals on program
!									intervals = 100;
!									
!									% Open of the file
!									file = fopen("theta_values.txt", 'r');
!									
!									% Scan of values on file
!									matrix_values = fscanf(file, '%f', [3, intervals + 1])';
!									
!									% Close of file
!									fclose(file);
!									
!									% Definition of contents
!									T     = matrix_values(:, 1);
!									theta = matrix_values(:, 2);
!									omega = matrix_values(:, 3);
!									
!									% Definition of the problems values
!									L = .5; g = 9.78;
!									
!									% Definition of expected behaviour values
!									Period    = 2 * pi * sqrt(L / g);
!									frecuency = 1 / Period;
!									 
!									f  = @(x) pi/4 * cos(pi/frecuency * x);
!									
!									df = @(x) - pi/frecuency * pi/4 * sin(pi/frecuency * x);
!									
!									% Clear of previous plots
!									clf('reset')
!									
!									hold on
!									
!									% Plot of theta vs time
!									plot(T, theta, 'b', DisplayName= '\theta(t)')
!									
!									% Plot of expected theta behaviour
!									fplot(f, [0, 5], DisplayName= '\theta_{expected}(t)')
!									
!									% Plot of omega vs time
!									plot(T, omega, 'r', DisplayName= '\omega(t)')
!									
!									% Plot of expected omega behaviour
!									fplot(df, [0, 5], DisplayName= '\omega_{expected}(t)')
!									
!									% Plot of the Periods ticks
!									plot([1 * Period, 1 * Period], [-pi/4, pi/4], ...
!									    'k', LineStyle= ':', DisplayName = 'Expected Period')
!									
!									plot([2 * Period, 2 * Period], [-pi/4, pi/4], ...
!									    'k', LineStyle= ':', HandleVisibility='off')
!									
!									plot([3 * Period, 3 * Period], [-pi/4, pi/4], ...
!									    'k', LineStyle= ':', HandleVisibility='off')
!									
!									% Auxiliar definitions for better visualization
!									yline(0, HandleVisibility = 'off')
!									
!									title('Behaviour present on the theta values file')
!									subtitle('Comparation of expected vs obtained')
!									
!									xlabel('time(seconds)')
!									
!									legend(Location = 'bestoutside', Interpreter= 'tex')
!									
!									grid on
!
! En el código presentado anteriormente se emplean 100 intervalos, para visualización
! adaptativa empleando una cantidad cualquiera de intervalos, solo basta con cambiar
! el valor de la variable intervalos, y con esto se logra apreciar la visualización
! obtenida.
!
! ------------------------------------------------------------------------------------ !

! solución de examen 5: Ecuaciones diferenciales
program main

  ! Variables definition
  implicit none

  ! Universal variables definition
  real(4), parameter :: pi = 4. * ATAN(1.)   ! PI definition  
  real(8), parameter :: g  = 9.78           ! Gravity constant definition

  ! Definition of inner program variables
  real(8), parameter :: L = 0.5             ! Length of pendulum

  real(8) :: t_start       ! Left value of interval
  real(8) :: t_step        ! Time delta to be considered for the mesh
  real(8) :: t_end         ! Right value of interval

  real(8), dimension(2) :: theta_initial = (/ pi/4., 0. /)    ! Theta initial conditions Theta = <θ_1, θ_2>

  real(8), allocatable, dimension(:, :) :: theta              ! Theta evaluation Theta = <θ_1(0), θ_2(0)>

  integer :: intervals    ! Number of intervals to consider

  integer :: i      ! Iteration for tabulation

  ! Welcome sequence
  write(*,*) " program made by @alujan"
  write(*,*) "The program contains the solution for the problem presented"
  write(*,*)  ! white space
  write(*,*) " >> To start, first define a number of intervals to be considered in the numerical aproximation"
  write(*,'(a)', advance= 'no') "    Intervals: "
  read(*,*) intervals
  write(*,*)  ! white space

  ! Initialization of variables
  allocate(theta(intervals +1, size(theta_initial) + 1))
  t_start = 0.; t_end = 5.        ! Time interval for PVI

  ! Call of function
  theta = RK4s_PVI(F, t_start, t_end, theta_initial, intervals)

  ! Evaluation sequence of the program
  write(*,*) "With the conditions presented, we have:"
  write(*,'(a,10F8.2)') " - Initial time presented: ", t_start
  write(*,'(a,10F8.2)') " - Final time presented: ", t_end
  write(*,'(a,I10)') " - Intervals of time considered: ", intervals
  write(*,'(a,20ES15.7)') " - Step of time between values: ", (t_end - t_start) / intervals
  write(*,*)  ! white space
  write(*,*) "The Evaluation it's sumarized in the table below"
  write(*,*)  ! white space
  
  ! Table Header definition
  write(*,*) "       time(seconds)       |        Angle(theta)         | Angular Velocity(omega)"

  ! Print out of values
  table_console_printout: do i = 1, (intervals + 1)

    ! Row defintion
    write(*,*) theta(i, 1), " | ", theta(i, 2), " | ", theta(i, 3) 

  end do table_console_printout

  ! Creation of auxiliar table for graphication
  open(UNIT = 1, FILE = 'theta_values.txt', STATUS = 'UNKNOWN')

  table_file_printout: do i = 1, (intervals + 1)

    ! Row defintion
    write(1,*) theta(i, :)

  end do table_file_printout

  close(1)

  ! End sequence of the program
  write(*,*)  ! Whitespace
  write(*,*) "With the values presented, and with aid of a external software for graphication purpuses"
  write(*,*) "we can  see that the values obtained for theta periodic but not harmonic, where along the"
  write(*,*) "increase of time the difference between the expected performance of theta, using the small"
  write(*,*) "angle aproximation."

  ! Functions definitions
  contains

  ! Vectorial field: Description of Vectorial system ODE
  function F(t, Y) result(X)

    ! Variables definition
    implicit none

    real(8), intent(in) :: t      ! Variable T
    
    real(8), dimension(:), intent(in) :: Y      ! Variable Y

    real(8), allocatable, dimension(:) :: X     ! Returning of the function 

    integer :: i    ! Component iterator

    ! Initialization of variables
    allocate(X(size(Y)))

    ! Function definition
    X = (/ Y(2), - g / l * sin(Y(1)) /)

  end function F

  ! Runge - Kutta 4 Vectorial Method: PVI with first Taylor step
  function RK4s_PVI(f, t_initial, t_final, y_initial, M) result(RK4)

    ! Variables definition
    implicit none 
    
    real(8), dimension(:), intent(in) :: y_initial          ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final               ! Interval to evaluate

    integer, intent(in) :: M                                ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: RK4            ! Matrix RK4(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T                 ! T Component definition of RK4
    
    real(8), allocatable, dimension(:,:) :: Y               ! Y Component definition of RK4

    real(8), allocatable, dimension(:) :: F1, F2, F3, F4    ! Temporal components of summation

    real(8) :: h      ! Delta t: Length of interval to take

    integer :: i, j   ! Iterators for M matrix

    ! Definition of interface for Vectorial function F
    interface

      function f(t, Y) result(X)
        real(8), intent(in) :: t                    ! Variable T
    
        real(8), dimension(:), intent(in) :: Y      ! Variable Y

        real(8), allocatable, dimension(:) :: X     ! Returning of the function 

      end function f

    end interface

    ! Initialization of variables
    allocate(RK4(M + 1, size(y_initial) + 1))                     ! Allocation of RK4 matrix

    allocate(T(M + 1)); allocate(Y(M + 1, size(y_initial)))       ! Allocation of components for RK4 Matrix

    ! Allocation of temporal terms of summation
    allocate(F1(size(y_initial)))
    allocate(F2(size(y_initial)))  
    allocate(F3(size(y_initial)))
    allocate(F4(size(y_initial)))

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i, :) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      F1 = h * f(T(i), Y(i, :))
      F2 = h * f(T(i) + h/2, Y(i, :) + F1/2)
      F3 = h * f(T(i) + h/2, Y(i, :) + F2/2)
      F4 = h * f(T(i) + h  , Y(i, :) + F3  )

      Y(i + 1, :) = Y(i, :) + (F1 + 2*F2 + 2*F3 + F4) / 6

    end do explicit_evaluation

    RK4(:, 1) = T; RK4(:, 2:) = Y    ! Component composition RK4

  end function ! Runge-Kutta 4 Vectorial Method

  end program main
