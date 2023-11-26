
! ------------------------------------------------------------------------------------ !
! @file     : Proyecto Física computacional
! @author   : @mtellez & @alujan
! @brief    : Solución de la Ecuación de schrodinger en el potencial de Morse
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
! @Disclaimer
!
! Para fácilidad del cálculo numérico y ondar fuertemente en los resultados a obtener,
! se realiza la adimensionalización de la ecuación de Schrodinger.
!
! ------------------------------------------------------------------------------------ !

program main

  ! ------------------------------- Variables definition ----------------------------- !

  implicit none

  ! Parameters defintion
  real(8), parameter :: m     = 6.643E-27         ! Mass of an alpha particle (kg)
  real(8), parameter :: a_0   = 5.291772109E-1    ! Bohr radious (A)
  real(8), parameter :: R_y   = 13.6057           ! Rydberg constant (eV)
  real(8), parameter :: h_bar = 1.054571817E-34   ! h bar constant (J s)

  real(8), parameter :: beta = 1.31956687E-1      ! Well width (A)
  real(8), parameter :: De   = 10.474             ! Well depth (eV)

  integer, parameter :: lambda = 6                ! Adimentional Schrodinger Eq Variable  

  ! Variables definition
  real(8), allocatable, dimension(:) :: epsilons  ! Energies array

  integer :: n, n_max                             ! n definition of state (Level energy)

  double precision :: epsilon                              ! Iterations of energy to be considered

  double precision :: epsilon_step                         ! Step to consider for the aproximation

  double precision :: epsilon_left, epsilon_right          ! Definition of limits for Energy finding


  double precision, allocatable, dimension(:) :: delta    ! Difference admitted for energy


  real(8) :: x_left = -3.                           ! Definition of far limits: Inifinity for the aproximation

  real(8), allocatable, dimension(:) :: x_right     ! Right limits defintions: variable over functions

  double precision, dimension(2) :: Psi_condition          ! Conditions for psi function for aproximation

  double precision, allocatable, dimension(:, :) :: Psi_k  ! Psi wave function for k energy step

  double precision :: psi_left, psi_right, psi_iteration   ! Control variables for iteration

  double precision, allocatable, dimension(:, :) :: psi    ! End of catch energy array


  logical :: root_control = .TRUE.     ! Control flow for root finded

  integer :: dim = 100000              ! Number of steps for psi_k

  integer :: k                         ! Iterator definition

  ! ------------------------------------ Initialization of variables -------------------------------------- !

  allocate(Psi_k(dim +1, 2))              ! Allocation of step energy psi

  n = 0; n_max = int(lambda - 1/2)        ! Energy states allowed: n = 1, ..., floor(lambda - 1/2)

  allocate(epsilons(0:n_max))             ! Energies allowed map

  allocate(psi(dim +1, 0:n_max))          ! Psi functions found per energy level

  allocate(delta(0:n_max))

  allocate(x_right(0:n_max))


  epsilon_left = 0.; epsilon_right = lambda ** 2            ! Initialization of energy limits [0., lambda^2]

  epsilon_step = (epsilon_right - epsilon_left) / 2E2       ! Epsilon initial step required

  epsilon = epsilon_left                                    ! Epsilon iteration initialization

  x_right = (/ 12., 12., 14., 16., 18., 20. /)

  delta = (/ 1.d-18, 1.d-26, 1.d-26, 1.d-30, 1.d-35, 1.d-40 /) ! Maximum error admitted per energy


  Psi_condition = (/ 1.d-71, 0.d0 /)                      ! Even conditions: Near zero conditions

  psi_k = RK4s_PVI(F, x_left, x_right(n + 1), Psi_condition, dim)      ! Initial conditions for iteration

  psi(:, 0) = psi_k(:, 1)                                       ! Definition of x partitions for psi

  psi_left = psi_k(dim +1, 2)                                   ! Left psi definition

  ! ----------------------- Energy finding: Expected energies[0, Floor{lambda - 1/2}] ----------------------- !
  !
  ! En esta sección es donde ocurre la busqueda de energia, en un primer momento la iteración se realiza
  ! progresivamente comenzando desde una energía cero hasta hallar un intervalo discreto en el cual se
  ! un cambio de signo, con esto se evidencia que debe existir una raíz en el intervalo.
  !
  ! Una vez se halla un intervalo donde existe la raiz, se cambia la metodología de iteración y se procede a
  ! realizar bisección para hallar el valor de energía presente en el intervalo hasta una tolerancia dada, 
  ! ya que los valores a hallar de energía dependen de las definiciones intrinsicas de la función se emplean
  ! arreglos variables que permitan tomar la decisión en la inicialización de variables.
  !
  ! Tener presente que la busqueda se realiza empleando el operador diferencial como función de la energía,
  ! donde cada valor es hallado solucionando la ecuación diferencial y verificando que la condición de
  ! frontera se satisfaga para el intervalo.

  energy_fishing: do
    
    ! -------------------- Root finding: Iteration over large step ------------------ !
    !
    ! Busqueda de un intervalo cerrado donde se pueda asegurar que
    ! existe la raiz.

    if(root_control) then

      ! Current step: next step
      epsilon = epsilon + epsilon_step

      ! Definition of values
      psi_k = RK4s_PVI(F, x_left, x_right(n + 1), Psi_condition, dim)

      psi_iteration = psi_k(dim +1, 2)

      ! Debugging: Print of conditions
      write(*,*) "Current root finding", psi_left, psi_iteration

      ! Root in interval
      if(psi_left * psi_iteration .LT. 0) then

        ! Preparation of telescopic search
        epsilon_right = epsilon

        psi_right = psi_iteration

        ! Close of gate
        root_control = .FALSE.

        ! Catch
        write(*,*) "Catch of energy, state must be between it's betweeen: ", epsilon_left, epsilon_right

      ! No root on interval
      else

        ! Preparation for next iteration
        epsilon_left = epsilon

        psi_left = psi_iteration

      endif

    ! --------------------------- Root enclosing: iteration over small step -------------------------- !
    !
    ! Bisección en el intervalo cerrado para convergencia del valor. Bisección tiene la
    ! ventaja que minimiza rápidamente el valor de la raiz a convergencia del método,
    ! Entonces no requerimos altas tolerancias para finas raices.

    else 

      ! current step:
      epsilon = (epsilon_right + epsilon_left) / 2 

      ! Definition of values
      psi_k = RK4s_PVI(F, x_left, x_right(n + 1), Psi_condition, dim)

      psi_iteration = psi_k(dim +1, 2)

      ! Debugging: Bisection
      write(*,*) "Current root finding", psi_left, psi_right
      
      ! Bisection control
      if(psi_left * psi_iteration .LT. 0) then

        epsilon_right = epsilon
        psi_right     = psi_iteration

      else

        epsilon_left = epsilon
        psi_left     = psi_iteration

      endif

    endif

    ! ------------------------------ Zero finding control flow -------------------------------- !
    !
    ! Despues de captura de un auto estado y auto valor debemos guardar estos valores en
    ! los arreglos desginados para posterior procesamiento empleando otro lenguaje  

    if(abs(psi_iteration) .LT. delta(n + 1)) then

      ! Catch of value
      write(*,*) "Catch of energy, The tolerance it's satisfied: ", epsilon, n

      ! Storage of values
      epsilons(n + 1) = epsilon

      psi(:, n + 1) = psi_k(:, 2)
      
      ! Next step preparation
      n = n + 1

      ! Preparation for next value finding
      root_control = .TRUE.

      ! Redefinition of limits
      epsilon_left = epsilon + 1; epsilon_right = lambda**2

      epsilon_step = (epsilon_right - epsilon_left) / 1E2       ! Epsilon initial step required

      ! Initial values redifinition
      epsilon = epsilon_left                                    ! Epsilon iteration initialization

      psi_k = RK4s_PVI(F, x_left, x_right(n + 1), Psi_condition, dim)  ! Initial conditions for iteration

      psi_left = psi_k(dim +1, 2)                               ! Left psi definition
    
    endif

    ! Number of energies allowed control flow
    if(n .EQ. n_max) EXIT

  end do energy_fishing 
  
  ! -------------------- Exit sequence of program --------------------- !
  !
  ! Escritura de valores obtenidos de auto-estados y auto-valores
  ! respectivamente en archivos para posterior comparación.

  open(UNIT = 1, FILE = 'psi_values_2.txt', STATUS = 'UNKNOWN')

  print_psi: do k = 1, (dim +1)

    ! Print out of value
    write(1,*) psi(k, :)

  end do print_psi

  close(1)

  open(UNIT = 2, FILE = 'epsilon_values_2.txt', STATUS = 'UNKNOWN')
  
  print_epsilon: do k = 1, n_max

    ! Print out of value
    write(2,*) epsilons(k)

  end do print_epsilon

  close(2)

  ! -------------------------- Functions defintions ------------------------------- !
  !
  ! Definiciones de funciones declaradas para el trabajo propuesto anteriormente:
  ! 
  ! - Morse: Función de potencial de morse
  ! - F : Función de campo vectorial que encierra la información del sistema de 
  !       ecuaciones diferenciales equivalente a la ecuación diferencial de segundo
  !       orden.
  ! - RK4s_PVI: Función de iteración Runge-Kutta orden 4 para la aproximación de una
  !             ecuación diferencial ordinaria de primer orden Vectorial (sistema)

  contains

  ! Morse: Morse potential Function
  double precision function morse(x)

    ! Variables definition
    implicit none

    real(8) :: x    ! Position

    ! Potential definition
    morse = (lambda * (1 - exp(-x)) ) ** 2

  end function ! V - Morse potential
  
  ! F: Schrodinger Equation PVI Consideration
  function F(x, Y) result(psi)

    ! Variables definition
    implicit none

    real(8), intent(in) :: x      ! Variable X
    
    double precision, dimension(:), intent(in) :: Y      ! Variable Y

    double precision, allocatable, dimension(:) :: psi     ! Returning of the function 

    integer :: i    ! Component iterator

    ! Initialization of variables
    allocate(psi(size(Y)))

    ! Function definition
    psi = (/ Y(2), (morse(x) - epsilon) * Y(1) /)
    
  end function
  
  ! Runge - Kutta 4 Vectorial Method: PVI with first Taylor step
  function RK4s_PVI(f, t_initial, t_final, y_initial, M) result(RK4)

    ! Variables definition
    implicit none 
    
    double precision, dimension(:), intent(in) :: y_initial          ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final               ! Interval to evaluate

    integer, intent(in) :: M                                ! Number of steps to evaluate (nodes)

    double precision, allocatable, dimension(:, :) :: RK4            ! Matrix RK4(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T                 ! T Component definition of RK4
    
    double precision, allocatable, dimension(:,:) :: Y               ! Y Component definition of RK4

    double precision, allocatable, dimension(:) :: F1, F2, F3, F4    ! Temporal components of summation

    real(8) :: h      ! Delta t: Length of interval to take

    integer :: i, j   ! Iterators for M matrix

    ! Definition of interface for Vectorial function F
    interface

      function f(t, Y) result(X)
        real(8), intent(in) :: t                    ! Variable T
    
        double precision, dimension(:), intent(in) :: Y      ! Variable Y

        double precision, allocatable, dimension(:) :: X     ! Returning of the function 

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

  !Minimum: Minimum value finding over an array
  double precision function minimum(X)

    ! Variable definition
    implicit none

    double precision, dimension(:), intent(in) :: X    ! Array of values

    integer :: k    ! Iterator over values

    double precision :: min_value = 1.

    find_min: do k = 1, size(X)

      if(abs(X(k)) .LT. min_value) then
        min_value = abs(X(k))
      endif

    end do find_min

      minimum = min_value

  end function ! Minimum
  
end program main
