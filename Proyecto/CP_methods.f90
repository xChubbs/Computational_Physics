
! ------------------------------------------------------------------------------------ !
! @file     : Computational Physics modules
! @author   : alujan
! @brief    : Definition of module containing the methods of the course
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

! Module definition
module CP_methods

  ! Calling of env
  use iso_fortran_env, only:  int8, int16, int32, int64, real32, real64, &
                              input_unit, output_unit, error_unit

  ! No implicit definitions
  implicit none

  ! Universal variables definition
  real, parameter :: pi = 4 * ATAN(1.)   ! PI definition  
  real, parameter :: e = 2.7182818285    ! Euler definition

  ! Computational Variables definitions
  integer, parameter :: sp        = real32
  integer, parameter :: dp        = real64
  integer, parameter :: stdin     = input_unit
  integer, parameter :: stdout    = output_unit
  integer, parameter :: stderr    = error_unit

  contains

  ! --------------------------- Definition of methods: f(x) = 0 ------------------------ !
    
    ! -------------------- Routines of closed interval -------------------- ! 

    ! Bisection: Problems with f(x) = 0
    real function bisect(f, lInterval, rInterval, delta, iterations)

    ! Input description: Declaration of variables
    implicit none

    real, external :: f ! Function to bisect

    real, intent(in) :: lInterval   ! Left of the interval
    real, intent(in) :: rInterval   ! right of the interval

    real(10), intent(inout) :: delta    ! Error obtained for the method

    integer, intent(inout) :: iterations    ! Number of iterations

    ! Method description: Declaration of variables
    real     :: a = 0, b = 0    ! Handlers of the interval
    real(10) :: epsilon  = 0    ! Error obtained on the method
    real     :: bisect_k = 0    ! Iteration of step k

    integer :: iter = 0   ! Iteration indexer
    
    ! Internal definitions
    a = lInterval
    b = rInterval

    ! Method description: Error handling
    if( f(a) * f(b) .GT. 0 ) then; ERROR STOP " Error found on the arguments: f(a) * f(b) > 0"; endif

    ! Method description: Special cases:
    if( abs(f(a)) .EQ. 0 ) then; bisect = lInterval; iterations = 0; delta = 0
    else if( abs(f(b)) .EQ. 0 ) then; bisect = rInterval; iterations = 0; delta = 0; endif

    ! Method description: Iterations
    bisect_iter: do iter = 1, iterations

      ! Iteration of step k
      bisect_k = (a + b) / 2.

      ! Cases handling: return point
      if(f(bisect_k) .EQ. 0) then; EXIT;    ! Zero found

      else if( f(bisect_k) * f(b) .LT. 0 ) then   ! Left inteval change
        a = bisect_k

      else    ! Right interval change
        b = bisect_k

      endif
  
      epsilon = abs(b - a)    ! Computation of error

      ! Control flow: Exit by definitions
      if(epsilon .LT. delta) then; EXIT;                     ! Steps close
      else if(abs(f(bisect_k)) .LT. delta) then; EXIT; endif   ! Zero close to each other

    end do bisect_iter
    
    ! Compilation of results
    iterations = iter;    ! Echo of the iterations needed

    delta = epsilon       ! Echo of the error obtained

    bisect = bisect_k     ! Echo of the value obtained

  end function  ! Bisection

  ! Regula Falsi: Problems with f(x) = 0
  real function regula_falsi(f, lInterval, rInterval, delta, iterations)

    ! Input description: Declaration of variables
    implicit none

    real, external :: f ! Function to Use Regula Falsi

    real, intent(in) :: lInterval   ! Left of the interval
    real, intent(in) :: rInterval   ! right of the interval

    real(10), intent(inout) :: delta    ! Error obtained for the method

    integer, intent(inout) :: iterations    ! Number of iterations

    ! Method description: Declaration of variables
    real     :: a = 0, b = 0    ! Handlers of the interval
    real(10) :: epsilon  = 0    ! Error obtained on the method
    real     :: rFalsi_k = 0    ! Iteration of step k

    integer :: iter = 0   ! Iteration indexer
    
    ! Internal definitions
    a = lInterval
    b = rInterval

    ! Method description: Error handling
    if( f(a) * f(b) .GT. 0 ) then; ERROR STOP " Error found on the arguments: f(a) * f(b) > 0"; endif

    ! Method description: Special cases:
    if( abs(f(a)) .EQ. 0 ) then; regula_falsi = lInterval; iterations = 0; delta = 0
    else if( abs(f(b)) .EQ. 0 ) then; regula_falsi = rInterval; iterations = 0; delta = 0; endif

    ! Method description: Iterations
    regula_falsi_iter: do iter = 1, iterations

      ! Iteration of step k
      rFalsi_k = b - f(b) * (a - b) / (f(a) - f(b))

      ! Cases handling: return point
      if(f(rFalsi_k) .EQ. 0) then; EXIT;    ! Zero found

      else if( f(rFalsi_k) * f(b) .LT. 0 ) then   ! Left inteval change
        a = rFalsi_k

      else    ! Right interval change
        b = rFalsi_k

      endif

      epsilon = abs(b - a)    ! Computation of error
  
      ! Control flow: Exit by definitions
      if(epsilon .LT. delta) then; EXIT;                        ! Steps close
      else if(abs(f(rFalsi_k)) .LT. delta) then; EXIT; endif   ! Zero close to each other

    end do regula_falsi_iter
    
    ! Compilation of results
    iterations = iter;    ! Echo of the iterations needed

    delta = epsilon       ! Echo of the error obtained

    regula_falsi = rFalsi_k     ! Echo of the value obtained

  end function  ! Regula Falsi

  ! -------------------- Routines of open interval -------------------- ! 

  ! Fix point: Problems of nature x = f(x)
  real function fixpt(f, x_initial, delta, iterations)

    ! Input description: Declaration of variables
    implicit none

    real, external :: f ! Function to Use Fix Point

    real, intent(in) :: x_initial   ! Initial aproximation

    real(10), intent(inout) :: delta    ! Error obtained for the method

    integer, intent(inout) :: iterations    ! Number of iterations

    ! Method description: Declaration of variables
    real     :: x_iter = 0       ! Handler of the value per iteration
    real(10) :: epsilon  = 0    ! Error obtained on the method

    integer :: iter = 0   ! Iteration indexer
    
    ! Internal definitions
    x_iter = x_initial    ! Definition of first value for iteration
    epsilon = delta       ! Definition of initial error

    ! Method description: Iterations
    fixpt_iter: do iter = 0, iterations

      ! Control flow
      if(epsilon .LT. delta) then; EXIT; endif    ! Error flow

      ! Error calculation
      epsilon = abs(f(x_iter) - x_iter)
      
      ! method definition
      x_iter = f(x_iter)

    end do fixpt_iter

    ! Method return sequence
    delta      = epsilon  ! Error obtained
    iterations = iter     ! Last iteration
    fixpt      = x_iter   ! Last iteration done

  end function  ! Fix point

  ! Newton: Problems of f(x) = 0
  real function newton(f, df, x_initial, delta, iterations)
    
    ! Input description: Declaration of variables
    implicit none

    real, external :: f  ! Function to Use Newton
    real, external :: df ! Derivative of the function to use Newton

    real, intent(in) :: x_initial   ! Initial aproximation

    real(10), intent(inout) :: delta    ! Error obtained for the method

    integer, intent(inout) :: iterations    ! Number of iterations

    ! Method description: Declaration of variables
    real     :: x_iter = 0       ! Handler of the value per iteration
    real(10) :: epsilon  = 0    ! Error obtained on the method

    integer :: iter = 0   ! Iteration indexer
    
    ! Internal definitions
    x_iter = x_initial    ! Definition of first value for iteration
    epsilon = f(x_iter)    ! Definition of initial error

    ! Method description: Iterations
    newton_iter: do iter = 0, iterations

      ! Control flow
      if(epsilon .LT. delta) then; EXIT; endif    ! Error flow

      ! Error calculation
      epsilon = abs(f(x_iter) / df(x_iter))

      ! Method definition
      x_iter = x_iter - f(x_iter) / df(x_iter)

    end do newton_iter

    ! Method return sequence
    delta      = epsilon  ! Error obtained
    iterations = iter     ! Number of iterations
    newton     = x_iter   ! Last iteration done

  end function  ! Newton

  ! Newton Modified: Problems of f(x) = 0 with linear convergence
  real function newton_modified(f, df, d2f, x_initial, delta, iterations)
    
    ! Input description: Declaration of variables
    implicit none

    real, external :: f    ! Function to Use Newton
    real, external :: df   ! Derivative of the function to use Newton
    real, external :: d2f  ! Second Derivative of the function to use Newton

    real, intent(in) :: x_initial   ! Initial aproximation

    real(10), intent(inout) :: delta    ! Error obtained for the method

    integer, intent(inout) :: iterations    ! Number of iterations

    ! Method description: Declaration of variables
    real     :: x_iter = 0       ! Handler of the value per iteration
    real(10) :: epsilon  = 0    ! Error obtained on the method

    integer :: iter = 0   ! Iteration indexer
    
    ! Internal definitions
    x_iter = x_initial    ! Definition of first value for iteration
    epsilon = f(x_iter)    ! Definition of initial error

    ! Method description: Iterations
    newton_mod_iter: do iter = 0, iterations

      ! Control flow
      if(epsilon .LT. delta) then; EXIT; endif    ! Error flow

      ! Error calculation
      epsilon = abs(df(x_iter) * f(x_iter) / (df(x_iter)**2. - f(x_iter) * d2f(x_iter)))

      ! Method definition
      x_iter = x_iter - df(x_iter) * f(x_iter) / (df(x_iter)**2. - f(x_iter) * d2f(x_iter))

    end do newton_mod_iter

    ! Method return sequence
    delta           = epsilon  ! Error obtained
    iterations      = iter     ! Number of iterations
    newton_modified = x_iter   ! Last iteration done

  end function  ! Newton Modification

  ! Secant: Problems with f(x) = 0
  real function secant(f, x_prev, x_next, delta, iterations)

    ! Input description: Declaration of variables
    implicit none

    real, external :: f ! Function to bisect

    real, intent(in) :: x_prev   ! Left of the interval
    real, intent(in) :: x_next   ! right of the interval

    real(10), intent(inout) :: delta    ! Error obtained for the method

    integer, intent(inout) :: iterations    ! Number of iterations

    ! Method description: Declaration of variables
    real     :: x0 = 0, x1 = 0    ! Handlers of the interval
    real(10) :: epsilon  = 0    ! Error obtained on the method
    real     :: secant_k = 0    ! Iteration of step k

    integer :: iter = 2   ! Iteration indexer
    
    ! Method description: initialization
    x0 = x_prev; x1 = x_next
          
    ! Method description: Special cases:
    if( abs(f(x0)) .EQ. delta ) then; secant = x0; iterations = 0; delta = 0
    else if( abs(f(x1)) .EQ. delta ) then; secant = x1; iterations = 1; delta = abs(x1 - x0); endif

    ! Method description: Iterations
    secant_iter: do iter = 2, iterations

      ! Iteration of step k
      secant_k = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))
      
      ! Control Flow: definition of next variables
      x0 = x1; x1 = secant_k

      epsilon = abs(x1 - x0)    ! Computation of error

      ! Control flow: Exit by definitions
      if(epsilon .LT. delta) then; EXIT;                 ! Steps close
      else if(abs(f(x1)) .LT. delta) then; EXIT; endif   ! Zero close to each other

    end do secant_iter
    
    ! Compilation of results
    iterations = iter;    ! Echo of the iterations needed

    delta = epsilon       ! Echo of the error obtained

    secant = secant_k     ! Echo of the value obtained

  end function  ! Secant

  ! --------------------------- Definition of methods: Ax = b ------------------------- !

  ! Gaussian Elimination: Exact solution for Ax = b
  function gauss_elimination(system, independent) result(X)

    ! Declaration of variables
    implicit none

    ! Declaration of function variables
    real(8), dimension(:,:), intent(in) :: system           ! A-Matrix of system
    real(8), dimension(:), intent(in)   :: independent      ! b-Vector of values

    real(8), allocatable, dimension(:) :: X  ! X-Vector of variables

    ! Declaration of iner-variables
    real(8), allocatable, dimension(:, :) :: A      ! Extended matrix

    real(8), allocatable, dimension(:) :: temporal  ! Temporal bin of values

    real(8) :: epsilon = 1E-6   ! Singular matrix parameter

    integer :: i, j, k ! Iterators over the indexes
    integer :: dim     ! Dimension of the system

    real(8) :: pivot = 0.       ! Pivot control variable

    ! Initialization of variables
    dim = size(independent)     ! Initilization of dimension on the system

    allocate(X(dim))            ! Allocation of X-vector
    
    allocate(temporal(dim +1))  ! Allocation of temporal bin

    allocate(A(dim, dim + 1))   ! Allocation of Extended System matrix

    A(:, 1:dim)   = system        ! Extended matrix definition: system
    A(:, dim + 1) = independent   ! Extended matrix definition: independent
 
    ! Reduction of the extended Matrix
    matrix_redux: do i = 1, dim

      ! Localization of pivot
      pivot = abs(A(i, i)); k = i   ! First guess on the pivot value

      pivot_search: do j = i+1, dim

        ! Validation of matrix_redux pivot value
        if(abs(A(k, i)) .GT. pivot) then

          pivot = abs(A(i, k)); k = j ! Update on pivot

        endif

      end do pivot_search

      ! Control flow: near singular
      if(pivot .LT. epsilon) then
        ERROR STOP "System near singular, Check your results after continue"

      endif

      ! Pivot row not singular, not in i
      if(.NOT.(k .EQ. i)) then
        temporal = A(i,:)   ! Buffer of the i-row
        A(i,:) = A(k, :)    ! Change of i-row to pivot-row
        A(k, :) = temporal  ! Change of k-row to old i-row

      endif

      ! Resultant rows for elimination
      row_selection: do j = i + 1, dim
        A(j, :) = A(j, :) - A(j, i) / A(i,i) * A(i, :)

      end do row_selection

    end do matrix_redux

    ! Regressive sustitution for solution

    X = A(:, dim+1)   ! Initialization of X values

    regressive_sub: do j = dim, 1, -1

      residual_elimination: do k = j+1, dim

        X(j) = X(j) - A(j,k) * X(k)
      end do residual_elimination

      X(j) = X(j) / A(j,j)  ! Normalization of value
    
    end do regressive_sub
    
    ! Stop sequence of the function
    return

  end function  ! Gaussian Elimination

  ! Gauss-Jordan Elimination: Exact solution for Ax = b
  function gauss_jordan_elimination(system, independent) result(X)

    ! Declaration of variables
    implicit none

    ! Declaration of function variables
    real(8), dimension(:,:), intent(in) :: system           ! A-Matrix of system
    real(8), dimension(:), intent(in)   :: independent      ! b-Vector of values

    real(8), allocatable, dimension(:) :: X  ! X-Vector of variables

    ! Declaration of iner-variables
    real(8), allocatable, dimension(:, :) :: A      ! Extended matrix

    real(8), allocatable, dimension(:) :: temporal  ! Temporal bin of values

    real(8) :: epsilon = 1E-6   ! Singular matrix parameter

    integer :: i, j, k ! Iterators over the indexes
    integer :: dim     ! Dimension of the system

    real(8) :: pivot = 0.       ! Pivot control variable

    ! Initialization of variables
    dim = size(independent)     ! Initilization of dimension on the system

    allocate(X(dim))            ! Allocation of X-vector
    
    allocate(temporal(dim +1))  ! Allocation of temporal bin

    allocate(A(dim, dim + 1))   ! Allocation of Extended System matrix

    A(:, 1:dim)   = system        ! Extended matrix definition: system
    A(:, dim + 1) = independent   ! Extended matrix definition: independent
 
    ! Reduction of the extended Matrix
    matrix_redux: do i = 1, dim

      ! Localization of pivot
      pivot = abs(A(i, i)); k = i   ! First guess on the pivot value

      pivot_search: do j = i+1, dim

        ! Validation of matrix_redux pivot value
        if(abs(A(k, i)) .GT. pivot) then

          pivot = abs(A(i, k)); k = j ! Update on pivot

        endif

      end do pivot_search

      ! Control flow: near singular
      if(pivot .LT. epsilon) then
        ERROR STOP "System near singular, Check your results after continue"

      endif

      ! Pivot row not singular, not in i
      if(.NOT.(k .EQ. i)) then
        temporal = A(i,:)   ! Buffer of the i-row
        A(i,:) = A(k, :)    ! Change of i-row to pivot-row
        A(k, :) = temporal  ! Change of k-row to old i-row

      endif

      A(i, :) = A(i, :) / A(i,i)    ! Normalization of row

      ! Elimination of one-out
      row_selection: do j = 1, dim
        if(j .EQ. i) then; CYCLE; endif

        A(j, :) = A(j, :) - A(j, i) / A(i,i) * A(i, :)

      end do row_selection

    end do matrix_redux

    ! Regressive sustitution for solution

    X = A(:, dim+1)   ! Definition of X values

    ! Stop sequence of the function
    return

  end function  ! Gauss-Jordan  Elimination

  ! Jacobi method: Aproximate solution for Ax = b 
  function jacobi(system, independent, X_guess, epsilon, iterations) result(X)

    implicit none

    ! Declaration of function variables
    real(8), dimension(:,:), intent(in) :: system           ! A-Matrix of system
    real(8), dimension(:), intent(in)   :: independent      ! b-Vector of values

    real(8), dimension(:), intent(inout)   :: X_guess       ! X-Guess Vector

    real(8), intent(inout) :: epsilon      ! Singular matrix parameter
    integer, intent(inout) :: iterations   ! Iterations of the method

    real(8), allocatable, dimension(:, :) :: A      ! Extended matrix

    real(8), allocatable, dimension(:) :: X         ! X-Vector of variables

    ! Declaration of iner-variables
    integer :: i, k    ! Iterators over the indexes
    integer :: dim     ! Dimension of the system

    real(8) :: delta = 0.       ! Inner unit error
    real(8) :: max_delta        ! Inner maximum error

    integer :: iter  = 0   ! Inner iterations of the function

    ! Initialization of variables
    dim = size(independent)     ! Initilization of dimension on the system

    allocate(X(dim))            ! Allocation of X-vector

    allocate(A(dim, dim))       ! Allocation of Dummy matrix

    ! Method definition
	  method_definition: do
		  jacobi_definition: do i = 1, dim
		
		    X(i) = independent(i)   ! Independent value perturbation
		
		    jacobi_peturbation: do k = 1, dim
	
	        if(k .EQ. i) then; CYCLE; endif   ! Cero case
		
		      X(i) = X(i) - A(i, k) * X_guess(k)
		
		  end do jacobi_peturbation
	
	    X(i) = X(i) / A(i,i)  ! Normalization of value
		
		  end do jacobi_definition
	
	    ! Step control flow
	    iter = iter + 1

      ! Error control flow
      max_delta = 0.

	    infinite_norm_error: do k = 1, dim

	      ! Definition of unit error
	      delta = abs(X(k) - X_guess(k))
	
	      ! Definition of inifinite norm error
        if(delta .GT. max_delta) then; max_delta = delta; endif
	
	    end do infinite_norm_error
	
      ! Convergence Criteria
	    if(iter .GT. iterations) then; EXIT; endif  ! Iteration criteria

	    if(max_delta .LT. epsilon) then; EXIT; endif  ! Error criteria
	
	    ! Next guess definition
	    X_guess = X
	
	  end do method_definition

    ! Return sequence of values

    iterations = iter    ! Echo of the iterations needed

    epsilon = delta   ! Echo of the error obtained

    return

  end function ! Jacobi

  ! Gauss-Seidel method: Aproximate progressive solution for Ax = b
  function gauss_seidel(system, independent, X_guess, epsilon, iterations) result(X)

    implicit none

    ! Declaration of function variables
    real(8), dimension(:,:), intent(in) :: system           ! A-Matrix of system
    real(8), dimension(:), intent(in)   :: independent      ! b-Vector of values

    real(8), dimension(:), intent(inout)   :: X_guess       ! X-Guess Vector

    real(8), intent(inout) :: epsilon      ! Singular matrix parameter
    integer, intent(inout) :: iterations   ! Iterations of the method

    real(8), allocatable, dimension(:, :) :: A      ! Extended matrix

    real(8), allocatable, dimension(:) :: X  ! X-Vector of variables

    ! Declaration of iner-variables
    integer :: i, k    ! Iterators over the indexes
    integer :: dim     ! Dimension of the system

    real(8) :: delta = 0.       ! Inner unit error
    real(8) :: max_delta        ! Inner maximum error

    integer :: iter  = 0   ! Inner iterations of the function

    ! Initialization of variables
    dim = size(independent)     ! Initilization of dimension on the system

    allocate(X(dim))            ! Allocation of X-vector

    allocate(A(dim, dim))       ! Allocation of Dummy matrix

    ! Allocation of variables
    A = system        !System allocation: iterable A

    ! Method definition
	  method_definition: do
		  gauss_seidel_definition: do i = 1, dim
		
		    X(i) = independent(i)   ! Independent value perturbation
		
		    gauss_seidel_peturbation: do k = 1, dim
	
	        if(k .EQ. i) then; CYCLE; endif   ! Cero case
		
          if(k .GT. i) then                     ! Upper Triangular case
		        X(i) = X(i) - A(i, k) * X_guess(k)

          else                                  ! Lower Triangular case
            X(i) = X(i) - A(i, k) * X(k)

          endif

		
		  end do gauss_seidel_peturbation
	
	    X(i) = X(i) / A(i,i)  ! Normalization of value
		
		  end do gauss_seidel_definition
	
	    ! Step control flow
	    iter = iter + 1

      ! Error control flow
      max_delta = 0.

	    infinite_norm_error: do k = 1, dim

	      ! Definition of unit error
	      delta = abs(X(k) - X_guess(k))
	
	      ! Definition of inifinite norm error
        if(delta .GT. max_delta) then; max_delta = delta; endif
	
	    end do infinite_norm_error
	
      ! Convergence Criteria
	    if(iter .GT. iterations) then; EXIT; endif  ! Iteration criteria

	    if(max_delta .LT. epsilon) then; EXIT; endif  ! Error criteria
	
	    ! Next guess definition
	    X_guess = X
	
	  end do method_definition

    ! Return sequence of values

    iterations = iter    ! Echo of the iterations needed

    epsilon = delta   ! Echo of the error obtained

    return

  end function ! Gauss-Seidel

  ! --------------------------- Definition of methods: df/dx ------------------------- !

  ! Definition of derivative method
  function derivative(f, x, order, nodes, h) result(dx)

	  ! Definition of inner constants
	  implicit none
	
	  real(8), external :: f           ! Function declaration
	
	  real(8), intent(in) :: x         ! Value to aproximate
	  real(8), intent(in) :: h         ! Length of interval to consider
	
	  integer, intent(in) :: nodes     ! Number of nodes to consider
	  integer, intent(in) :: order     ! Order of derivative to evaluate

    real(8) :: dx                    ! Value of the derivative in x_value
	
	  ! Selection of order
	  order_selection: select case (order)
	
	    ! First order derivative
	    case (1)
	    
	      ! Nodes selection
	      nodes_selection_1st: select case (nodes)
	
          ! Selection of nodes count
	        case (2); go to 12
	        case (3); go to 13
	        case (5); go to 15

          ! Default case: 3 nodes derivative
          case default; go to 13

	      end select nodes_selection_1st
	    
	    ! Second order derivative: 3 nodes derivative
	    case (2)
        
        ! Nodes selection
	      nodes_selection_2nd: select case (nodes)
	
          ! Selection of nodes count
	        case (3); go to 23
	        case (5); go to 25
          
          ! Default case: 4 nodes derivative
          case default; go to 23

	      end select nodes_selection_2nd

	    ! Default case: First order 3 nodes derivative
      case default; go to 13
	
	  end select order_selection
	
	  ! Derivatives definitions: First order
    12 dx = (f(x + h) - f(x)) / h; return                                         ! Foward definition
    13 dx = (f(x + h) - f(x - h)) / (2*h); return                                 ! Centered definition
    15 dx = (f(x - 2*h) - 8*f(x - h) + 8*f(x + h) - f(x + 2*h)) / (12*h); return  ! 2nd order centered

    ! Derivatives definitions: Second order
    23 dx = (f(x + h) - 2*f(x) + f(x - h)) / h**2; return                                          ! centered definition
    25 dx = (-f(x - 2*h) + 16*f(x - h) - 30*f(x) + 16*f(x + h) - f(x + 2*h)) / (12*h**2); return   ! 2nd order centered 

  end function ! Derivative


  ! --------------------------- Definition of methods: Integration ------------------------- !

  ! Integral Trapezoidal rule: Integration over the Trapezoidal rule
  function integral_trapezoidal(f, x_left, x_right, intervals) result(integral)

    ! Definition of variables
    implicit none

    real, external :: f                     ! Function of integrand

    real, intent(in) :: x_left, x_right     ! Integral interval

    integer, intent(in) :: intervals        ! Number of intervals to consider

    real :: integral        ! Numerical integral value

    real(8) :: a, b         ! Inner interval definitions: recursive limits

    real(8) :: h            ! Step of integration

    integer :: i = 0        ! Iterator of the integration rule

    ! Condition of rule
    if( .not.(mod(intervals, 2) .EQ. 0)) then
      ERROR STOP  "The intervals for the trapezoidal rule must be even (multiple of 2)"

    endif

    ! Initialization integration variables
    integral = 0

    h = (x_right - x_left) / intervals

    ! Integration over subintervals: Implicit definition of composite rule
    Integration_sub_intervals: do i = 0, (intervals / 2 - 1)
  
      ! Initialization of interval
      a = x_left + 2*i*h    ! Initialization of left limit

      b = a + 2*h          ! Initialization of right limit

      ! Value of sub-integral
      integral = integral + (f(a) + 2*f(a + h) + f(b))

    end do Integration_sub_intervals

    ! Scale of integral delta x
    integral = h/2. * integral 
    
  end function ! Integral Trapezoidal

  ! Integral Simpson rule: Integration over the Simpson rule
  function integral_simpson(f, x_left, x_right, intervals) result(integral)

    ! Definition of variables
    implicit none

    real, external :: f                     ! Function of integrand

    real, intent(in) :: x_left, x_right     ! Integral interval

    integer, intent(in) :: intervals        ! Number of intervals to consider

    real :: integral        ! Numerical integral value

    real(8) :: a, b         ! Inner interval definitions: recursive limits

    real(8) :: h            ! Step of integration

    integer :: i = 0        ! Iterator of the integration rule

    ! Condition of rule
    if( .not.(mod(intervals, 2) .EQ. 0)) then
      ERROR STOP  "The intervals for the simpson rule must be even (multiple of 2)"

    endif

    ! Initialization integration variables
    integral = 0

    h = (x_right - x_left) / intervals

    ! Integration over subintervals: Implicit definition of composite rule
    Integration_sub_intervals: do i = 0, (intervals / 2 - 1)
  
      ! Initialization of interval
      a = x_left + 2*i*h    ! Initialization of left limit

      b = a + 2*h          ! Initialization of right limit

      ! Value of sub-integral
      integral = integral + (f(a) + 4*f(a + h) + f(b))

    end do Integration_sub_intervals

    ! Scale of integral delta x
    integral = h/3. * integral 
    
  end function ! Integral Trapezoidal

  ! ------------------- Definition of methods: Interpolation p(x) = f(x) -------------------- !

  function lagran_interpolant(X, Y, x_eval) result(y_eval)
	
    ! Definition of variables
    implicit none

    real(8), dimension(:), intent(in) :: X   ! X-Nodes of interpolation 
    real(8), dimension(:), intent(in) :: Y    ! Y-Nodes of interpolation

    real(8), intent(in) :: x_eval             ! X-Value to aproximate

    real(8) :: y_eval     ! Y-Value to aproximate

	  real(8) :: L_i        ! L polynomial aproximated value

    integer :: i, j       ! Method iterators

    ! Method definition
    y_eval = 0.           ! Evaluation of x-value initialization
     
    for_values: do j = 1, size(X)

      ! Determination of value hat
      L_i = 1     ! L aproximation hat initialization

      value_hat: do i = 1, size(X)

        ! Control flow: Zero case
        if(i .EQ. j) then; CYCLE; endif
	
        L_i = L_i * (x_eval - X(i)) / (X(j) - X(i))
	
      end do value_hat
	
      y_eval = y_eval + Y(j) * L_i

    end do for_values

  end function ! Lagran Interpolant

  ! --------------------- Definition of methods: EDO PVI: f(t, y) ---------------------- !

  ! Euler Method: PVI with first Taylor step
  function euler_PVI(f, t_initial, t_final, y_initial, M) result(E)

    ! Variables definition
    implicit none

    real(8), external :: f                          ! Function f(t, y) = dy/dt
    
    real(8), intent(in) :: y_initial                ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final       ! Interval to evaluate

    integer, intent(in) :: M                        ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: E      ! Matrix E(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T, Y      ! T,Y Components definition of E

    real(8) :: h      ! Delta t: Length of interval to take

    integer :: i, j   ! Iterators for M matrix

    ! Initialization of variables
    allocate(E(M + 1, 2))                  ! Allocation of E matrix

    allocate(T(M + 1)); allocate(Y(M + 1)) ! Allocation of components for E Matrix

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      Y(i + 1) = Y(i) + h * f(T(i), Y(i))

    end do explicit_evaluation

    E(:, 1) = T; E(:, 2) = Y    ! Component composition E

  end function ! Euler Method

  ! Euler Vectorial Method: PVI with first Taylor step
  function eulers_PVI(f, t_initial, t_final, y_initial, M) result(E)

    ! Variables definition
    implicit none 
    
    real(8), dimension(:), intent(in) :: y_initial  ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final       ! Interval to evaluate

    integer, intent(in) :: M                        ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: E      ! Matrix E(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T         ! T Component definition of E
    
    real(8), allocatable, dimension(:,:) :: Y       ! Y Component definition of E

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
    allocate(E(M + 1, size(y_initial) + 1))                 ! Allocation of E matrix

    allocate(T(M + 1)); allocate(Y(M + 1, size(y_initial))) ! Allocation of components for E Matrix

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i, :) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      Y(i + 1, :) = Y(i, :) + h * f(T(i), Y(i, :))

    end do explicit_evaluation

    E(:, 1) = T; E(:, 2:) = Y    ! Component composition E

  end function ! Euler Vectorial Method

  ! Runge - Kutta 2 Method: PVI with evaluations on the second Taylor step
  function RK2_PVI(f, t_initial, t_final, y_initial, M) result(RK2)

    ! Variables definition
    implicit none

    real(8), external :: f                          ! Function f(t, y) = dy/dt
    
    real(8), intent(in) :: y_initial                ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final       ! Interval to evaluate

    integer, intent(in) :: M                        ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: RK2    ! Matrix RK2(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T, Y      ! T,Y Components definition of RK2

    real(8) :: F1, F2 ! Temporal terms of the summation

    real(8) :: h      ! Delta t: Length of interval to take

    integer :: i, j   ! Iterators for M matrix

    ! Initialization of variables
    allocate(RK2(M + 1, 2))                ! Allocation of RK2 matrix

    allocate(T(M + 1)); allocate(Y(M + 1)) ! Allocation of components for RK2 Matrix

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      F1 = h * f(T(i) + h, Y(i) + h * f(T(i), Y(i)))

      F2 = h * f(T(i) + h, Y(i) + F1)

      Y(i + 1) = Y(i) + (F1 + F2) / 2

    end do explicit_evaluation

    RK2(:, 1) = T; RK2(:, 2) = Y    ! Component composition RK2

  end function ! Runge - Kutta 2 Method

  ! Runge - Kutta 2 Vectorial Method: PVI with first Taylor step
  function RK2s_PVI(f, t_initial, t_final, y_initial, M) result(RK2)

    ! Variables definition
    implicit none 
    
    real(8), dimension(:), intent(in) :: y_initial  ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final       ! Interval to evaluate

    integer, intent(in) :: M                        ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: RK2    ! Matrix RK2(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T         ! T Component definition of RK2
    
    real(8), allocatable, dimension(:,:) :: Y       ! Y Component definition of RK2

    real(8), allocatable, dimension(:) :: F1, F2    ! Temporal components of summation

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
    allocate(RK2(M + 1, size(y_initial) + 1))                     ! Allocation of RK2 matrix

    allocate(T(M + 1)); allocate(Y(M + 1, size(y_initial)))       ! Allocation of components for RK2 Matrix

    allocate(F1(size(y_initial))); allocate(F2(size(y_initial)))  ! Allocation of temporal terms of summation

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i, :) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      F1 = h * f(T(i) + h, Y(i, :) + h * f(T(i), Y(i, :)))

      F2 = h * f(T(i) + h, Y(i, :) + F1)

      Y(i + 1, :) = Y(i, :) + (F1 + F2) / 2

    end do explicit_evaluation

    RK2(:, 1) = T; RK2(:, 2:) = Y    ! Component composition RK2

  end function ! Runge-Kutta 2 Vectorial Method

  ! Runge - Kutta 4 Method: PVI with evaluations on the second Taylor step
  function RK4_PVI(f, t_initial, t_final, y_initial, M) result(RK4)

    ! Variables definition
    implicit none

    real(8), external :: f                          ! Function f(t, y) = dy/dt
    
    real(8), intent(in) :: y_initial                ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final       ! Interval to evaluate

    integer, intent(in) :: M                        ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: RK4    ! Matrix RK2(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T, Y      ! T,Y Components definition of RK4

    real(8) :: F1, F2, F3, F4 ! Temporal terms of the summation

    real(8) :: h              ! Delta t: Length of interval to take

    integer :: i, j           ! Iterators for M matrix

    ! Initialization of variables
    allocate(RK4(M + 1, 2))                ! Allocation of RK4 matrix

    allocate(T(M + 1)); allocate(Y(M + 1)) ! Allocation of components for RK4 Matrix

    h = (t_final - t_initial) / M          ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      F1 = h * f(T(i), Y(i))
      F2 = h * f(T(i) + h/2, Y(i) + F1/2)
      F3 = h * f(T(i) + h/2, Y(i) + F2/2)
      F4 = h * f(T(i) + h  , Y(i) + F3  )

      Y(i + 1) = Y(i) + (F1 + 2*F2 + 2*F3 + F4) / 6

    end do explicit_evaluation

    RK4(:, 1) = T; RK4(:, 2) = Y    ! Component composition RK4

  end function ! Runge - Kutta 4 Method


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

  ! Linear Shot: Linear shot method to resolve a ODE with dirichlet conditions
  function linshot(D1, D2, t_initial, t_final, y_initial, M) result(LS)

    ! Variables definition
    implicit none 
    
    real(8), dimension(:), intent(in) :: y_initial          ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final               ! Interval to evaluate

    integer, intent(in) :: M                                ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:) :: T                 ! T Component definition of PVI

    real(8), allocatable, dimension(:, :) :: U              ! Frontier dependent PVI solution

    real(8), allocatable, dimension(:, :) :: V              ! Frontier independent PVI perturbation

    real(8), allocatable, dimension(:,:) :: Y               ! Y Component definition of RK4

    real(8), allocatable, dimension(:, :) :: LS             ! Frontier evaluated solution

    real(8), allocatable, dimension(:) :: F1, F2, F3, F4    ! Temporal components of summation

    real(8) :: h      ! Delta t: Length of interval to take

    integer :: i, j   ! Iterators for M matrix

    ! Interface definitions: Vectorial fields functions
    interface

      ! Definition of interface for Vectorial function D1: Initial conditions dependent
      function D1(t, Y) result(X)
        real(8), intent(in) :: t                    ! Variable T
    
        real(8), dimension(:), intent(in) :: Y      ! Variable Y

        real(8), allocatable, dimension(:) :: X     ! Returning of the function 

      end function D1

      function D2(t, Y) result(X)
        real(8), intent(in) :: t                    ! Variable T
    
        real(8), dimension(:), intent(in) :: Y      ! Variable Y

        real(8), allocatable, dimension(:) :: X     ! Returning of the function 

      end function D2

    end interface

    ! Initialization of variables
    allocate(U(M + 1, size(y_initial) + 1))   ! Allocation of U matrix
    allocate(V(M + 1, size(y_initial) + 1))   ! Allocation of U matrix

    allocate(LS(M + 1, 2))                    ! Allocation of U matrix

    allocate(T(M + 1)); allocate(Y(M + 1, size(y_initial)))       ! Allocation of components for RK4 Matrix

    ! Allocation of temporal terms of summation
    allocate(F1(size(y_initial)))
    allocate(F2(size(y_initial)))  
    allocate(F3(size(y_initial)))
    allocate(F4(size(y_initial)))

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Parallel solutions for U & V
    explicit_evaluation: do i = 1, M

      ! First element definition: Common T
      if(i .EQ. 1) then; T(i) = t_initial; endif

      ! Method definition T-step
      T(i + 1) = t_initial + i * h

      ! -------------------- Evaluation of U PVI -------------------- !

      ! First element definition
      if(i .EQ. 1) then; U(i, :) = (/y_initial(1), 0.d0/); endif

      ! Method definition: U evaluation with RK4
      F1 = h * D1(T(i), U(i, :))
      F2 = h * D1(T(i) + h/2, U(i, :) + F1/2)
      F3 = h * D1(T(i) + h/2, U(i, :) + F2/2)
      F4 = h * D1(T(i) + h  , U(i, :) + F3  )

      U(i + 1, :) = U(i, :) + (F1 + 2*F2 + 2*F3 + F4) / 6

      ! -------------------- Evaluation of V PVI -------------------- !

      ! First element definition
      if(i .EQ. 1) then; V(i, :) = (/ 0.d0, 1.d0/); endif

      ! Method definition: V evaluation with RK4
      F1 = h * D2(T(i), V(i, :))
      F2 = h * D2(T(i) + h/2, V(i, :) + F1/2)
      F3 = h * D2(T(i) + h/2, V(i, :) + F2/2)
      F4 = h * D2(T(i) + h  , V(i, :) + F3  )

      V(i + 1, :) = V(i, :) + (F1 + 2*F2 + 2*F3 + F4) / 6

    end do explicit_evaluation

    ! Combination of results
    LS(:, 1) = T    ! Common step definition

    ! Linear perturbation of U: Definition of Linear Shot
    LS(:, 2) = U(:, 1) + (y_initial(2) - U(M+1, 1)) / V(M+1, 1) * V(:, 1)

  end function ! Linear-shot function

end module CP_methods 
