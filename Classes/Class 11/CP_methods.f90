
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


end module CP_methods 
