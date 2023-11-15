
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

    ! No implicit definitions
    implicit none

    ! Universal variables definition
  real, parameter :: pi = 4 * ATAN(1.)   ! PI definition  
  real, parameter :: e = 2.7182818285    ! Euler definition

  contains

    ! --------------------------- Definition of subroutines ---------------------------!
    
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

  ! Bisection: Problems with f(x) = 0
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

  end function  ! Bisection


end module CP_methods 
