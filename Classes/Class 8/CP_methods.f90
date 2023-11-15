
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
    
    ! Bisection:
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
    if( f(a) * f(b) .GT. 0 ) then; ERROR STOP " Error found on the arguments"; endif

    ! Method description: Special cases:
    if( f(a) .EQ. 0 ) then; bisect = lInterval; iterations = 0; delta = 0
    else if( f(b) .EQ. 0 ) then; bisect = lInterval; iterations = 0; delta = 0; endif

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
  
      ! Control flow: Exit by definitions
      if(b - a .LT. delta) then; EXIT; endif

    end do bisect_iter
    
    ! Compilation of results
    iterations = iter;    ! Echo of the iterations needed

    delta = epsilon       ! Echo of the error obtained

    bisect = bisect_k     ! Echo of the value obtained

  end function

end module CP_methods 
