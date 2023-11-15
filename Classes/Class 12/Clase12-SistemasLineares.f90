
! ------------------------------------------------------------------------------------ !
! @file     : Clase 12. Sistemas de ecuaciones
! @author   : @ alujan
! @brief    : Método de Gauss-Jordan & Jacobi
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

! todo: Modificar método de Gauss, para ajustar la eliminación Gauss-Jordan.

program gauss_seidel_example ! Import of modules

  !use CP_methods

  ! Declaration of variables
  implicit none
  
  real(8), dimension(4, 4) :: A
  real(8), dimension(4) :: b
  real(8), dimension(4) :: X

  real(8), dimension(4) :: X_guess

  real(8) :: epsilon = 1E-12

  integer :: iterations = 1E3

  integer :: i

  A = reshape((/ 10., 1., 1., 1., 1., 10., 1., 1., 1., 1., 10., 1., 1., 1., 1., 10. /), shape(A), order=[2, 1])

  b = (/ 19., 28., 37., 46. /)

  X_guess = (/ 0., 0., 0., 0. /)

  X = gauss_seidel(A, b, X_guess, epsilon, iterations)

  write(*,*) X

  write(*,*) iterations

  write(*, '(ES15.3E3)') epsilon

  contains

  function gauss_seidel(system, independent, X_guess, epsilon, iterations) result(X)

    implicit none

    ! Declaration of function variables
    real(8), dimension(:,:), intent(in) :: system           ! A-Matrix of system
    real(8), dimension(:), intent(in)   :: independent      ! b-Vector of values

    real(8), dimension(:), intent(inout)   :: X_guess       ! X-Guess Vector

    real(8), intent(inout) :: epsilon      ! Singular matrix parameter
    integer, intent(inout) :: iterations   ! Iterations of the method

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

end program gauss_seidel_example

