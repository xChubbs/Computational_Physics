
! ------------------------------------------------------------------------------------ !
! @file     : Examen 3
! @author   : @alujan
! @brief    : Sistemas de Ecuaciones Lineales
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

! Solución de examen 3: Sistema de ecuaciones lineales

! ------------------------------------------------------------------------------------ !
! @ Consideraciones sobre el archivo problem_matrix.txt
!
! El archivo contiene la matriz aumentada del sistema 3x4 para la consideración del 
! problema, esta consiste en la concatenación de elementos:
!   - A: matriz del problema, contiene la información de la suma de fuerzas para
!        cada uno de los paracaidistas, la ecuación i-esima compromete al cuerpo i-esimo
!   - b: Vector de variables independientes, Valor a igualar el sistema, en especial
!        hace referencia a las fuerzas resultantes que toman valor por el eje de
!        referencia inercial que se mueve con los paracaidistas.
!
! Considerar que la matriz aumentada M = [A | b], esta consiste en:
!
!   Ecuación cuerpo i |  Balance de fuerzas en dirección -y  | Fuerza resultante
!   Ecuación cuerpo 1 |       m_1 * g - c_1 * v - T          | m_1 * a
!   Ecuación cuerpo 2 |     m_2 * g - c_2 * v  + T - R       | m_2 * a
!   Ecuación cuerpo 3 |       m_3 * g + R - c_3 * v          | m_3 * a
!
! Como a hace parte de las variables que debemos considerar, vamos a reorganizar el
! para unir los ejes con una fuerza resultante que conozcamos:
!
!   Ecuación cuerpo i |   Balance de fuerzas en dirección -y   | Fuerza resultante
!   Ecuación cuerpo 1 |             - m_1 * a - T              | - m_1 * g + c_1 * v
!   Ecuación cuerpo 2 |           - m_2 * a + T - R            | - m_2 * g + c_2 * v
!   Ecuación cuerpo 3 |             - m_3 * a + R              | - m_3 * g + c_3 * v
!
! Considerando el sistema de ecuaciones con el plano inercial c * v - m * g conocido, 
! podemos definir entonces la matriz a del balance de fuerzas (problem_matrix.txt) como:
!
!   ////// |     a        T       R     |         b
!   Fila 1 |  - m_1      - 1      0     | c_1 * v - m_1 * g
!   Fila 2 |  - m_2       1      - 1    | c_2 * v - m_2 * g
!   Fila 3 |  - m_3       0       1     | c_3 * v - m_3 * g
!
! Calculando los valores que hallamos, la matriz se define como:
!
!   ////// |     a        T       R     |   b
!   Fila 1 |   - 70      - 1      0     | - 636
!   Fila 2 |   - 60       1      - 1    | - 518
!   Fila 3 |   - 40       0       1     | - 307
!
! Considerando la naturaleza de la entrada y los archivos, definimos los contenidos como:
!
!                                 -70.  , -1. , 0.  , -636.
!                                 -60. , 1.  , -1. , -518.
!                                 -40. , 0.  ,  1. , -307.

! ------------------------------------------------------------------------------------ !
! @ Consideraciones sobre el programa
!
! En el programa se sintetiza la aproximación a la solución del problema ejemplo, se
! intenta dar una aproximación al proceso realizado.
!
! ------------------------------------------------------------------------------------ !

! Definición del programa principal
program main

  ! Variables definition
  implicit none

  real(8), dimension(3, 4) :: M         ! Extended matrix of the system

  real(8), dimension(3, 3) :: A         ! Coeficients Matrix
  real(8), dimension(3)    :: b         ! Independent values vector

  real(8), dimension(3) :: X_exact      ! Exact Variables vector:      X = [a, T, R]
  real(8), dimension(3) :: X_aprox      ! Aproximate Variables vector: X = [a, T, R]

  real(8), dimension(3) :: X_initial    ! Initial aproximation for the system

  real(8) :: epsilon_display, epsilon = 1E-12            ! Tolerance of error permitted

  integer :: iterations_display, iterations = 1E3           ! Max iterations permitted

  integer :: i = 0, status = 0    ! Lecture from archive control flow

  ! -------------------------- Initialization of variables ---------------------------- !

  ! Saving on the reference values to display
  epsilon_display = epsilon; iterations_display = iterations

  X_initial = (/ 0., 0., 0./)   ! First guess null variables

  ! Reading of the first matrix
  open(UNIT=1, FILE= "problem_matrix.txt", STATUS= 'OLD')   ! Open of file with matrix M

  ! Reading of the matrix by row
  extended_matrix_read: do i = 1, 3

    ! Control flow of status
    if(status .LT. 0) then; EXIT; endif

    read(1, *, IOSTAT = status) M(i, :)   ! Reading of entire row 

  end do extended_matrix_read

  close(1)          ! Close of the first unit

  ! Separation of matrices
  extended_system_reading: do i = 1, 3

    ! Reading of system coeficients
    A(i, :) = M(i, 1:3)

    ! Reading of system independent
    b(i) = M(i, 4)

  end do extended_system_reading

  ! -------------------------------- Call of functions -------------------------------- !

  ! Call of Gauss-Jordan Elimination
  X_exact = gauss_jordan_elimination(A, b)

  ! Call of Gauss-Seidel Aproximation
  X_aprox = gauss_seidel(A, b, X_initial, epsilon, iterations)

  ! ----------------------------- Ending sequence of program ------------------------------- !

  ! Welcome sequence
  write(*,*) "Hello!, this program was made by @alujan"

  ! Instructions of the system developed
  write(*,*) "This program is made to solve the Aplication problem proposed on the exam"
  write(*,*) "first let's take a look on the equations developed for each body: "
  write(*,*)  ! White space
  write(*,*) "  Ecuación cuerpo i |  Balance de fuerzas en dirección -y  | Fuerza resultante"
  write(*,*) "  Ecuación cuerpo 1 |       m_1 * g - c_1 * v - T          | m_1 * a"
  write(*,*) "  Ecuación cuerpo 2 |     m_2 * g - c_2 * v  + T - R       | m_2 * a"
  write(*,*) "  Ecuación cuerpo 3 |       m_3 * g + R - c_3 * v          | m_3 * a"
  write(*,*)  ! White space
  write(*,*) "Using this system, the proposed approach is to separate the known values from"
  write(*,*) "the unknown variables, organizing by each variable to find per column:"
  write(*,*)  ! White space
  write(*,*) "  ////// |     a        T       R     |         b"
  write(*,*) "  Fila 1 |  - m_1      - 1      0     | c_1 * v - m_1 * g"
  write(*,*) "  Fila 2 |  - m_2       1      - 1    | c_2 * v - m_2 * g"
  write(*,*) "  Fila 3 |  - m_3       0       1     | c_3 * v - m_3 * g"
  write(*,*)  ! White space
  write(*,*) "The resultant coeficient matrix is proposed to be consignated on the archive"
  write(*,*) "problem_matrix.txt, let's take a look on the archive contents"

  M_matrix_display: do i = 1, 3
    
    write(*,*) M(i, :)    ! Display of M contents

  end do M_matrix_display

  write(*,*)  ! White space
  write(*,*) "This matrix it's the extended system matrix, where M = [A | b], let's separate"
  write(*,*) "the contents to use the functions defined"
  write(*,*) "  - A coeficients matrix definition:"
  
  A_matrix_display: do i = 1, 3
    
    write(*,*) A(i, :)    ! Display of A contents

  end do A_matrix_display

  write(*,*)  ! White space
  write(*,*) "  - b independent vector definition:"
  write(*,*) (b(i), i = 1, 3)
  write(*,*)  ! White space
  write(*,*) "With this out of the way, now it's time to check the values obtained on the methods"
  write(*,*) ">> The use of Gauss-Jordan elimination was proposed, the coeficients obtained where:"
  write(*,*) (X_exact(i), i = 1, 3)
  write(*,*)  ! White space
  write(*,*) "it's important to note that for each entry the value was:"
  write(*,*) "  - Acceleration a: ", X_exact(1)
  write(*,*) "  - Tension T: ",      X_exact(2)
  write(*,*) "  - Tension R: ",      X_exact(3)
  write(*,*)  ! White space
  write(*,*) ">> In the other hand using the Gauss-Seidel method give us a very good aproximation"
  write(*,*) "   where we used:"
  write(*,'(a)', advance='no') "     - epsilon (Tolerance of error): "
  write(*,'(ES15.7)') epsilon_display
  write(*,*) "     - max_iter (Maximum of iterations permitted): ", iterations_display
  write(*,*)  ! White space
  write(*,*) "   with this in mind, we obtained:"
  write(*,'(a)', advance='no') "     - X vector of aproximatted values:"
  write(*,*) X_aprox
  write(*,*) "     - iterations needed: ", iterations 
  write(*,'(a)', advance='no') "     - Error estimated for aproximation: "
  write(*,'(ES15.7)') epsilon
  write(*,*)  ! White space
  write(*,*) "Finally for convenience, I created the archives solution_approximated.txt, and"
  write(*,*) "solution_exact.txt to check the values obtained for each variable"

  ! Creation of auxiliar archives

  ! Writing of the exact solution archive
  open(UNIT=2, FILE= "solution_exact.txt", STATUS= 'NEW')   ! Open of file with matrix M

  write(2, *) "Remember the order of the solution vector:"
  write(2, *) "  X = [ a : acceleration, T : Tension , R : Tension]"
  write(2,*)  ! White space

  ! writing of the solution vector
  solution_exact_write: do i = 1, 3

    ! Control flow of status
    if(status .LT. 0) then; EXIT; endif

    write(2, *, IOSTAT = status) X_exact(i)   ! writing of variable

  end do solution_exact_write

  close(2)          ! Close of the first created unit (2) unit

  ! Writing of the exact solution archive
  open(UNIT=3, FILE= "solution_approximated.txt", STATUS= 'NEW')   ! Open of file with matrix M

  write(3, *) "Remember the order of the solution vector:"
  write(3, *) "  X = [ a : acceleration, T : Tension , R : Tension]"
  write(3,*)  ! White space
  write(3,*) "For this aproximation we obtained"
  write(3,*) "  - iterations needed: ", iterations 
  write(3,'(a)', advance='no') "  - Error estimated for aproximation: "
  write(3,'(ES15.7)') epsilon
  write(3,*)  ! White space

  ! writing of the solution vector
  solution_approximated_write: do i = 1, 3

    ! Control flow of status
    if(status .LT. 0) then; EXIT; endif

    write(3, *, IOSTAT = status) X_aprox(i)   ! writing of variable

  end do solution_approximated_write

  close(3)          ! Close of the first created unit (2) unit

  ! ------------------------- Contents definitions of program ------------------------- !

  ! Functions definitions
  contains

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

  ! Gauss-Seidel method: Aproximate progressive solution for Ax = b
  function gauss_seidel(system, independent, X_guess, epsilon, iterations) result(X)

    ! Definition of variables
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
    A = system        ! System allocation: A matrix

    ! Method definition
	  method_definition: do
		  gauss_seidel_definition: do i = 1, dim
		
		    X(i) = independent(i)   ! Independent value perturbation
		
		    gauss_seidel_peturbation: do k = 1, dim
	
	        if(k .EQ. i) then; CYCLE; endif       ! Cero case
		
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

end program main

