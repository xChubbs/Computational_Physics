
! ------------------------------------------------------------------------------------ !
! @file     : Fundamentos de Programación
! @author   : @alujan
! @brief    : Arreglos bidimensionales & Subrutinas
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

! Arreglos bidimensionales
!
! Podemos definir arreglos multidimensionales, extendiendo la definición de dimensiones
! analogo a como se realiza una definición unidimensional, pero extendiendo las 
! indexaciones, como se muestra por ejemplo:
!
!           TYPE, DIMENSION(1-INIT : 1-END, 2-INIT : 2-END, ...) var_name
!
! La inicialización se puede realizar como se realizó anteriormente con las matrices
! unidimensionales, empleando:
!     - Asignación con DO anidados.
!     - Declaración empleando RESHAPE, que modifica la forma del arreglo.
!       Tomar en cuenta que el comando RESHAPE, genera la definición verticalmente.

! Operaciones con Subconjuntos
! Los arreglos permite ejecutar operaciones aritmeticas conjuntas (element-wise), si
! son condormables (es un arreglo real / entero).
! Y similar que programas como MATLAB, permite hacer la indexación tomando:
!
!                var_name(i, j) -> Elemento_i_j de varn_name
! 
! Funciones propias/intrinsicas propias de los arreglos son:
!       - SHAPE : Forma del arreglo
!       - SIZE : Tamaño, número de elementos del arreglo
!       - LBOUND : Indice inferior, indice minimo.
!       - UBOUND : Indice superior, máximo indice.
!
! Por otro lado tenemos funciones intrinsicas transformacionales, que tienen uno o más
! arreglos como argumentos. Estos involucran:
!       - DOT_PRODUCT(VECTOR_A,VECTOR_B)
!       - MATMUL(MATRIZ_A,MATRIZ_B)
!       - RESHAPE(ARREGLO_FUENTE,ARREGLO1D_FORMA)

! Arreglos dinamicos.
! Arreglos que permiten generar asignación dinamica de memoria, y permite modifcar,
! rapidamente los tamaños de un arreglo. Estos se definen:
!
!            TYPE, ALLOCATABLE, DIMENSION(:) :: array_name
!
! Para la determinación de las formas de los arreglos dinamicos, es necesario emplear
! las funciones:
!       - ALLOCATE(array1, array2, ..., STAT=status) -> Asignación satisfactoria es
!                                                       STAT=0
!       - DEALLOCATE(array1, array2, ..., STAT=status) -> Liberar memoria de datos.

! program MatrixMultiplication
! 
!   ! Definition of variables
!   real, allocatable, dimension(:,:) :: matrix_1        ! Matrix 1
!   real, allocatable, dimension(:,:) :: matrix_2        ! Matrix 2
!   real, allocatable, dimension(:,:) :: matrix_result   ! Matrix Result
! 
!   integer :: index_0          = 0    ! Index 0: Temporal index to store
!   integer :: index_1          = 0    ! Index 1: Row index
!   integer :: index_2          = 0    ! Index 2: Column index
!   integer :: index_validation = 0    ! Index Validation for multiplication
! 
!   integer :: i, j     ! Indexes for the matrix
! 
!   ! Welcome sequence of the program
!   write(*,*) "Program calculator for multiplication of 2 matrixes"
!   write(*,*) "Enter the dimensions for each matrix, in orden: rows, columns"
!   
!   ! Input of variables
!   write(*, '(a)', advance='no') "Enter the shape of the first matrix: "
!   read(*,*) index_1, index_2
! 
!   allocate(matrix_1(index_1, index_2))  ! Allocation of the first matrix
! 
!   index_0 = index_1             ! Temporal first index storage
!   index_validation = index_2    ! Temporal index validation
! 
!   write(*,*) ""
!   write(*,*) "Enter the positions of the matrix:"
!   write(*,*) ""
! 
!   ! Writing of the matrix, row-wise
!   do i = 1, index_1
! 
!     ! Lecture of values per row
!     write(*,*) "Elements in row: ", i
!     read(*,*) (matrix_1(i, j), j = 1, index_2)
! 
!   end do
!   
!   write(*, '(a)', advance='no') "Enter the shape of the second matrix: "
!   read(*,*) index_1, index_2
!   
!   allocate(matrix_2(index_1, index_2))  ! Allocation of the second matrix
! 
!   ! Conditions of matrix multiplication
!   if(index_validation .NE. index_1) then
! 
!     ! Stop conditions for error in validation
!     write(*,*) "  @Error"
!     write(*,*) "The dimensions input are not valid, remember the conditions"
!     write(*,*) "Matrix multiplication."
! 
!     stop
!   endif
! 
!   write(*,*) ""
!   write(*,*) "Enter the positions of the matrix:"
!   write(*,*) ""
! 
!   ! Writing of the matrix, row-wise
!   do i = 1, index_1
! 
!     ! Lecture of values per row
!     write(*,*) "Elements in row: ", i
!     read(*,*) (matrix_2(i, j), j = 1, index_2)
! 
!   end do
! 
!   ! Result of the matrix multiplication
!   allocate(matrix_result(index_0, index_2))
! 
!   001 matrix_result = MATMUL(matrix_1, matrix_2)
! 
!   ! End sequence
!   write(*,*) "The multiplication between the matrices equals to the matrix:"
! 
!   ! Writing of the matrix row-wise
!   do i = 1, index_0
!     write(*,*) (matrix_result(i, j), j = 1, index_2)
!   end do
!   
! end program MatrixMultiplication

! Procedimientos y Subrutinas
!
! Procedimientos: Fragmentos de código fuente, los cuales permite aislar tareas
! y luego ser unidos para la creación de un programa robusto.
!
! Principalmente esto permite generar secciones de código reusables, y genera
! independencia entre tareas.
!
! Para la definición de una subrutina, se emplea el llamado y la definicion:
!
!   SUBROUTINE subroutine_name(argument_list):
!        ...                                      CALL subroutine_name(argument_list)
!   END SUBROUTINE subroutine_name
!
! Para la definición de la subrutina, es necesario realizar atributos INTENT, como se
! muestra a continuación:
!
!   REAL, INTENT(IN)  :: input_1, input_2, ...  >> Variable de entrada
!   REAL, INTENT(OUT) :: output_1, output_2 ... >> Variable de salida
!
!   REAL, INTENT(IN OUT) :: output_1, output_2 ... >> Variable de mutable.
   
! Modulos
!
! Programa unidad con definiciones y valores iniciales en un conjunto de entrada para
! compartición de datos para compartir con otros programas en la unidad.
!
!     MODULE module_name
!       ...
!       parameters definitions
!       variable definitions
!       ...
!       CONTAINS
!         ...
!         subroutines definitions
!         ...
!     END MODULE module_name
!
! Y para realizar el llamado de las modulos, se requiere emplear el método:
!     
!     ...
!     USE module_name
!     ...

! Funciones
!
! Estas se declaran siguiendo la sintaxis:
!
!     TYPE, FUNCTION function_name(argument_list)
!       ...
!       Declaration of variables
!       ...
!       function_name = expression
!     END FUNCTION function_name
!
! Debemos tener en cuenta, que la ejecución de la función finaliza al alcanzar la 
! linea donde se iguala la función a una variable. En especial esta estructura se
! basa en un subprograma que retorna una variable que tiene la misma nombre de la
! función.

! Subprogramas como argumentos
! Los subprogramas pueden colocarse como argumentos, solo si son declarados como
! externos en el procedimiento involucrado. en especial se puede verificar:
!
!   REAL, EXTERNAL :: function_1
!   REAL, EXTERNAL :: function_2
!   
!   CALL subroutine_name(function_1, argument_list)
!   CALL subroutine_name(function_2, argument_list)
!   ...
!   SUBROUTINE subroutine_name()
!     ...
!   END SUBROUTINE subroutine_name

program functionComposition

  ! Variables definition
  implicit none
  real, external :: f, g, h  ! Composing functions

  real :: x     ! Dummy variable to compose

  integer :: selector_1 ! Selector for function f
  integer :: selector_2 ! Selector for function g
  integer :: selector_3 ! Selector for function h


  ! Initialization sequence
  write(*,*) "Calculator for the composition of 3 functions: f(x), g(x), h(x)"

  ! Composition of functions
  write(*,*) "Let's compose our functions from the following list:"
  write(*,*) "  (1) SIN(x)"
  write(*,*) "  (2) EXP(x)"
  write(*,*) "  (3) LOG(x)"
  write(*,*) "  (4) TAN(x)"

  write(*,'(a)', advance = 'no') "Enter a number to select the form of the function f(x): "
  read(*,*) selector_1

  if((selector_1 .LT. 1) .or. (selector_1 .GT. 4)) then

    write(*,*) "Enter a valid selection for the function f(x)"
  endif

  write(*,'(a)', advance = 'no') "Enter a number to select the form of the function g(x): "
  read(*,*) selector_2

  if((selector_2 .LT. 1) .or. (selector_2 .GT. 4)) then

    write(*,*) "Enter a valid selection for the function g(x)"
  endif


  write(*,'(a)', advance = 'no') "Enter a number to select the form of the function h(x): "
  read(*,*) selector_3
  
  if((selector_3 .LT. 1) .or. (selector_3 .GT. 4)) then

    write(*,*) "Enter a valid selection for the function h(x)"
  endif

  ! Enter a value to evaluate the composition
  write(*, '(a)', advance='no') "Enter a value for x to evaluate: x = "
  read(*,*) x

  ! Secuencial composition of functions
  write(*,*) "The value obtained for h(x) is:       ", h(x, selector_3)
  write(*,*) "The value obtained for g(h(x)) is:    ", g(h(x, selector_3), selector_2)
  write(*,*) "The value obtained for f(g(h(x))) is: ", f(g(h(x, selector_3), selector_2), selector_1)

end program functionComposition

! Function definitions
real function f(x, selector)

  ! Variable definitions
  implicit none

  real, intent(in) :: x         ! Input variable
  integer, intent(in) :: selector  ! Selector of function

  select case(selector)

  case(1)
    f = sin(x)

  case(2)
    f = exp(x)

  case(3)
    f = log(x)

  case(4)
    f = tan(x)

  case default
    f = 0
  end select

end function

real function g(x, selector)

  ! Variable definitions
  implicit none

  real, intent(in) :: x         ! Input variable
  integer, intent(in) :: selector  ! Selector of function

  select case(selector)

  case(1)
    g = sin(x)

  case(2)
    g = exp(x)

  case(3)
    g = log(x)

  case(4)
    g = tan(x)

  case default
    g = 0
  end select

end function

real function h(x, selector)

  ! Variable definitions
  implicit none

  real, intent(in) :: x         ! Input variable
  integer, intent(in) :: selector  ! Selector of function

  select case(selector)

  case(1)
    h = sin(x)

  case(2)
    h = exp(x)

  case(3)
    h = log(x)

  case(4)
    h = tan(x)

  case default
    h = 0
  end select

end function

