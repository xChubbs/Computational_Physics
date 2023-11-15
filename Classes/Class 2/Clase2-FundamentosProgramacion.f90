
! ------------------------------------------------------------------------------------ !
! @file     : Clase 2
! @author   : alujan
! @brief    : Fundamentos de Programación
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

! Este lenguaje está orientado a programación secuencial, por lo cual, la estructura
! básica de un programa toma la forma:
!
!program ejemplo2
!  ! Esto es una declaración, diferente de una asignación
!  integer :: i, j, k
!
!  ! Esto es un comentario
!  write(*,*) "Ingrese 2 números enteros: "
!
!  read(*,*) i,j !Lectura de valores: Toma entrada estricta: i,j
!
!  ! Definimos una operación larga
!  k = 2*i +3*j -2*i**3 -10*j**3 +32*i**4 +5*j**5 +3**2 -2**5 &
!          +34*i**2
!
!  ! Con el simbolo & indica la continuación en una nueva linea.
!
!  999 write(*,*) "Resultado = ", z !Esta linea se encuentra etiquetada = 999
!
! Secuencia de Finalización
!
!stop
!end program ejemplo2

! Finalmente FORTRAN cuenta con:
!   - Palabras reservadas, como todas las discriminadas durante la escritura
!     del programa
!   - Constantes: Mayusculas como PI
!   - Varianles: Minusculas, y maneables, discriminadas y decalaradas.
!   - Nombres de programa y procedimientos.

! Finalmente podemos denotar que FORTRAN cuenta con datos:
!   - INTEGER: Enteros
!   - REAL: Reales
!   - COMPLEX: Complejos
!   - LOGIC: Booleanos
!   - CHARACTER: Caracter

! Presición y rango
! Podemos representar según los bits requeridos los números de manera simetrica
! cuando la asignación es automatica, se realiza con el espacio minimo.
!   - Enteros: {-2**(n-1), 2**(n+1)}
!   - Reales: Ya que cuenta con valores enteros y decimales, requere:
!     - Mantissa: 24 bits, así -> {-2**23, +2**23}
!     - exponente: 8 bits que toma 2^exponente, así -> {-2**127, 2**127}
!                   finalmente se espera tener una precisión de 7 decimas.

! Declaración Explicita & Implicita
! Se considera Explcita al exponer el tipo de variable, y longuitudes, así:
!
!   INTEGER :: i, j, k
!   REAL :: tiempo
!   CHARACTER(n) :: resultados, mensaje
!   CHARACTER(len = 9) :: resultado
!
! Particularmente se refieren las variables de no ser declaradas, las variables i, j, k
! se considera entera y el resto real.

! Constantes
! Las constantes pueden tener tipo, se puede tipar. Por lo que se puede tomar:
!
! REAL, PARAMETER :: PI = 3.141593
!
! Estas constantes no pueden ser modificadas en ningún momento del programa, y conserva la
! consistencia del modelo.

! Consecuencia del tipado en la aritmetica
! Para enteros, se esperan salidas enteras, por lo cual, es tener presente que todo lo representa
! en un conjunto cerrado, la suma de 2 enteros expone un entero, por lo que la división resulta en 
! truncamiento.
! Por otro lado un número real, puede presentar resultados ligeramente diferentes.
!
!                      3. * (1. / 3.) != 1    ,   2. * (1. / 2.) = 2
!
! El problema se presenta con números irracionales.

! La jerarquia de operaciones se realiza:
!   1. Primero se realizan parentesis.
!   2. Se evaluan potencias de derecha a izquierda
!   3. Se realiza multiplicación y división de izquierda a derecha
!   4. La suma y diferencia se realizan de izquierda a derecha.

! Funciones intrinsicas.
! Funciones fundamentales / propias de FORTRAN. Sigue la sintaxis sencilla:
!
! hypotenuse = side2 / SIN(theta) : SIN(theta) función propia.
!
! Instrucciones de entrada y salida.
! Se tienen las funciones como se definen:
!   - read(Unidad de entrada: default = *, Formato : default = *) lista - variables - entrada
!   - write(Unidad de salida: default = *, Formato: default = *) lista - variables - salida
!
! Nota: Importante, se requere siempre iniciar variables, antes de ser usadas.

! IMPLICIT NONE
! Al incluirse la directiva, cualquier variable no definida explicitamente se considera como error.
! muy similar al manejo que se tiene cotidiando en otros lenguajes. Se recomienda en el uso de
! de la inicialización del programa.

! Realizar tarea <- Para la proxima sesión.
!
! Escribir un programa que permita despliegue en consola las dos raíces de una ecuación cuadrática
! de la forma ax2 + bx + c = 0.
! Entradas: a, b y c.
! Salidas: x1 y x2.

program tarea_S00

  ! Declaration of variables
  implicit none

  real :: a = 0, b = 0, c = 0 ! Coeficients of the variables
  real :: det = 0             ! determinant of the equation
  complex :: c1 = 0, c2 = 0   ! Complex roots found
  real :: r1 = 0, r2 = 0      ! Real roots found

  ! Welcome state
  write(*,*) "@alujan"
  write(*,*) "Solución tarea número 0."
  write(*,*) ""

  ! Write instruction needed
  write(*,*) "Enter the your cuadratic equation in the form:"
  write(*,*) "  ax^2 + bx + c = 0"

  ! Write per line the values needed
  write(*, '(a)', advance='no') "Value of a = "
  read(*,*) a ! Value of a

  write(*, '(a)', advance='no') "Value of b = "
  read(*,*) b ! Value of b

  write(*, '(a)', advance='no') "Value of c = "
  read(*,*) c ! Value of c

  ! Finding of the values
  det = b**2 - 4 * a * c

  ! Conditional flow of work
  if (a .eq. 0) then
    ! This is default test case
    r1 = -c / b
    r2 = r1

    ! Print out of values
    write(*,*) "This equation represents a line!"
    write(*,*) "The unique root it's ", r1
    write(*,*) "You almost got me!"
    write(*,*) ""
  
  else if (det .eq. 0) then
    ! There is just one unique real root
    r1 = -b / (2 * a)
    r2 = r1

    ! Print out of values
    write(*,*) "There's just 1 root in the equation!"
    write(*,*) "The unique root it's ", r1

  else
    ! Flow to find out the nature of the root
    if (det .lt. 0) then
      ! There're 2 complex roots
      c1 = (-b + sqrt(complex(det, 0.))) / (2 * a)
      c2 = (-b - sqrt(complex(det, 0.))) / (2 * a)

      ! Print out of values
      write(*,*) "There're 2 complex roots in the equation!"
      write(*,*) "The first root it's ", c1
      write(*,*) "The second root it's ", c2

    else
      ! There're 2 real roots
      r1 = (-b + sqrt(det)) / (2. * a)
      r2 = (-b - sqrt(det)) / (2. * a)

      ! Print out of values
      write(*,*) "There're 2 real roots in the equation!"
      write(*,*) "The first root it's ", r1
      write(*,*) "The second root it's ", r2

    endif

  endif

  ! Print out of determinant
  write(*,*) "Remember we found the  determinant it's ", det

  ! Secuencia de Finalización
  stop
  
end program tarea_S00

