
! ------------------------------------------------------------------------------------ !
! @file     : Fundamentos de Programación
! @author   : @alujan
! @brief    : Lógica & Algoritmos
!
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

! Diseño de algoritmos
! Una estructura básica que puede ayudar para la escritura de buenos algoritmos, puede
! tomar la forma:
!
!   I. Diseño del programa
!     1. Planteamiento del problema que se intenta resolver
!     2. Definición de las entradas y salidas
!     3. Diseño del algoritmo
!         a. Descomposición
!         b. Refinamiento paso a paso
!     4. Conversión del algoritmo en ordenes de FORTRAN
!     5. Prueba de los resultados obtenidos en el programa de FORTRAN
!
!   II. Prueba del programa
!     1. Pruebas unitarias que revisen casos sub tareas.
!     2. Compilación de programas y subtareas.
!     3. Salida alpha a producción
!     4. Salida beta a producción
!         a. Arreglos de bugs menores

! Operadores Lógicos
!
!  - Datos Lógicos: Variables con valor de verdad, que únicamente puede tomar valores
!                   de verdad, como verdadero (.TRUE.) y falso (.FALSE.).
!
! Estos valores pueden o representan los valores de verdad para operadores relacionales,
! y estos pueden tomar. Estos pueden ser:
!
!   - (==) .EQ.
!   - (/=) .NE.
!   - ( >) .GT.
!   - (>=) .GE.
!   - (<=) .LE.
!   - ( <) .LT.
!
! Igualmente es posible emplear operadores combinacionales, donde se comparan valores
! lógicos, y siguen reglas lógicas:
!
!   - (&&) .AND.
!   - (||) .OR.
!   - (==) .EQV.
!   - (/=) .NEQV.
!   - ( ~) .NOT.
!
! Igualmente la estas siguen una jerarquia que toman el siguiente orden:
!
!   1. Todos los relacionales se evaluan de izquierda a derecha.
!   2. Todos los operadores .NOT.
!   3. Se evaluan de izquierda a derecha .AND.
!   4. Todos los operadores .OR. de izquierda a derecha
!   5. Todos los operadores .NEQV. de izquierda a derecha

! IF...ELSE
! Esta instrucción evalua una expresión lógica y genera un flujo según las instrucciones
! dadas. La estructura básica toma la forma:
!
! if (logical_expression_1) then
!   -Code block-
! else if (logical_expression_2) then
!   -Code block-
! else
!   -Code block-
! endif
!
! Esta estructura se puede anidar, permitiendo tener flujos dependientes de flujos
! externos/internos. Para la construcción de estas, se recomienda emplear etiquetas.

! SELECT...CASE
! Se emplea un selector de casos, el cual toma variables de tipo entero, en las cuales
! se selecciona una única sección para una misma variable, y se genera el flujo.
!
! select case (case_expression)
! case (case_selector_1)
!   -Code block-
! case (case_selector_2)
!   -Code block-
! case default
!   -Code block-
! end select
!
! En este selector, tenemos opciones para esta estructura, y estos toman formas:
!
!   - case_value           Se verifica case_value == case_expression
!   - low_value:           Se verifica low_value <= case_espression
!   - :high_value          Se verifica case_expression <= high_value
!   - low_value:high_value Se verifica low_value <= case_expression <= high_value

program actividad_s00

  ! Variables definition
  implicit none   ! All variables must be defined
  real :: x = 0   ! Dummy variable to evaluate
  real :: y = 0   ! Results variable {f(x) = y}

  ! Control of print out & in for the program
  write(*,*) "Calculator for the piece-wise function f(x) defined in class"
  write(*, '(a)', advance = 'no') "Enter the value of x you want to work with x = "
  
  read(*,*) x

  ! Flow operation for the piece-wise function
  Region: if (x <= 0) then
    y = exp(-x / 2.)

  else if (x <= 3.1) then
    y = x**(3./2.)

  else if(x <= 10.3) then
    y = log(x**2) - sin(x)

  else
    y = cosh(x)

  endif Region
  
  ! Control of print out of expressions
  write(*,*) "  The function evaluated on the point you entered is f(x) = ", y
  stop

end program actividad_s00

