
! ------------------------------------------------------------------------------------ !
! @file     : Fundamentos de programación
! @author   : alujan
! @brief    : Bucles y ciclos
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

! Estructura DO
! Esctructura que realiza una tarea,  que permite tener una sub estructura de selección,
! que permite abortar la tarea
!
!     DO ...                  do while(logical_expression)
!       <Code Block>    ->      <Code Block>
!     END DO                  end do
!
! Igualmente esta tarea se puede asemejar a un for, empleando un contador, el cual controla
! el número de iteraciones que se realiza, empleando secuencias:
!
!           iteration = start, end, increment
!
! Fortran presenta 2 instrucciones de flujo, que funcionan como se presentan:
!
!   - EXIT : Similar a un STOP que para el flujo del ciclo y continua en la operación de 
!             mayor jerarquia.
!   - CYCLE : Similar a un SKIP que salta la iteración o skippea la iteración actual.
!
! Esta estructura se permite anidar, por lo cual el algoritmo de menor jerarquia se realiza
! primero, y en especial, por cada iteración e mayor jerarquia.

! Depuración manual
!
! - Se recomienda antes de realizar el programa, proponer un diagrama de flujo, en especial, 
!   aquellos que involucren estructuras repetitivas y selectivas.
! - Implementar WRITE, no PRINT
! - Chequear condiciones logicas.
! - Comparar números reales usando tolerancias, como se propone:
!        IF( x .EQ. 1.0 ) THEN ... -> IF( abs(x - 1.0) .LE. 1E-5 ) THEN ...

! Instrucciones con CHARACTERS
! Los caracteres presenta una declaración de lista, por lo cual es necesario generar de la
! forma:
!   
!     CHARACTER(LEN = INTEGER_VALUE) :: variable
!
! Con estos es posible realizar indexado, o sea, crear substings. Y naturalmente se puede
! realizar concatenación.
!
!     character-name(init_index:index_end:increment)
!
! Por otro lado, para la realización de concatenación, se requiere seguir la sintaxis:
!
!     substring_1 // substring_2 // ...
!
! FORTRAN igualmente posee tambien operaciones relacionales con caracteres, en especial,
! se puede realizar comparaciones de valores ASCII entre caracteres. Igualmente presenta
! operaciones intrínsecas con caracteres, como son:
!
!   - ACHAR(integer_value) : Returns the ASCII value in the position integer_value
!   - IACHAR(character_value) : Returns the ASCII position of the ASCII value.
!   - LEN(string_list) : Length of the string in characters
!   - LEN_TRIM(string_list) : Returns the length of the string, exluiding blanks
!   - TRIM(string_list) : returns the string with trailing blaks removed.

! ACTIVIDAD

program actividad_01
  
  ! Declaration of variables
  implicit none   ! Not implicit variables defined

  real :: x = 0   ! nth-Component of the first vector X
  real :: y = 0   ! nth-Component of the second vector Y
  real :: S = 0   ! Acummulative sum of values

  integer :: index = 1   ! indexer od the dimentionality of the vector
  integer :: n = 0        ! Dimentionality of the vectors

  ! Definition by user of the lenth of operation
  write(*, *) "Calculator for the dot product between 2 vectors of dimentionality n"
  write(*, '(a)', advance = 'no') "Enter the dimentionality of the vectors you want to operate: "
  
  read(*, *) n

  write(*,*) ""
  write(*,*) "The dimention of the vectors is: ", n, "Enter the positions requested"
  write(*,*) "Consider the vector 1 it's X and the vector 2 it's Y"
  write(*,*) ""

  ! Flux of the program
  ndimentionality : do index = 1, n

    ! Operation position-wise
    write(*, '(a)', advance = 'no') "Postion "
    write(*, '(I4)', advance = 'no') index
    write(*, '(a)', advance = 'no') ", for the vector X = "
    read(*, *) x

    write(*, '(a)', advance = 'no') "Postion "
    write(*, '(I4)', advance = 'no') index
    write(*, '(a)', advance = 'no') ", for the vector Y = "
    read(*, *) y
    
    001 S = S + x * y 
    
  end do ndimentionality

    write(*, *) ""
    write(*, *) "The dot product for these vectors is: ", S

end program actividad_01
