
! ------------------------------------------------------------------------------------ !
! @file     : Fundamentos de Programación
! @author   : alujan
! @brief    : Arreglos
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

! Los arreglos
! La definición de un arreglo, se realiza de la siguiente forma:
!
!         TYPE, DIMENSION(integer_value) :: variable_name
!
! Para este se requieren realizar las siguientes definiciones teoricas, como se realizan:
!
!     - Rango: Número de subindices
!     - Grado: Número de elementos
!     - Forma: Rango y grado
!     - Tamaño: Número total de elementos
!
! Para la carga de elementos, se requiere realizar declaración en parentesis y se 
! concatenan los mismos como se realiza con strings.
!
!     TYPE, DIMENTION(integer_value) :: variable_name = (/value_1, value_2, .../)
!
! Esta operación tambien se puede realizar por asignación, empleando DO en su forma
! explicita o implicita.
!
!     TYPE, DIMENTION(integer_value) :: variable_name = (/i, i = 1, 10, 1/)
!
! Se pueden realizat lecturas para asignación directa de los valores del arreglo, o con
! una operación conjunta. Limpieza de arreglo inicializando en cero.
!
! Es posible realizar una redefinición del rango de indices, y es posible definir:
!
!     TYPE, DIMENTION(integer_left:integer_right) :: variable_name ...
!
! Una recomendación es emplear parametros para la definición de los tamaños de un arreglo,
! en ya que genera comodidad de lectura:
!
!           INTEGER, PARAMETER::tamano=10
!           REAL, DIMENSION(tamano)::tiempo
!
! FORTRAN es capaz de realizar operaciones conjuntas aritmeticas ordinarias, en especial
! como lo realiza MATLAB, el único limitante son los tamaños de los arreglos.
!
! Es posible realizar subconjuntos del arreglo similar como se realiza con strings, 
! empleando secuencias sencillas que se emplean de indexado.
!
!   - subindice_1:subindice_2 >> subindice_1 hasta subindice_2 en paso 1
!   - subindice_1: >> subindice_1 hasta final en paso 1
!   - subindice_1::paso >> subindice_1 hasta subindice_2 en paso paso
!   - ::subindice_2 >> inicio hasta subindice_2 en paso 1
!   - :subindice_2:paso >> inicio hasta subindice_2 en paso paso
!   - ::paso >> inicio hasta fin en paso paso
!   - : >> inicio hasta fin en paso 1

! Actividad 2

! program actividad_02
!   
!   ! Declaration of variables
!   implicit none   ! Not implicit variables defined
! 
!   integer :: index = 1   ! indexer of the dimentionality of the vector
!   integer :: n = 0        ! Dimentionality of the vectors
! 
!   real, dimension(:), allocatable :: x   ! nth-Component of the first vector X
!   real, dimension(:), allocatable :: y   ! nth-Component of the second vector Y
! 
!   ! Definition by user of the lenth of operation
!   write(*, *) "Calculator for the dot product between 2 vectors of dimentionality n"
!   write(*, '(a)', advance = 'no') "Enter the dimentionality of the vectors you want to operate: "
!   read(*, *) n
! 
!   write(*,*) ""
!   write(*, '(a)', advance = 'no') "The dimention of the vectors is: "
!   write(*, *) n
!   write(*, *) "Enter the positions requested"
!   write(*,*) ""
! 
!   ! Creation of allocation for the vectors
!   allocate(X(n))    ! Allocation of the vector X to n dimension
!   allocate(Y(n))    ! Allocation of the vector Y to n dimension
! 
!   ! Input of the first vector X
!   write(*, *) "Enter the argument by argument the vector X"
!   read(*, *) (x(index), index = 1, n)
! 
!   ! Input of the second vector Y
!   write(*, '(a)', advance = 'no') ""
!   write(*, *) "Now, write down the arguments of the vector Y"
!   read(*, *) (y(index), index = 1, n)
! 
!   write(*, *) ""
!   write(*, *) "The dot product for these vectors is: ", sum(X * Y)
! 
! end program actividad_02

! Actividad 3

program actividad_03
  
  ! Declaration of variables
  implicit none   ! Not implicit variables defined

  integer :: index = 1             ! indexer of the dimentionality of the vector
  integer, parameter :: size = 3   ! Dimentionality of the vectors

  real, dimension(size) :: x = 0       ! nth-Component of the first vector X
  real, dimension(size) :: y = 0       ! nth-Component of the second vector Y

  real, dimension(size) :: cross = 0   ! Result of cross product

  ! Definition by user of the lenth of operation
  write(*, *) "Calculator for the cross product between 2 vectors of dimentionality 3"
  write(*,*) ""

  write(*, *) "Enter the positions requested"
  write(*,*) ""

  ! Input of the first vector X
  write(*, *) "Enter the argument by argument the vector X"
  read(*, *) (x(index), index = 1, size)

  ! Input of the second vector Y
  write(*, '(a)', advance = 'no') ""
  write(*, *) "Now, write down the arguments of the vector Y"
  read(*, *) (y(index), index = 1, size)

  ! Definition of the cross product
  041 cross(1) = x(2) * y(3) - x(3) * y(2)
  042 cross(2) = - (x(1) * y(3) - x(3) * y(1))
  043 cross(3) = x(1) * y(2) - x(2) * y(1)

  write(*, *) ""
  write(*, *) "The resultant vector for these cross is: ", cross
end program actividad_03
