
! ------------------------------------------------------------------------------------ !
! @file     : Tarea 02
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

! Problemas capitulo 6

! 10. The program is proposed as follows

!  program PolarToRectangular
!  
!    implicit none
!  
!    ! Definition of parameters
!    real, parameter :: PI = 4*ATAN(1.)     ! Description of PI
!  
!    ! Definition of variables
!    real, dimension(2) :: X_Rectangular   ! Rectangular coordinates vector
!    real, dimension(2) :: X_Polar         ! Polar coordinates vector
!  
!    integer :: iterator = 0   ! iterator variable
!  
!    ! Welcome sequence
!    write(*,*) "Calculator to convert Polar coordinates to Rectangular coordinates"
!    write(*,*) "Enter the vector in polar coordinates to transform:"
!    write(*,*) " - Use the notation X = <r, theta>"
!    write(*,*) ""
!    write(*,'(a)', advance= 'no') "  Vector defined as: X = "
!    read(*,*) (X_Polar(iterator), iterator = 1, 2)
!  
!    write(*,*) ""
!  
!    ! Transformation of the vector
!    X_Polar(2) = X_Polar(2) * PI / 180.   ! Transformation of angle
!  
!    X_Rectangular(1) = X_Polar(1) * cos(X_Polar(2))   ! X-Coordinate
!    X_Rectangular(2) = X_Polar(1) * sin(X_Polar(2))   ! Y-Coordinate
!  
!    ! Return sequence
!    write(*,*) " The vector in rectangular coordinates it's equivalent to: "
!    write(*,'(a)', advance='no') "X = "
!    write(*,'(F11.3)', advance='no') X_Rectangular(1)
!    write(*,'(a)', advance='no') " i + "
!    write(*,'(F11.3)', advance='no') X_Rectangular(2)
!    write(*,*) " j"
!  
!    write(*,*) ""
!    write(*,*) "this is equivalent to:"
!    write(*, '(a)', advance= 'no') "X = "
!    write(*,*) (X_Rectangular(iterator), iterator = 1, 2)
!  
!  end program PolarToRectangular

! 11. The opposite program to the proposed before is:

! program  RectangularToPolar
! 
!   implicit none
! 
!   ! Definition of parameters
!   real, parameter :: PI = 4*ATAN(1.)     ! Description of PI
! 
!   ! Definition of variables
!   real, dimension(2) :: X_Rectangular   ! Rectangular coordinates vector
!   real, dimension(2) :: X_Polar         ! Polar coordinates vector
! 
!   integer :: iterator = 0   ! iterator variable
! 
!   ! Welcome sequence
!   write(*,*) "Calculator to convert Polar coordinates to Rectangular coordinates"
!   write(*,*) "Enter the vector in polar coordinates to transform:"
!   write(*,*) " - Use the notation X = <x-coordinate, y-coordinate>"
!   write(*,*) ""
!   write(*,'(a)', advance= 'no') "  Vector defined as: X = "
!   read(*,*) (X_Rectangular(iterator), iterator = 1, 2)
! 
!   write(*,*) ""
! 
!   ! Transformation of the vector
!   X_Polar(1) = sqrt(sum((/(X_Rectangular(iterator)**2, iterator= 1, 2)/)))   ! Magnitude
!   X_Polar(2) = atan(X_Rectangular(2) / X_Rectangular(1))                      ! Angule
! 
!   X_Polar(2) = X_Polar(2) * 180. / PI     ! Transformation of angle
! 
!   ! Return sequence
!   write(*,*) " The vector in Polar coordinates it's equivalent to: "
!   write(*,'(a)', advance='no') "X = "
!   write(*,'(F11.2)', advance='no') X_Polar(1)
!   write(*,'(a)', advance='no') " ∠ "
!   write(*,'(F11.3)', advance='no') X_Polar(2)
! 
!   write(*,*) ""
!   write(*,*) "this is equivalent to:"
!   write(*, '(a)', advance= 'no') "X = "
!   write(*,*) (X_Polar(iterator), iterator = 1, 2)
! 
! end program RectangularToPolar

! 19. The program is proposed:

! program SetTheory
! 
!   implicit none
! 
!   ! Definition of variables
!   real, allocatable, dimension(:) :: A    ! Reference set
!   real, allocatable, dimension(:) :: B    ! Set to consider
! 
!   real, allocatable, dimension(:) :: result   ! Result set of the operation
! 
!   integer :: A_dim    ! Dimension of A
!   integer :: B_dim    ! Dimension of B
!   
!   integer :: indexer  = 0   ! Indexer of the sets
!   integer :: iterator = 0  ! Iterator of the sets
!   integer :: selector = 0  ! Selector of operation
! 
!   ! Welcome sequence
!   write(*,*) "Calculator of operation between 2 sets"
!   write(*,*) "Enter the number operation you want to use between the sets"
!   write(*,*) " 1. Union"
!   write(*,*) " 2. Intersection"
!   write(*,'(a)', advance= 'no') "  The operation wanted is: "
!   read(*,*) selector
! 
!   ! Dimension for A
!   write(*,*) ""
!   write(*,'(a)', advance='no') "Enter the dimension of the largest set, A: "
!   read(*,*) A_dim
! 
!   ! Allocation for A
!   allocate(A(A_dim))
! 
!   write(*,*) ""
!   write(*,*) "Enter below the values of the set A"
!   write(*,'(a)', advance='no') " A: "
!   read(*,*) (A(iterator), iterator = 1, A_dim)
! 
!   ! Dimension of B
!   write(*,*) ""
!   write(*,'(a)', advance='no') "Enter the smallest set, B: "
!   read(*,*) B_dim
! 
!    ! Allocation for A
!   allocate(B(B_dim))
! 
!   write(*,*) ""
!   write(*,*) "Enter below the values of the set B"
!   write(*,'(a)', advance='no') " B: "
!   read(*,*) (B(iterator), iterator = 1, B_dim)
! 
!   ! Program selection from selector
!   select case (selector)
! 
!   CASE(1); CALL getUnion(A, B, A_dim, B_dim, result)
! 
!   CASE(2); CALL getIntersection(A, B, A_dim, B_dim, result)
! 
!   CASE DEFAULT
! 
!     write(*,*) ""
!     write(*,*) "Invalid selection"
!     STOP
! 
!   end select
! 
!   ! Return sequence
!   write(*,*) "The result of the operation is: ", result
! 
!   ! Function definitions
!   contains
! 
!   subroutine getUnion(A, B, A_dim, B_dim, result)
! 
! 	  implicit none
! 	
! 	  ! Definition of variables
! 	  integer, intent(in) :: A_dim    ! Dimension of A
! 	  integer, intent(in) :: B_dim    ! Dimension of B
! 
! 	  real, dimension(:), intent(in) :: A    ! Reference set
! 	  real, dimension(:), intent(in) :: B    ! Set to consider
! 	
! 	  integer :: R_dim   ! Dimension of Result
! 
!     real, allocatable, dimension(:) :: temporal   ! Temporal set to return
! 
! 	  real, allocatable, dimension(:), intent(inout) :: result    ! Result set
! 	  
! 	  integer :: iterator = 0      ! Iterator of the function
!     integer :: indexer  = 1      ! Indexer of the set
! 	
! 	  ! Allocation of result
! 	  R_dim = A_dim + B_dim
! 	  
! 	  allocate(temporal(1:R_dim))
! 	
! 	  ! Defnition of union: A set is referenced
! 	  union_iter_1: do iterator = 1, A_dim
! 	
! 	    ! Control flow of the values: contained already in R
! 	    if( any( A(iterator) .EQ. temporal ) ) then; CYCLE; endif
! 	
! 	    ! Addition of the element
! 	    temporal(indexer) = A(iterator)
! 	
! 	    ! Control flow of the indexer
! 	    indexer = indexer + 1
! 
!     end do union_iter_1
! 
!     ! Defnition of union: A set is referenced
! 	  union_iter_2: do iterator = 1, B_dim
! 	
! 	    ! Control flow of the values: contained already in R
! 	    if( any( B(iterator) .EQ. temporal ) ) then; CYCLE; endif
! 	
! 	    ! Addition of the element
! 	    temporal(indexer) = B(iterator)
! 	
! 	    ! Control flow of the indexer
! 	    indexer = indexer + 1
! 
!     end do union_iter_2
! 
!     ! Recover of indexer and allocation of result
!     indexer = indexer -1
!     allocate(result(indexer))
!   
!     ! Reshape of result
!     result = temporal(1:indexer)
! 
!   end subroutine
! 
! 	subroutine getIntersection(A, B, A_dim, B_dim, result)
! 	
! 	  implicit none
! 	
! 	  ! Definition of variables
! 	  integer, intent(in) :: A_dim    ! Dimension of A
! 	  integer, intent(in) :: B_dim    ! Dimension of B
! 
! 	  real, dimension(A_dim), intent(in) :: A    ! Reference set
! 	  real, dimension(B_dim), intent(in) :: B    ! Set to consider
! 	
!     real, allocatable, dimension(:) :: temporal   ! Temporal set to return
! 
! 	  real, allocatable, dimension(:), intent(inout) :: result    ! Result set
! 	  
! 	  integer :: iterator = 1      ! Iterator of the function
!     integer :: indexer  = 1      ! Indexer of the set
! 	
! 	  ! Allocation of result: Minor set  
! 	  allocate(temporal(B_dim))
! 	
! 	  ! Defnition of union: A set is referenced
! 	  intersection_iter: do iterator = 1, B_dim
! 	
! 	    ! Control flow of the values: contained in B
! 	    if(any( B(iterator) .EQ. A )) then	
! 
! 	      ! Control flow of the values: contained already in R
! 	      if( any( B(iterator) .EQ. temporal ) ) then
!         
!           write(*,*) "Value already in temporal: ", B(iterator)
! 
!         else
!           ! Addition of the element
! 	        temporal(indexer) = B(iterator)
! 	
! 	        ! Control flow of the indexer
! 	        indexer = indexer + 1
! 
!           write(*,*) "Value not in temporal: ", B(iterator)
! 
!         endif
! 	
!       endif
! 	
! 	  end do intersection_iter
! 	 
!     ! Recover and allocation of indexer
!     indexer = indexer
!     allocate(result(indexer))
! 
!     ! Returning of result
! 	  result = temporal(1:indexer-1)
! 
!     !Dellocation of temporal
!     deallocate(temporal)
! 
!   	end subroutine
! 
! end program SetTheory

! 20. The program is proposed

program DistanceBetweenPoints

  implicit none

  ! Definition of variables
  real, allocatable, dimension(:) :: X, Y     ! Abstract base of the vectors

  integer :: N            ! Dimension of the space to consider
  integer :: iterator     ! Iterator for the vectors

  real :: distance    ! Distance of the vectors

  ! Welcome sequence
  write(*,*) "Calculator of the euclidian distance between 2 vectors"
  write(*,'(a)', advance= 'no')  "Enter the dimension of the space to consider: "
  read(*,*) N

  ! Allocation of vectors
  allocate(X(N)); allocate(Y(N))

  ! Input of the vectors
  write(*,'(a)', advance='no') " - Enter the first vector X: "
  read(*,*) (X(iterator), iterator = 1, N)

  write(*,'(a)', advance='no') " - Enter the second vector Y: "
  read(*,*) (Y(iterator), iterator = 1, N)

  ! Distance definition of the vectors
  002 distance = sqrt(sum((/( (X(iterator) - Y(iterator))**2, iterator=1, N )/)))

  ! Return sequence
  write(*,*) "The distance between the vectors: ", distance

  stop

end program DistanceBetweenPoints

