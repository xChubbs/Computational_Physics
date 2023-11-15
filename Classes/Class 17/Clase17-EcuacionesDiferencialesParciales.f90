
! ------------------------------------------------------------------------------------ !
! @file     : Clase 16. Ecuaciones diferenciales Ordinarias
! @author   : @alujan
! @brief    : Solución de ecuaciones diferenciales ordinarias con Valores iniciales
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

! Reacración de modulos pasados

program numerical_integration

  ! Definition of variables
  implicit none

  real(8) :: t_initial = 0.
  real(8) :: t_final = 2 * 4*atan(1.)
  real(8), dimension(2) :: y_initial = (/ 0., 1. /)  

  integer :: M = 60

  integer :: i

  real(8), dimension(60 + 1, 3) :: E

  ! Call of function
  E = RK4s_PVI(f, t_initial, t_final, y_initial, M)
 
  ! Print of result
  do i = 1, M+1
    write(*,*) E(i, :)
  end do

  ! Definition of functions
  contains

  ! Test function
  function f(t, Y) result(X)

    ! Variables definition
    implicit none

    real(8), intent(in) :: t      ! Variable T
    
    real(8), dimension(:), intent(in) :: Y      ! Variable Y

    real(8), allocatable, dimension(:) :: X     ! Returning of the function 

    integer :: i    ! Component iterator

    ! Initialization of variables
    allocate(X(size(Y)))

    ! Function definition
    X = (/ Y(2), -Y(1) /)

  end function

  ! Runge - Kutta 4 Vectorial Method: PVI with first Taylor step
  function RK4s_PVI(f, t_initial, t_final, y_initial, M) result(RK4)

    ! Variables definition
    implicit none 
    
    real(8), dimension(:), intent(in) :: y_initial          ! Initial value y(t = 0)

    real(8), intent(in) :: t_initial, t_final               ! Interval to evaluate

    integer, intent(in) :: M                                ! Number of steps to evaluate (nodes)

    real(8), allocatable, dimension(:, :) :: RK4            ! Matrix RK4(t_step, f(t_step))

    real(8), allocatable, dimension(:) :: T                 ! T Component definition of RK4
    
    real(8), allocatable, dimension(:,:) :: Y               ! Y Component definition of RK4

    real(8), allocatable, dimension(:) :: F1, F2, F3, F4    ! Temporal components of summation

    real(8) :: h      ! Delta t: Length of interval to take

    integer :: i, j   ! Iterators for M matrix

    ! Definition of interface for Vectorial function F
    interface

      function f(t, Y) result(X)
        real(8), intent(in) :: t                    ! Variable T
    
        real(8), dimension(:), intent(in) :: Y      ! Variable Y

        real(8), allocatable, dimension(:) :: X     ! Returning of the function 

      end function f

    end interface

    ! Initialization of variables
    allocate(RK4(M + 1, size(y_initial) + 1))                     ! Allocation of RK2 matrix

    allocate(T(M + 1)); allocate(Y(M + 1, size(y_initial)))       ! Allocation of components for RK2 Matrix

    ! Allocation of temporal terms of summation
    allocate(F1(size(y_initial)))
    allocate(F2(size(y_initial)))  
    allocate(F3(size(y_initial)))
    allocate(F4(size(y_initial)))

    h = (t_final - t_initial) / M       ! Definition of delta t

    ! Explicit evaluation of the derivative f(t, y)
    explicit_evaluation: do i = 1, M

      ! First element definition
      if(i .EQ. 1) then; T(i) = t_initial; Y(i, :) = y_initial; endif

      ! Method definition
      T(i + 1) = t_initial + i * h

      F1 = h * f(T(i), Y(i, :))
      F2 = h * f(T(i) + h/2, Y(i, :) + F1/2)
      F3 = h * f(T(i) + h/2, Y(i, :) + F2/2)
      F4 = h * f(T(i) + h  , Y(i, :) + F3  )

      Y(i + 1, :) = Y(i, :) + (F1 + 2*F2 + 2*F3 + F4) / 6

    end do explicit_evaluation

    RK4(:, 1) = T; RK4(:, 2:) = Y    ! Component composition RK2

  end function ! Runge-Kutta 4 Vectorial Method

  end program numerical_integration
