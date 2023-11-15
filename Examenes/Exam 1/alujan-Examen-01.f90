
! ------------------------------------------------------------------------------------ !
! @file     : Examen 1. Fundamentos de Programación
! @author   : alujan
! @brief    : Parte práctica del examen
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

! Programa Principal
program main

  implicit none   ! No implicit definitions

  ! Definition of variables
  complex, dimension(4, 5) :: mat_1     ! First file matrix
  complex, dimension(4, 5) :: mat_2     ! Second file matrix

  complex, dimension(4, 5) :: mat_sum    ! Sum matrix
  real,    dimension(4, 5) :: mat_mod    ! Module matrix

  real, dimension(20) :: vec_fila   ! Row vector of matrix values

  real :: min_value   ! Minimum value returned

  integer :: i = 1, status   ! Elements needed for reading

  ! Welcome sequence:
  write(*,*) "Exam done by @alujan"
  write(*,*) ""

  ! 1. Reading sequence of the matrix values
  ! Reading of the first matrix
  open(UNIT=1, FILE= "mat1.txt", STATUS= 'OLD')      ! Open of file with matrix 1

  ! Reading of the matrix by row
  mat_1_read: do i = 1, 4

    ! Control flow of status
    if(status .LT. 0) then; EXIT; endif

    read(1, *, IOSTAT = status) mat_1(i, :)   ! Reading of entire row 

  end do mat_1_read

  close(1)          ! Close of the first unit

  ! Reading of the Second matrix
  open(UNIT= 2, FILE= "mat2.txt", STATUS= 'OLD')    ! Open of file with matrix 1

  ! Reading of the matrix by row
  mat_2_read: do i = 1, 4

    ! Control flow of status
    if(status .LT. 0) then; EXIT; endif

    read(2, *, IOSTAT = status) mat_2(i, :)   ! Reading of entire row 

  end do mat_2_read

  close(2)         ! Close of the Second unit

  ! Echo of the values obtained for the matrix reading
  write(*,*) "1. The values obtained for the matrices are:"
  write(*,*) " >> Matrix 1: mat_1"

  mat_1_disp: do i = 1, 4
    ! Wrinting per row to have a more organized display
    write(*,*) mat_1(i, :)

  end do mat_1_disp

  write(*,*) ""
  write(*,*) " >> Matrix 2: mat_2"

  mat_2_disp: do i = 1, 4
    ! Wrinting per row to have a more organized display
    write(*,*) mat_2(i, :)

  end do mat_2_disp

  ! 2. Invocation of the suma_mat method
  CALL suma_mat(mat_1, mat_2, mat_sum)

  ! Echo of the value obtained for the matrix sum
  write(*,*) ""
  write(*,*) "2. The value obtained for the sum between the 2 matrices is:"
  write(*,*) " >> Matrix sum: mat_sum"
  
  mat_sum_disp: do i = 1, 4
    ! Wrinting per row to have a more organized display
    write(*,*) mat_sum(i, :)

  end do mat_sum_disp

  ! 3. Invocation fo the modu_mat method
  CALL modu_mat(mat_sum, mat_mod)

  ! Echo of the value obtained for the matrix sum
  write(*,*) ""
  write(*,*) "3. The value obtained for the mapping of the values and the module of each position is:"
  write(*,*) " >> Module matrix: mat_mod"

  mat_mod_disp: do i = 1, 4
    ! Wrinting per row to have a more organized display
    write(*,*) mat_mod(i, :)

  end do mat_mod_disp

  ! 4. Invocation of the mini_val method
  CALL mini_val(mat_mod, vec_fila, min_value)

  ! Echo of the value obtained for the matrix sum
  write(*,*) ""
  write(*,*) "4. The resultant vector that contains all the entries on the mat_mod is:"
  write(*,*) " >> Row vector: vec_fila"
  write(*,*) vec_fila
  
  write(*,*) ""
  write(*,*) "Finally the minimum value on the matrix is:"
  write(*,*) min_value

  ! Definition of subroutines
  contains

  ! 2. Definition of the sum_mat method
  subroutine suma_mat(mat_1, mat_2, mat_sum)

    implicit none   ! No implicit variables defined

    ! External variables definition
    complex, dimension(4, 5), intent(in) :: mat_1   ! Matrix 1
    complex, dimension(4, 5), intent(in) :: mat_2   ! Matrix 2

    complex, dimension(4, 5), intent(out) :: mat_sum  ! Result of sum matrix

    ! Local variables definition
    integer :: i = 1, j = 1     ! Variables needed for indexing
    
    ! Column - Row indexing: Reading through the the matrix
    row_read: do i = 1, 4
      column_read: do j = 1, 5
      
      mat_sum(i, j) = mat_1(i, j) + mat_2(i, j)   ! Sum of positions

      end do column_read
    end do row_read

  end subroutine

  ! 3. Definition of the modu_mat method
  subroutine modu_mat(mat_in, mat_mod)

  implicit none   ! No implicit variables defined

    ! External variables definition
    complex, dimension(4, 5), intent(in) :: mat_in   ! Matrix input

    real, dimension(4, 5), intent(out) :: mat_mod  ! Modules matrix
  
    ! Local variables definition
    integer :: i = 1, j = 1     ! Variables needed for indexing

  ! Column - Row indexing: Reading through the the matrix
    row_read: do i = 1, 4
      column_read: do j = 1, 5

      mat_mod(i, j) = abs(mat_in(i, j)) ! Parallelism of absolut value and module

      end do column_read
    end do row_read

  end subroutine

  ! 4. Definition of the mini_val method
  subroutine mini_val(mat_in, vec_fila, min_value)

    implicit none   ! No implicit variables defined

    ! External variables definition
    real, dimension(4, 5), intent(in) :: mat_in  ! Modules matrix

    real, dimension(20), intent(out) :: vec_fila    ! Row representation of mat_in
    
    real, intent(out) :: min_value   ! Minima of the matrix
  
    ! Local variables definition
    integer :: i = 1, j = 1, k = 1     ! Variables needed for indexing

    ! Minima value finding initialization
    min_value = mat_in(1, 1)

    ! Column - Row indexing: Reading through the the matrix
    row_read: do i = 1, 4
      column_read: do j = 1, 5
      
        ! Definition of the row representation for the matrix
        k = 5*(i - 1) + j

        ! Filling of the row representative vector
        vec_fila(k) = mat_in(i, j)

        ! Parallel finding of the lowest value on the matrix
        if(vec_fila(k) .LT. min_value) then; min_value = vec_fila(k); endif

      end do column_read
    end do row_read

  end subroutine

end program main

