
! ------------------------------------------------------------------------------------ !
! @file     : examen 4
! @author   : @alujan
! @brief    : integración, diferenciación e interpolación
! ------------------------------------------------------------------------------------ !
! @authors
! 
! introducción a la física computacional
! facultad de ciencias
! ing. física
!
! ------------------------------------------------------------------------------------ !
! @atention
! 
! el siguiente código no cuenta con derechos de autor, sus intenciones son en su 
! totalidad completamente educativas. frente a cualquier duda, se recomienda
! remitirse directamente al autor.
!
! ------------------------------------------------------------------------------------ !
! @consideraciones del problema
!
! el problema presentado cuenta con 2 puntos fuertes que deben tenerse en cuenta en la
! resolución del problema, primeramente consideraremos la integral a evaluar.
!
! - numéricamente para poder cálcular la integral, debemos garantizar que el integrando
!   dentro de los nodos que se consideren debe ser continuo, en especifico la función
!   hallada cuenta con una singularidad en el extremo izquierdo, por lo que debe 
!   considerarse el uso de una regla de cuadratura abierta, para usos prácticos se
!   plantea el uso de la regla de trapecio abierta, donde se desprecian los valores
!   extremos de la evaluación.
!   aunque fortran es capaz de realizar la evaluación empleando una regla cerrada por
!   las fallas de precisión que presenta, por responsabilidad se realiza la labor extra
!   de la creación de la función.
!
! - en el planteamiento de la integral, debemos recordar un concepto fundamental en el
!   planteamiento del phi en que se evalua la integral. recordemos que para un pendulo,
!   el angulo theta que es considerado en su trayectoria corresponde al angulo que
!   describe el pendulo en su recorrido, por lo cual realmente la trayectoria que debemos
!   considerar positiva es horaria, por esto mismo antes de evaluar la integral, se
!   realiza un cambio de variable que permite describir este comportamiento.
!
! ------------------------------------------------------------------------------------ !
! @planteamiento de la integral
!
! veamos que inicialmente planteamos la integral considerando:
!
!         
!                    / φ
!                   |        1
!                   | ------------------- dφ'
!                   | √ cos(φ') - cos(φ₀)
!                  /   φ₀
!
! primeramente realizaremos el cambio: x = φ₀ - φ', que habla sobre el recorrido que
! realiza el pendulo desde el angulo φ₀ hasta el angulo que estamos considerando:
!
!                    / φ
!                   |        1
!                 - | ------------------- dφ'
!                   | √ cos(φ₀ - φ') - cos(φ₀)
!                  /   φ₀
!
! debemos ser cautelosos en la evaluación de la integral, ya que los angulos que
! vamos a considerar en grados, por lo cual es necesario tener en cuenta el factor
! de conversión.
!
! ------------------------------------------------------------------------------------ !
! @consideraciones del programa
! 
! el programa es adaptativo para las secciones 2 y 3, se recomienda el uso de un valor
! grande de datos para apreciaciones sustantivas en la interpolación del valor 15º,
! idealmente lo que se desea mostrar es que con una cantidad decente de puntos y, 
! consideraciones cercanas del intervalo, se pueden obtener buenas estimaciones.
!
! desde valores de cantidad de muestras (phi_nodes) mayores o iguales a 28, se obtienen
! buenos y consistentes resultados.
!
! ------------------------------------------------------------------------------------ !

! solución de examen 4

! preliminaries

program main

  ! variables definition
  implicit none

  ! universal variables definition
  real(8), parameter :: pi = 4 * atan(1.)   ! pi definition  
  real(8), parameter :: g  = 9.78           ! gravity in m / s^2

  ! internal variables definitions
  real(8) :: initial_phi   ! initial angle in radiants
  real(8) :: phi           ! angle to evaluate in radiants

  real(8) :: i          ! auxiliar variable for the integral
  real(8) :: time       ! traveled time aproximated using the numerical integral

  real(8), dimension(:), allocatable :: times     ! times vector for table

  real(8), dimension(:), allocatable :: sample_phi     ! phi vector for interpolation
  real(8), dimension(:), allocatable :: sample_times   ! times vector for interpolation
  
  integer :: intervals  ! number of intervals considered for the open rule

  real(8) :: l      ! length of the pendulum in m
  real(8) :: t      ! period of the pendulum in seconds

  real(8) :: phi_start    ! first value of phi for the table
  real(8) :: phi_step     ! length of step considered for the table
  real(8) :: phi_stop     ! last value of phi for the table

  integer :: phi_nodes    ! number of values considered

  integer :: k            ! iterator for table

  integer :: sample_partition     ! sample partition for interpolation

  real(8) :: sample_start         ! sample start value of interpolation: nearest phi < 15º
  real(8) :: sample_stop          ! sample stop value of the interpolation : nearest 15º < phi

  real(8) :: interpolated_time    ! value obtained from the interpolation

  ! welcome sequence
  write(*,*) " program made by @alujan"
  write(*,*) "the program contains the solutions for all the sections presented in the exam"
  write(*,*)  ! white space

  ! 1. with the values provided, we can compare the values for the aproximated value
  !    of the integral and a quarter of the period

  ! initialization of variables: integral aproximation
  initial_phi = pi / 180 * 10     ! set of value for 10º
  phi = pi / 180 * 0              ! set of value for 0º

  ! initialization of variables: period aproximation
  l = 0.5                              ! length of the pendulum
  t = 2. * pi * sqrt(l / g) * 1./4.    ! quarter of period aproximation

  ! numerical procedure: calculation of integral
  intervals = 1e4

  i = trapezoidal_open_integral(integrand, initial_phi, phi, intervals)
  
  time = sqrt(l / (2. * g)) * i

  ! end sequence of section
  write(*,*) " >> section 1"
  write(*,*) "comparing the value of the time expected using small angles aproximation and the numerical value of the integral,"
  write(*,*) "we arrived to the conclusions: "
  write(*,*) " - the value calculated for the period using the small angle aproximation gives us t = ", t
  write(*,*) " - using the numerical aproximation with ", intervals
  write(*,*) "   gives us a value of t = ", time
  write(*,*) " - comparing with the value of small angle period t, we obtained an absolute distance of: ", abs(t - time)
  write(*,*) "   resulting in a relative distance of: ", abs(t - time) / time
  write(*,*) "   we can conclude the small angle aproximation gives us an acceptable value with a small percentage of error"
  write(*,*)  ! white space

  ! 2. now let's consider the change of the time of quarter of a period with the initial condition

  ! initialization of variables
  phi_start = 10 * pi / 180   ! start value of the table in radiants
  phi_stop  = 80 * pi / 180   ! stop value fo the table in radiants

  ! input sequence
  write(*,*) " >> section 2"
  write(*,'(a)', advance = 'no') " for this section, first, enter the number of values you want to consider for the table: "

  read(*,*) phi_nodes

  write(*,*) "with ", phi_nodes, " nodes the desired step in degrees it's: ", (phi_stop - phi_start) / phi_nodes * 180 / pi
  write(*,*)  ! white space
  write(*,*) "the table expected it's:"
  write(*,*)  ! white space

  ! creation of table: declaration of header
  write(*,*) "    phi(degrees)          |       time(seconds)       |           delta t         |    percentual delta t" 

  ! creation of table: declaration of contents
  phi_step = (phi_stop - phi_start) / phi_nodes   ! declaration of the step per iteration

  allocate(times(0:phi_nodes))                      ! allocation of times vector

  table_iteration: do k = 0, phi_nodes

    ! calculation of time
    initial_phi = phi_start + k*phi_step

    i = trapezoidal_open_integral(integrand, initial_phi, phi, intervals)

    time = sqrt(l / (2. * g)) * i 
    
    ! declaration of row
    write(*,*) initial_phi * 180 / pi, "|", time, "|", abs(t - time), "|", abs(t - time) / time * 100

    ! saving of the value on vector
    times(k) = time

  end do table_iteration

  write(*,*) ! white space

  ! creation of auxiliar text file
  open(unit = 1, file = "time_stamp.txt", status = 'unknown')

  file_creation: do k = 0, phi_nodes

    ! calculation of time
    initial_phi = phi_start + k*phi_step
    
    ! declaration of row
    write(1,*) initial_phi * 180 / pi, times(k)

  end do file_creation

  close(1)    ! close of the unit

  ! end sequence of section
  write(*,*) "the diference shown on the table shows a growth on 1 order of magnitute in the absolute error,"
  write(*,*) "even a growth of 2 orders in the % percentual error, that makes the aproximation not apropiate for large angles"
  write(*,*)  ! white space
  write(*,*) "for convinence the file time_stamp.txt were created were the values of phi and times are stored for better"
  write(*,*) "visualization"
  write(*,*)  ! white space

  ! 3. let's take a proportion of the data presented for cretion of an interpolant and review of the results

  ! initialization of data
  sample_partition = 2  ! we're going to consider a partition of half the data
  
  initial_phi = 15.   ! value to interpolate 

  ! partition of time sample
  phi_start = 10.   ! start value of the sample in degrees
  phi_stop  = 10.   ! stop value fo the sample in degrees

  phi_step = phi_step * 180 / pi  ! step in degrees

  k = 0     ! sample size

  partition_finding_times: do

    ! partition length
    k = k + 1

    ! partition encapsulation
    if((initial_phi - phi_stop) .lt. 0) then

      ! partition creation
      phi_start = phi_stop - phi_step

      ! end of cycle
      exit
      
    end if

    ! control flow: cycle
    phi_stop = phi_stop + phi_step

  end do partition_finding_times

  ! allocation of sample
  allocate(sample_phi(0:(k / sample_partition))); allocate(sample_times(0:(k / sample_partition)))

  ! declaration sequence
  write(*,*) " >> section 3"
  write(*,*) "for this section, we're created a sample of the table presented previously, then we declared a partition for the"
  write(*,*) "sample, in this case the partition it's ", sample_partition
  write(*,*) "then we're going to consider a proportion of the data needed: ", 1. / sample_partition 
  write(*,*) " - i have in mind that the value expected", initial_phi, " º"
  write(*,*) " - it's between ", phi_start, " º"
  write(*,*) "   and ", phi_stop, " º"
  write(*,*) "the sample data used for the interpolation it's presented below: "
  write(*,*)  ! white space

  ! creation of table: declaration of header
  write(*,*) "    phi(degrees)          |       time(seconds)       " 

  ! sample declaration
  phi_start = 10.   ! start value of the sample in degrees

  sample_allocation: do k = 0, size(sample_phi)

    ! save of sample values
    sample_phi(k) = phi_start + sample_partition * k * phi_step

    sample_times(k) = times(sample_partition * k)

    ! declaration of row
    write(*,*) sample_phi(k) , "|", sample_times(k)

  end do sample_allocation

  write(*,*)  ! white space

  ! interpolation of value
  interpolated_time = lagran_interpolant(sample_phi, sample_times, initial_phi)

  ! calculation of numerical obtained time
  phi = 0. * pi / 180; initial_phi = 15. * pi / 180

  i = trapezoidal_open_integral(integrand, initial_phi, phi, intervals)

  time = sqrt(l / (2. * g)) * i

  ! ending sequence of section
  write(*,*) "with the values presented, the interpolant polynomial gives for the value ", 15., " º"
  write(*,*) "a value of: ", interpolated_time
  write(*,*) "and the numerical obtained value it's:", time
  write(*,*)  ! white space
  write(*,*) " - the estimation considered gives us and absolute error of ", abs(interpolated_time - time)
  write(*,*) " - and concludes in an percentage of error around", abs(interpolated_time - time) / time * 100, " %"

  ! function definitions
  contains

  real(8) function integrand(phi)

    ! variable definition
    implicit none

    real(8) :: phi    ! angle traveled by the pendulum

    integrand = - 1. / sqrt(cos(initial_phi - phi) - cos(initial_phi))

  end function integrand

  ! integral open trapezoidal rule: integration over the trapezoidal rule
  function trapezoidal_open_integral(f, x_left, x_right, intervals) result(integral)

    ! definition of variables
    implicit none

    real(8), external :: f                   ! function of integrand

    real(8), intent(in) :: x_left, x_right   ! integral interval

    integer, intent(in) :: intervals         ! number of intervals to consider

    real(8) :: integral        ! numerical integral value

    real(8) :: a, b            ! inner interval definitions: recursive limits

    real(8) :: h               ! step of integration

    integer :: i = 0           ! iterator of the integration rule

    ! condition of rule
    if( .not.(mod((intervals + 2), 2) .eq. 0)) then
      error stop "the intervals for the trapezoidal open rule must be even a multiple of 2"

    endif

    ! initialization integration variables
    integral = 0

    h = (x_right - x_left) / (intervals + 2)

    ! integration over subintervals: implicit definition of composite rule
    integration_sub_intervals: do i = 0, ((intervals + 2)/4 - 1)
  
      ! initialization of interval
      a = x_left + 4*i*h          ! initialization of left limit

      b = x_left + 4*(i + 1)*h    ! initialization of right limit

      ! value of sub-integral
      integral = integral + (2*f(a + h) - f(a + 2*h) + 2*f(a + 3*h))

    end do integration_sub_intervals

    ! scale of integral delta x
    integral = 4./3. * h * integral 
    
  end function ! integral open trapezoidal rule

  function lagran_interpolant(x, y, x_eval) result(y_eval)
	
    ! definition of variables
    implicit none

    real(8), dimension(:), intent(in) :: x   ! x-nodes of interpolation 
    real(8), dimension(:), intent(in) :: y    ! y-nodes of interpolation

    real(8), intent(in) :: x_eval             ! x-value to aproximate

    real(8) :: y_eval     ! y-value to aproximate

	  real(8) :: l_i        ! l polynomial aproximated value

    integer :: k, j       ! method iterators

    ! method definition
    y_eval = 0.           ! evaluation of x-value initialization
     
    for_values: do j = 1, size(x)

      ! determination of value hat
      l_i = 1     ! l aproximation hat initialization

      value_hat: do k = 1, size(x)

        ! control flow: zero case
        if(k .eq. j) then; cycle; endif
	
        l_i = l_i * (x_eval - x(k)) / (x(j) - x(k))
	
      end do value_hat
	
      y_eval = y_eval + y(j) * l_i

    end do for_values

  end function ! lagran interpolant

  end program main
