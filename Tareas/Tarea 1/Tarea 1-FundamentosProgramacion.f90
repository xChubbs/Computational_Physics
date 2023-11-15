
! ------------------------------------------------------------------------------------ !
! @file     : Tarea 1
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

! Puntos de los problemas capitulo 2

! 17. The following program is proposed

!program PendulumPeriod
!
!  ! Declaration of variables
!  implicit none    ! No implicit variables defined
!
!  real :: L     ! Length input
!  real :: T     ! Period output
!
!  ! Definition of constants
!  real, parameter :: PI = 16*ATAN(1./5.) - 4*ATAN(1./239.)
!  real, parameter :: G = 9.81
!
!  ! Welcome sequence of program
!  write(*,*) "Calculator for the period (T) of a pendulum of lenght (L):"
!  write(*, '(a)', advance = 'no') "  Enter the lenght in meters (L) = "
!  
!  read(*,*) L ! Input of variables
!
!  ! Calculation of the value
!  001 T = 2 * PI * sqrt(L / G)
!  
!  ! Ending sequence
!  write(*,*) ""
!  write(*,*) " Value obtained for period it's: ", T, "Hz"
!  
!  ! Stop Sequence
!  stop
!
!end program PendulumPeriod

! 18. We can compute the tast using a program as follows

!program Hypothenuse 
!
!  ! Declaration of variables
!  implicit none    ! No implicit variables defined
!
!  real :: C1       ! Side 1
!  real :: C2       ! Side 2
!  real :: H        ! Hypothenuse
!
!  ! Welcome sequence of program
!  write(*,*) "Calculator for the hypothenuse of a right triangle"
!  write(*,*) "with 2 defined sides"
!
!  ! Input of variables
!  write(*, '(a)', advance = 'no') "  Enter the first Side of the triangle =  "
!  read(*,*) C1 
!
!  write(*, '(a)', advance = 'no') "  Enter the second Side of the triangle = "
!  read(*,*) C2
!
!  ! Calculation of the value
!  002 H = sqrt(C1**2 + C2**2)
!
!  ! Ending sequence
!  write(*,*) ""
!  write(*,*) " Value obtained for hypothenuse it's: ", H
!  
!  ! Stop Sequence
!  stop
!
!end program Hypothenuse

! 19. The folowing program is proposed

! program Logarithm 
! 
!   ! Declaration of variables
!   implicit none    ! No implicit variables defined
! 
!   real :: x       ! Argument
!   real :: b       ! Base
!   real :: L        ! Logarithm
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the Logarithm of an argument on a base"
!   write(*,*) "Consider the definition L :: Log_b(X)"
! 
!   ! Input of variables
!   write(*, '(a)', advance = 'no') "  Enter the argument you want to evaluate (x) = "
!   read(*,*) x 
! 
!   write(*, '(a)', advance = 'no') "  Enter the base to consider the log (b)      = "
!   read(*,*) b
! 
!   ! Verification on values
!   if(x <= 0) then
!     write(*,*) ""
!     write(*,*) "The value entered for x is invalid, remember the restrictions"
!     write(*,*) "for REAL logarithms"
! 
!     stop
!     
!   !Incorrect input for b
!   else if((b <= 0) .OR. (b == 1)) then
!     write(*,*) ""
!     write(*,*) "The value entered for b is invalid, remember the restrictions"
!     write(*,*) "for REAL logarithms"
! 
!     stop
! 
!   endif
! 
!   ! Calculation of the value
!   003 L = log(x) / log(b)
! 
!   ! Ending sequence
!   write(*,*) ""
!   write(*,*) " Value obtained for logarithm it's: ", L
!  
!   ! Debugger conditions
!   if((x == 10) .AND. (b == 2.71)) then
!     ! Debugger conditions input
!     write(*,*) ""
!     write(*,*) "@Developer mode active:"
!     write(*,*) " The reference value it's LOG(10) = ", log(10.)
!     write(*,*) " The error detected it's = ", log(10.) - L
! 
!   endif
!   ! Stop Sequence
!   stop
!
!end program Logarithm

! 21. The program goes as follows

! program Distance 
! 
!   ! Declaration of variables
!   implicit none    ! No implicit variables defined
! 
!   real :: x       ! X-Coordinate component
!   real :: y       ! Y-Coordinate component
! 
!   complex :: P1   ! Point 1
!   complex :: P2   ! Point 2
!   
!   real :: D       ! Distance computed
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the distante betweeen 2 points on the"
!   write(*,*) "coordinate system, consider the points as (x, y)"
! 
!   ! Input of variables
!   write(*,'(a)', advance = 'no') "  Enter the first point: "
!   read(*,*) x, y
! 
!   P1 = complex(x, y)  ! Transformation of value
! 
!   write(*,'(a)', advance = 'no') "  Enter the second point: "
!   read(*,*) x, y
! 
!   P2 = complex(x, y)  ! Transformation of value
! 
!   ! Calculation of the value
!   005 D = abs(P1 - P2)
! 
!   ! Ending sequence
!   write(*,*) ""
!   write(*,*) " Distance obtained for the points it's: ", D
!  
!   ! Debugger conditions
!   if((P1 == (-1, 1)) .AND. (P2 == (6, 2))) then
!     ! Debugger conditions input
!     write(*,*) ""
!     write(*,*) "@Developer mode active:"
!     write(*,*) " The reference value it's = ", abs((-1,1) - (6,2))
!     write(*,*) " The error obtained it's  = ", abs((-1,1) - (6,2)) - D
! 
!   endif
!   ! Stop Sequence
!   stop
! 
! end program Distance

! 23. The program defined goes as follows:

!program hyperbolicCosine
 
!   ! Declaration of variables
!   implicit none    ! No implicit variables defined
! 
!   real :: x       ! Argument
!   real :: C       ! Hyperbolic Cosine
!    
!   ! Definition of constants
!   real, parameter :: E = exp(1.)
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the hyperbolic cosine of an argument (x)"
!  
!   ! Input of variables
!   write(*, '(a)', advance = 'no') "  Enter the argument you want to evaluate (x) = "
!   read(*,*) x
!  
!   ! Calculation of the value
!   006 C = (exp(x) + exp(-x)) / 2
! 
!   ! Ending sequence
!   write(*,*) ""
!   write(*,*) " Value obtained for logarithm it's: ", C
!   
!   ! Debugger conditions
!   if(x == 3.) then
!     ! Debugger conditions input
!     write(*,*) ""
!     write(*,*) "@Developer mode active:"
!     write(*,*) " The reference value it's cosh(3) = ", cosh(3.)
!     write(*,*) " The error detected it's = ", cosh(3.) - C
!   endif
! 
!   ! Stop Sequence
!   stop
! 
! end program hyperbolicCosine

! 25. The program proposed goes as follows:

! program RadioReciever
!  
!   ! Declaration of variables
!   implicit none    ! No implicit variables defined
! 
!   ! Definition of constants
!   real, parameter :: PI = 16*ATAN(1./5.) - 4*ATAN(1./239.)
! 
!   real :: L       ! Inductance
!   real :: C       ! Capacitance
!   real :: f       ! Frecuency
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the resonant frequency of a series RLC circuit"
!  
!   ! Input of variables
!   write(*, '(a)', advance = 'no') "  Enter the value of the inductor (L) in Henrys (H)  = "
!   read(*,*) L
!  
!   write(*, '(a)', advance = 'no') "  Enter the value of the capacitor (C) in Farads (F) = "
!   read(*,*) C
! 
!   ! Verification on values
!   if(L .LE. 0) then
!     write(*,*) ""
!     write(*,*) "The value entered for Inductance is invalid, remember the restrictions"
!     write(*,*) "Inductance components"
! 
!     stop
!     
!   !Incorrect input for b
!   else if(C .LE. 0) then
!     write(*,*) ""
!     write(*,*) "The value entered for Capacitance is invalid, remember the restrictions"
!     write(*,*) "Capacitance components"
! 
!     stop
! 
!   endif
! 
!   ! Calculation of the value
!   007 f = 1. / (2 * PI * sqrt(L * C))
! 
!   ! Ending sequence
!   write(*,*) ""
!   write(*,*) " Value obtained for the resonant frecuency (f) in Hertz it's: ", f
!   
!   ! Stop Sequence
!   stop
!
!end program RadioReciever

! 26. The program is proposed:

! program TurningRadious
!  
!   ! Declaration of variables
!   implicit none    ! No implicit variables defined
! 
!   ! Definition of constants
!   real, parameter :: Vs = 340  ! Speed of sound (m / s)
!   real, parameter ::  g = 9.81 ! Acceleration of gravity (m / s^2)
! 
!   character :: selection ! Character to decide flow
! 
!   real :: a     ! acceleration (m/ s^2)
!   real :: v     ! Tangent Speed (m / s)
!   real :: r     ! Radious of aircraft (m)
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the turning radious of an aircraft with the folowing conditions:"
!  
!   ! Section A
!   write(*,*) "Select a section from a), to c)"
!   write(*, '(a)', advance = 'no') "  Enter a character to select: "
!   read(*,*) selection 
! 
!   ! Flow of program based on selection
!   if(selection .EQ. 'a') then
!     ! Initialization of variables
!     v = Vs * .8
!     a = 2.8*g
! 
!     write(*,*) ""
!     write(*,*) "The conditions defined are: "
!     write(*,*) "  Velocity is Mach 0.8, v = ", v
!     write(*,*) "  Acceleration is 2.5g, a = ", a
! 
!   else if(selection .EQ. 'b') then
!     ! Initialization of variables
!     v = Vs * 1.5
!     a = 2.8*g
! 
!     write(*,*) ""
!     write(*,*) "The conditions defined are: "
!     write(*,*) "  Velocity is Mach 1.5, v = ", v
!     write(*,*) "  Acceleration is 2.5g, a = ", a
! 
!   else if(selection .EQ. 'c') then
!     ! Initialization of variables
!     v = Vs * 1.5
!     a = 0.7 * g
! 
!     write(*,*) ""
!     write(*,*) "The minimum conditions are: "
!     write(*,*) "  Velocity is Mach 1.5, v = ", v
!     write(*,*) "  Acceleration is 0.7g, a = ", a
! 
!   else
!   ! Default case handling
!     write(*,*) ""
!     write(*,*) "  The selection made it's out of bounds, Please try again!"
!     stop
! 
!   endif
! 
!   ! Calculation of the value
!   008 r = v**2 / a
! 
!   ! Ending sequence
!   write(*,*) ""
!   write(*,*) " The value obtained for the radious in meters it's r = ", r
!   
!   ! Stop Sequence
!   stop
! 
! end program TurningRadious

! 27. The program proposed goes as follows

! program ScapeVelocity
!  
!   ! Declaration of variables
!   implicit none    ! No implicit variables defined
! 
!   ! Definition of constants
!   real, parameter ::               G = 6.673E-11      ! Gravitational Constant
! 
!   real, parameter, dimension(4) :: Ms = (/ 6.E24, &  ! Mass: Earth
!                                           7.4E22, &  ! Mass: Moon
!                                           8.7E20, &  ! Mass: Ceres
!                                           1.9E27/)   ! Mass: Jupiter
! 
!   real, parameter, dimension(4) :: Rs = (/ 6.4E6, &  ! Radious: Earth
!                                            1.7E6, &  ! Radious: Moon
!                                            4.7E5, &  ! Radious: Ceres
!                                            7.1E7/)   ! Radious: Jupiter
! 
!   integer:: Selection   ! Character to decide flow
! 
!   real :: v     ! Scape velocity (m/ s^2)
!   real :: M     ! Mass selected (kg)
!   real :: R     ! Radious Selected (m)
!   
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the scape velocity in each celestial body"
!  
!   ! Section A
!   write(*,*) ""
!   write(*,*) "Select a Celestial body to calculate the scape velocity"
!   write(*,*) "  Enter a Selection from the following: "
!   write(*,*) "  (1) Earth"
!   write(*,*) "  (2) Moon"
!   write(*,*) "  (3) Ceres"
!   write(*,*) "  (4) Jupiter"
! 
!   write(*, '(a)', advance = 'no') " Body selected: "
!   read(*,*) selection 
! 
!   ! Flow of program based on selection
!   if(selection == 1) then
!     ! Initialization of variables
!     M = Ms(1)
!     R = Rs(1)
! 
!   else if(selection == 2) then
!     ! Initialization of variables
!     M = Ms(2)
!     R = Rs(2)
! 
!   else if(selection == 3) then
!        ! Initialization of variables
!     M = Ms(3)
!     R = Rs(3)
! 
!   else if(selection == 4) then
!        ! Initialization of variables
!     M = Ms(4)
!     R = Rs(4)
! 
!   else
!   ! Default case handling
!     write(*,*) ""
!     write(*,*) "  The selection made it's out of bounds, Please try again!"
!     stop
! 
!   endif
! 
!   ! Calculation of the value
!   009 V = sqrt(2. * G * M / R)
! 
!   ! Ending sequence
!   write(*,*) ""
!   write(*,*) " The value obtained for the scape velocity in (m / s) is: ", V
!   
!   ! Stop Sequence
!   stop
! 
! end program ScapeVelocity

! Ejercicios Capitulo 3

! 5. The following program is proposed

! program PackageCost
! 
!   ! Parameter Definitions
!   integer, parameter :: EXPRESS_DELIVERY_SERVICE = 12 
!   integer, parameter :: POUND_FRACTION_PRICE     = 4
!   integer, parameter :: EXCESS_WEIGHT_PRICE      = 10
! 
!   integer, parameter :: MAXIMUM_WEIGHT_THRESHOLD = 100
!   
!   ! Variables Definitions
!   integer :: cost   = 0    ! Cost of the package carry
!   integer :: weight = 0    ! Weight of the package
! 
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the cost of sending a package"
!   
!   write(*,*) "Enter the weight of the package"
!   write(*,'(a)', advance = 'no') "  Weight of the package in kg: "
! 
!   read(*,*) weight
! 
!   write(*,*) ""
! 
!   ! Maximum conditions found
!   if(weight .GT. MAXIMUM_WEIGHT_THRESHOLD) then
!     
!     ! Break of program
!     write(*,*) "The package is not accepted for shipping."
!     write(*,*) "Try again"
!     stop
!   endif
! 
!   ! No minimum conditions found
!   if(weight .LE. 2) then
!     
!     ! Break of program
!     write(*,*) "The cost of the package it's: ", cost + EXPRESS_DELIVERY_SERVICE
!     stop
!   endif
! 
!   ! Initial costs: Delivery service
!   cost = cost + EXPRESS_DELIVERY_SERVICE
!   weight = weight - 2
!   
!   if(weight .GT. 70) then
!     cost = cost + EXCESS_WEIGHT_PRICE
!   endif
! 
!   cost = cost + weight/2 * POUND_FRACTION_PRICE
! 
!   ! Result display sequence
!   write(*, '(a)', advance = 'no') "The price of the service it's: ", cost
! 
!   stop
! end program PackageCost

! 7. The following code is proposed

!  program PieceWiseFunction
!  
!   ! Variables Definitions
!    real :: x = 0  ! X-value
!    real :: y = 0  ! Y-value
!  
!    ! Welcome sequence of program
!    write(*,*) "Calculator for the piece-wise function f(x, y) proposed in example 3-3"
!    
!    write(*,*) "Enter the values for each variable to evaluate"
!    write(*,'(a)', advance = 'no') " X-Value: x = "
!    read(*,*) x
!  
!    write(*,'(a)', advance = 'no') " Y-Value: y = "
!    read(*,*) y
! 
!    write(*,*) ""
!  
!    ! Conditions evaluation for each variable: Pattern saw on x_i < 0
!    if(x .LT. 0) then; x = x**2; endif
!    if(y .LT. 0) then; y = y**2; endif
!  
!    ! Result display sequence
!    write(*, *) "The value of the function is: f(x, y) = ", x + y
!  
!   stop
!  end program PieceWiseFunction

! 12. The code is proposed as follows

! program Refraction
! 
!   ! Definition of constants
!   real, parameter :: PI = 16*ATAN(1./5.) - 4*ATAN(1./239.)
! 
!   ! Variables Definitions
!   real :: n_1 = 0  ! Refraction index 1
!   real :: n_2 = 0  ! Refraction index 2
! 
!   real :: theta_1 = 0  ! Incident angle 1
!   real :: theta_2 = 0  ! Incident angle 2
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator for the incident angle of region 2. Based on figure 3-16"
!   
!   write(*,*) "Enter the Region indexes & angles present"
!   write(*,'(a)', advance = 'no') " Region 1 index n_1 = "
!   read(*,*) n_1
! 
!   write(*,'(a)', advance = 'no') " Region 2 index n_2 = "
!   read(*,*) n_2
! 
!   write(*, '(a)', advance = 'no') " Region 1 incident angle theta_1 = "
!   read(*,*) theta_1
! 
!   write(*,*) ""
! 
!     ! No refraction conditions
!   if(n_1 .GT. n_2) then
! 
!     write(*,*) "  No refraction phenomena ocurrs, there will be Reflextion instead"
!     write(*,*) "  The reflexion angle it's: theta_2 = ",  theta_1
!     
!     stop
! 
!   endif
! 
!   ! Transformation of theta variable
!   theta_1 = theta_1 * PI / 180.
! 
!   ! Calculation of the refracted angle:
!   010 theta_2 = asin(n_1 / n_2 * sin(theta_1))
! 
!   ! Transformation of theta variable
!   theta_2 = theta_2 * 180. / PI
! 
!   ! Result display sequence
!   write(*,*) "  The refracted angle it's: theta_2 = ", theta_2
! 
!   stop
! end program Refraction

! Problemas Capitulo 4.

! 16. The code is proposed:

! program CalculatingOrbits
! 
!   ! Definition of constants
!   real, parameter :: PI = 16*ATAN(1./5.) - 4*ATAN(1./239.)
! 
!   real, parameter :: P = 1200  ! Size of the orbit (km)
! 
!   ! Variables Definitions
!   character(1) :: control_var = 'y'    ! Control flow of the program
!   logical :: welcome_sequence = .TRUE. !Initialization of the program
! 
!   real :: theta   = 0   ! Angle argument of the radious calculation
!   real :: epsilon = 0  ! Eccintricy of the orbit
! 
!   real :: radious = 0  ! Radious in function of the angle theta
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator of the radious of an orbit such a theta is input"
!   
!   ! Input of non-variational parameter
!   write(*,*) "  Enter the eccintricy to be consider in the orbital"
!   write(*,'(a)', advance = 'no') "    The eccintricy epsilon considered it's: epsilon = "
!   read(*,*) epsilon
! 
!   ! Cycle behaviour of the program
!   control_flow: do 
! 
!     if(.not. welcome_sequence) then
!     ! Control flow sequence
!       write(*,*) ""
!       write(*,*) "  Continue the calculation with the given eccintricy?"
!       write(*,*) "  Remember the value used it's epsilon: ", epsilon
!       write(*,*) ""
!       write(*,'(a)', advance = 'no') "Enter 'y' to continue, or 'n' to stop the program : "
!       read(*,*) control_var
!     endif
! 
!     !Control Of the reference variables
!     if(welcome_sequence) then; welcome_sequence = .FALSE.; endif
!     if(control_var .EQ. 'n') then; EXIT; endif
! 
!     ! Input of variational parameters
!     write(*,*) "  Enter the instant angle to be considered, use degrees."
!     write(*,'(a)', advance = 'no') "    The instant angle it's theta: theta = "
!     read(*,*) theta
! 
!     ! Transformation of the angle given:
!     theta = theta * PI / 180.
! 
!     011 radious = P / (1 - epsilon * cos(theta))
! 
!     ! Output of result
!     write(*,*) ""
!     write(*,*) "  The radious in kilometers for the instant given it's: r = ", radious
! 
!   end do control_flow
! 
!   ! End sequence of the progam
!   write(*,*) ""
!   write(*,*) "You selected to stop the calculation"
!  write(*,*) "reinit the program to change your parameters!"
!   stop
! 
! end program CalculatingOrbits

! 18. The following program is proposed

! program CurrentThroughDiode
! 
!   implicit none
! 
!   ! Definition of constants
!   real, parameter :: q   = 1.602E-19    ! Charge of the electron (C)
!   real, parameter :: k   = 1.38E-23     ! Boltzmann constant (J/K)
!   real, parameter :: I_0 = 2E-6         ! Current Leakage of the diode (A)
! 
!   ! Definition of functions
!   real, external :: convert_farenhait_to_kelvin   ! Conversion of temperature units
!   
!   ! Variables Definitions
!   real :: V_0 = 0       ! Initial Voltage across the diode (V)
!   real :: V_F = 0       ! Final Voltage across the diode (V)
!   real :: V_i = 0       ! Iterator Voltage across the diode (V)
! 
!   real :: I_D = 0       ! Current across the diode (A)
!   real :: T_0 = 0       ! Initial temperature of the medium (F)
!   real :: T_F = 0       ! End Temperature of the medium (F)
!   real :: T_i = 0       ! Iterator over temperatures (F)
!   real :: T_k           ! Temperature in Kelvin (K)
! 
!   real :: T_delta = 0     ! Step taken for the calculations (F)
!   real :: V_delta = 0     ! Step taken for the calculations (V)
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator of the Current flow (I_D) of a diode in Amperes given equation"
!   write(*,*) " (4-10), the calculation is done for Voltages from -1.0V to 0.6V"
! 
!   ! Input of Variational variables
!   write(*, *) "   Enter the range of temperature to be considered (ºF)"
!   write(*, '(a)', advance= 'no') "   Use the notation: T_initial, T_step, T_Final: "
!   read(*,*) T_0, T_delta, T_F
! 
!   write(*,*) "" ! New line
! 
!   write(*, *) "   Enter the range of Voltage to be considered (V)"
!   write(*, '(a)', advance= 'no') "   Use the notation: V_initial, V_step, V_Final: "
!   read(*,*) V_0, V_delta, V_F
! 
!   write(*,*) "" ! New line
! 
!   ! Creation of table header
!   write(*,'(a)', advance= 'no') "   Voltage "
!   write(*,'(a)', advance= 'no') " | "
! 
!   T_i = T_0! Initial values for temperature
! 
!   ! Iteration over temperature levels
!   iter_header: do
! 
!     ! Iteration over temperature levels
!     write(*, '(F11.3)', advance= 'no') T_i
! 
!     ! Inclusion of separator
!     write(*, '(a)', advance= 'no') ' | '
! 
!     ! Control flow for next iteration
!     T_i = T_i + T_delta
!     if(T_i .GT. T_F) then; EXIT; endif
! 
!   end do iter_header
! 
!   ! Creation of new line 
!   write(*,*) ""
! 
!   ! Control flow for Voltage iteration
!   V_i = V_0   ! Initial iteration
! 
!   ! Cycle behaviour of the program
!   iter_voltage: do
!     
!     ! Creation of row: Line title
!     write(*, '(F11.2)', advance= 'no') V_i
! 
!     ! Inclusion of separator
!       write(*, '(a)', advance= 'no') ' | '
! 
!     ! Control flow for temperature iteration
!     T_i = T_0   ! Initial value
! 
!     ! Creation of row: iteration over temperatures
!     iter_temp: do
! 
!       ! Conversion of the temperature
!       T_k = convert_farenhait_to_kelvin(T_i)
! 
!       ! Calculation of the current in the diode
!       012 I_D = I_0 * (exp(q * V_i / (k * T_k)) - 1)
! 
!       ! Addition of value in row
!       write(*, '(ES11.3)', advance= 'no') I_D
! 
!       ! Inclusion of separator
!       write(*, '(a)', advance= 'no') ' | '
! 
!       ! Control flow for next iteration
!       T_i = T_i + T_delta
!       if(T_i .GT. T_F) then; EXIT; endif
! 
!     end do iter_temp
! 
!     ! New Line for the iteration
!     write(*,*) ""
! 
!     ! Control flow for next iteration
!     V_i = V_i + V_delta
!     if(V_i .GT. V_F) then; EXIT; endif
! 
!   end do iter_voltage
! 
!   ! End sequence of the progam
!   write(*,*) ""
!   write(*,*) "The table presented are all the combinations of current & Temperature for the ranges selected" 
!   stop
! 
! end program CurrentThroughDiode
! 
! ! Function handle of the conversion
! real function convert_farenhait_to_kelvin(farenhait)
! 
!   implicit none
! 
!   ! Parameters
!   real, intent(in) :: farenhait
!   
!   ! Convertion to celcius then to kelvin
!   013 convert_farenhait_to_kelvin = (farenhait - 32.) * 5./9. + 273.15
! end function

! 22. The solution is proposed as follows:

! program TensionCable
! 
!   implicit none
! 
!   ! Definition of parameters
!   real, parameter :: g = 9.81   ! Gravity of earth considered
! 
!   ! Variables Definitions
!   real :: d_0 = 0       ! Initial distance of the pivot (m)
!   real :: d_F = 0       ! Final distance of the pivot (m)
!   real :: d_i = 0       ! Iterative distance of the pivot (m)
!   real :: d_min = 0     ! Minimum Tension distance (m)
! 
!   real :: d_delta = 0     ! Step taken for the calculations (m)
! 
!   real :: L_c = 0       ! Length of the cable (m)
!   real :: L_p = 0       ! Length of the pole (m)
!   real :: M_p = 0       ! Mass of the pole (kg)
! 
!   real :: T_i   = 0       ! Iterative tension of the pole (N)
!   real :: T_min = 0       ! Minimum tension of the cable
! 
! 
!   ! Welcome sequence of program
!   write(*,*) "Calculator of the Tension for the cable given equation"
!   write(*,*) "(4-10), given the image (4-15)"
! 
!   ! Input of Variational variables
!   write(*, *) "   Enter the parameters needed"
!   write(*, '(a)', advance= 'no') "    - Length of the cable in meters: L_c = "
!   read(*,*) L_c
! 
!   write(*, '(a)', advance= 'no') "    - Length of the pole in meters: L_p = "
!   read(*,*) L_p
! 
!   write(*, '(a)', advance= 'no') "    - Mass of the pole in kilograms: M_p = "
!   read(*,*) M_p
! 
!   write(*, '(a)', advance= 'no') "    - Precision of distance d needed: d_delta = "
!   read(*,*) d_delta
! 
!   write(*,*) "" ! New line
!   
!   ! Control flow of the iteration
!   d_F = L_c
!   d_0 = d_delta
!   d_i = d_0
! 
!   ! Iterative solution of the problem
!   iter_tension: do
!     
!     ! Calculation of tension
!     014 T_i = M_p * g * L_c * L_p / (d_i * sqrt(L_p**2 - d_i**2))
! 
!     ! Initialization of the iteration for Tension
!     if(d_i .EQ. d_0) then; T_min = T_i; endif
! 
!     ! Iterative search for min tension
!     if(T_i .LT. T_min) then
!       T_min = T_i
!       d_min = d_i
!     endif
! 
!     ! Control flow
!     d_i = d_i + d_delta                 ! Next iteration
! 
!     if(d_i .GT. d_F) then; EXIT; endif  ! Control flow of the iterations
!     
!   end do iter_tension
! 
!   ! End sequence of the program
!   write(*,*) ""
!   write(*,*) " The Minimum tension found at: d = ", d_min
!   write(*,*) " The value of the tension is: T = ", T_min
!   stop
! 
! end program TensionCable

! 26. The following code is proposed

! program InfiniteSeries
! 
!   implicit none
! 
!   ! Definition of parameters
!   real, parameter :: PI = 4 * ATAN(1.)
! 
!   ! Definition of functions
!   integer, external :: factorial   ! Factorial of a number
! 
!   ! Definition of variables
!   real    :: x = 0     ! Dummy angle to be considered
!   integer :: N = 0     ! Number of terms wanted
! 
!   real :: S = 0     ! Sum value of the series.
! 
!   integer :: i           = 0    ! Iterator
!   real    :: epsilon     = 0    ! Tolerance 
!   real    :: Abs_error   = 0    ! Error
! 
!   ! Welcome sequence
!   write(*,*) "Calculator for the SIN(X) according the series presented in (4-14)"
!   write(*,'(a)', advance= 'no') " Enter an angle in degrees X: x = "
!   read(*,*) x
! 
!   write(*,*) "" ! New line
! 
!   write(*,*) "--- Calculation for the SIN(X) using a number of terms of the series ---"
!   write(*,'(a)', advance= 'no') " Enter the number of terms wanted, use N between 0 and 16: N = "
!   read(*,*) N
!   
!   write(*,*) "" ! New line
! 
!   ! Transformation of value from degrees to radiants
!   x = x * PI / 180.
! 
!   ! Calculation of the sin(x)
!   iter_number: do i = 0, N
! 
!     ! Accumulation and calculation of the i-th term
!     015 S = S + (-1)**(i) * x**(2*i +1) / factorial(2*i +1)
! 
!   end do iter_number
! 
!   ! Return sequence of the value
!   write(*,*) "    The value obtained for this number of terms is: SIN(X) = ", S
!   write(*,*) "    The computer value for the angle is: SIN(X) = ", sin(x)
!   write(*,*) "    The error obtained for this aproximation is: ", abs(sin(x) - S)
! 
! 
!   ! Error based sequence
!   write(*,*) "" ! New line
! 
!   write(*,*) "--- Calculation for the SIN(X) using an estimated tolerance to the computer value ---"
!   write(*,'(a)', advance= 'no') " The tolerance needed for the aproximation: epsilon = "
!   read(*,*) epsilon
! 
!   ! Re-initializartion of the indexer & accumulator
!   i = 0
!   S = 0
! 
!   ! Calculation of the sin(x)
!   iter_error: do
! 
!     ! Accumulation and calculation of the i-th term
!     S = S + (-1)**(i) * x**(2*i + 1) / factorial(2*i +1)
! 
!     Abs_error = abs(S - sin(x))
! 
!     ! Error control flow
!     if(Abs_error .LT. epsilon) then; EXIT; endif
!     
!     ! Next iteration preparation
!     i = i + 1
! 
!   end do iter_error
! 
!   ! Return sequence of the value
!   write(*,*) "    The value obtained for this tolerance is: SIN(X) = ", S
!   write(*,*) "    The number of iterations needed for this tolerance is: N = ", i
! 
! end program InfiniteSeries
! 
! ! Auxiliat program to calculate the factorial of a number
! integer function factorial(number)
!  
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in) :: number
!   
!   integer :: iterator = 0    ! Factorial Iterator
!   
!   ! Case solving: Factorial of 0
!   if(number .LT. 0) error stop 'factorial is singular for negative integers'
! 
!   ! Case solving: Factorial of 0
!   if(number .EQ. 0) then; factorial = 1; endif
! 
!   ! Return of value
!   factorial = product((/(iterator,iterator=1,number)/))
! 
! end function

! 27. The code is proposed as follows

! program GeometricMean
! 
!   implicit none
! 
!   ! Definition of functions
!   real, external :: getAritmethicMean   ! Aritmethic Mean
!   real, external :: getGeometricMean    ! Geometric Mean
! 
!   ! Definition of variables
!   real, allocatable, dimension(:) :: numbers    ! Array of numbers
! 
!   integer                         :: N    = 0     ! Number of terms wanted
!   integer                         :: iter = 0     ! Iterator for numbers reception
! 
!   real :: geometric  = 0     ! Geometric mean value
!   real :: aritmethic = 0     ! Aritmethic mean value
! 
!   ! Welcome sequence
!   write(*,*) "Calculator of the geometric & aritmethic mean of a sequence of numbers"
!   write(*,'(a)', advance= 'no') " Enter the dimension of the sequence you want to consider: N = "
!   read(*,*) N
! 
!   write(*,*) "" ! New line
! 
!   ! Allocation of data
!   allocate(numbers(N))
! 
!   ! Input of elements on the array
!   write(*,'(a)', advance= 'no') " Enter all the terms of the sequence: x = "
!   read(*,*) (numbers(iter), iter = 1, N)
! 
!   ! Calculation of the means
!   aritmethic = getAritmethicMean(numbers, N)
!   geometric  = getGeometricMean (numbers, N)
! 
!   ! Returning of the results
!   write(*,*) " - Considering the aritmethic mean of the numbers: ", aritmethic
!   write(*,*) " - Considering the geometric mean of the numbers: ", geometric
!   write(*,*) " - The absolute distance between this two means: ", aritmethic - geometric
! 
!   ! Ending sequence
!   stop
! 
! end program GeometricMean
! 
! ! Auxiliat program to calculate the factorial of a number
! real function getGeometricMean(numbers_array, length)
!  
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getGeometricMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = product((/(numbers_array(indexer), indexer = 1, length)/))
! 
!   getGeometricMean = accumulator ** (1. / length)
! 
! end function
! 
! real function getAritmethicMean(numbers_array, length)
! 
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getAritmethicMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer), indexer = 1, length)/))
! 
!   getAritmethicMean = accumulator * (1. / length)
! 
! end function

! 28. The code is composed with the last code presented, and reformulated to contain the RMS

! program RMSmean
! 
!   implicit none
! 
!   ! Definition of functions
!   real, external :: getAritmethicMean   ! Aritmethic Mean
!   real, external :: getGeometricMean    ! Geometric Mean
!   real, external :: getRMS              ! RMS mean
! 
!   ! Definition of variables
!   real, allocatable, dimension(:) :: numbers    ! Array of numbers
! 
!   integer                         :: N    = 0     ! Number of terms wanted
!   integer                         :: iter = 0     ! Iterator for numbers reception
! 
!   real :: geometric  = 0     ! Geometric mean value
!   real :: aritmethic = 0     ! Aritmethic mean value
!   real :: rms        = 0     ! RMS mean value
! 
!   ! Welcome sequence
!   write(*,*) "Calculator of the geometric, rms & aritmethic mean of a sequence of numbers"
!   write(*,'(a)', advance= 'no') " Enter the dimension of the sequence you want to consider: N = "
!   read(*,*) N
! 
!   write(*,*) "" ! New line
! 
!   ! Allocation of data
!   allocate(numbers(N))
! 
!   ! Input of elements on the array
!   write(*,'(a)', advance= 'no') " Enter all the terms of the sequence: x = "
!   read(*,*) (numbers(iter), iter = 1, N)
! 
!   ! Calculation of the means
!   aritmethic = getAritmethicMean(numbers, N)
!   geometric  = getGeometricMean (numbers, N)
!   rms        = getRMS           (numbers, N)
! 
!   ! Returning of the results
!   write(*,*) " - Considering the aritmethic mean of the numbers: ", aritmethic
!   write(*,*) " - Considering the geometric mean of the numbers: ", geometric
!   write(*,*) " - Considering the RMS mean of the numbers: ", rms
!   
!   ! Ending sequence
!   stop
! 
! end program RMSmean
! 
! ! Auxiliat program to calculate the factorial of a number
! real function getGeometricMean(numbers_array, length)
!  
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getGeometricMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = product((/(numbers_array(indexer), indexer = 1, length)/))
! 
!   getGeometricMean = accumulator ** (1. / length)
! 
! end function
! 
! real function getAritmethicMean(numbers_array, length)
! 
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getAritmethicMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer), indexer = 1, length)/))
! 
!   getAritmethicMean = accumulator * (1. / length)
! 
! end function
! 
! real function getRMS(numbers_array, length)
! 
! implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getRMS = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer)**2, indexer = 1, length)/))
! 
!   getRMS = sqrt(accumulator * (1. / length))
! 
! end function

! 29. Let's do the same we did with the last code:

! program HarmonicMean
! 
!   implicit none
! 
!   ! Definition of functions
!   real, external :: getAritmethicMean   ! Aritmethic Mean
!   real, external :: getGeometricMean    ! Geometric Mean
!   real, external :: getRMS              ! RMS mean
!   real, external :: getHarmonicMean     ! Harmionic mean
! 
!   ! Definition of variables
!   real, allocatable, dimension(:) :: numbers    ! Array of numbers
! 
!   integer                         :: N    = 0     ! Number of terms wanted
!   integer                         :: iter = 0     ! Iterator for numbers reception
! 
!   real :: geometric  = 0     ! Geometric mean value
!   real :: aritmethic = 0     ! Aritmethic mean value
!   real :: rms        = 0     ! RMS mean value
!   real :: harmonic   = 0     ! Harmonic mean value
! 
!   ! Welcome sequence
!   write(*,*) "Calculator of the geometric, rms & aritmethic mean of a sequence of numbers"
!   write(*,'(a)', advance= 'no') " Enter the dimension of the sequence you want to consider: N = "
!   read(*,*) N
! 
!   write(*,*) "" ! New line
! 
!   ! Allocation of data
!   allocate(numbers(N))
! 
!   ! Input of elements on the array
!   write(*,'(a)', advance= 'no') " Enter all the terms of the sequence: x = "
!   read(*,*) (numbers(iter), iter = 1, N)
! 
!   ! Calculation of the means
!   aritmethic = getAritmethicMean(numbers, N)
!   geometric  = getGeometricMean (numbers, N)
!   rms        = getRMS           (numbers, N)
!   harmonic   = getHarmonicMean  (numbers, N)
! 
!   ! Returning of the results
!   write(*,*) " - Considering the aritmethic mean of the numbers: ", aritmethic
!   write(*,*) " - Considering the geometric mean of the numbers: ", geometric
!   write(*,*) " - Considering the RMS mean of the numbers: ", rms
!   write(*,*) " - Considering the Harmonic mean of the numbers: ", harmonic
!   
!   ! Ending sequence
!   stop
! 
! end program HarmonicMean
! 
! ! Auxiliat program to calculate the factorial of a number
! real function getGeometricMean(numbers_array, length)
!  
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getGeometricMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = product((/(numbers_array(indexer), indexer = 1, length)/))
! 
!   getGeometricMean = accumulator ** (1. / length)
! 
! end function
! 
! real function getAritmethicMean(numbers_array, length)
! 
!   implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getAritmethicMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer), indexer = 1, length)/))
! 
!   getAritmethicMean = accumulator * (1. / length)
! 
! end function
! 
! real function getRMS(numbers_array, length)
! 
! implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getRMS = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer)**2, indexer = 1, length)/))
! 
!   getRMS = sqrt(accumulator * (1. / length))
! 
! end function
! 
! real function getHarmonicMean(numbers_array, length)
! 
! implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getHarmonicMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer)**(-1), indexer = 1, length)/))
! 
!   getHarmonicMean = (length / accumulator)
! 
! end function

! 31. The code is proposed using elements from the last code as expressed below:

! program MeanTimeFailure
! 
!   implicit none
! 
!   ! Definition of functions
!   real, external :: getHarmonicMean     ! Harmionic mean
! 
!   ! Definition of variables
!   real, allocatable, dimension(:) :: MTBF    ! Array of numbers
! 
!   integer :: N    = 0     ! Number of terms wanted
!   integer :: iter = 0     ! Iterator for numbers reception
! 
!   real :: harmonic   = 0     ! Harmonic mean value
! 
!   ! Welcome sequence
!   write(*,*) "Calculator for the Mean Time Between Failure, the harmonic mean is used"
!   write(*,'(a)', advance= 'no') " Enter the number of subsystems to analyse N = "
!   read(*,*) N
! 
!   write(*,*) "" ! New line
! 
!   ! Allocation of data
!   allocate(MTBF(N))
! 
!   ! Input of elements on the array
!   write(*,'(a)', advance= 'no') " Enter the MTBF for each component:  "
!   read(*,*) (MTBF(iter), iter = 1, N)
! 
!   ! Calculation of the harmonic mean
!   harmonic = getHarmonicMean(MTBF, N)
! 
!   ! Returning of the results
!   write(*,*) "  Considering the Harmonic mean the MTBF of the system is: t = ", harmonic
!   
!   ! Ending sequence
!   stop
! 
! end program MeanTimeFailure
! 
! ! Auxiliary function to calculate the factorial of a number
! 
! real function getHarmonicMean(numbers_array, length)
! 
! implicit none
! 
!   ! Definition of variables
!   integer, intent(in)                 :: length          ! Length of the array  
!   real, dimension(length), intent(in) :: numbers_array   ! Array of numbers
! 
!   integer :: indexer     = 0    ! Indexer of the array
!   real    :: accumulator = 0    ! Accumulator of iterations
! 
!   ! Debugging for redundant cases
!   if(length .EQ. 0) then; getHarmonicMean = 0; STOP; endif
! 
!   ! Routine for  Aritmethic Mean
!   accumulator = sum((/( numbers_array(indexer)**(-1), indexer = 1, length)/))
! 
!   getHarmonicMean = (length / accumulator)
! 
! end function

! 32. The following code is proposed

program IdealGasLaw

  implicit none

  ! Definition of parameters
  real, parameter :: R = 8.314      ! Ideal Gas cosntant

  ! Definition of variables
  real :: P = 0       ! Pressure of the gas in KiloPascals (kPa)
  real :: V = 0       ! Volume of the gas in Liters (L)
  real :: n = 1       ! Number of moles in the gas
  real :: T = 273     ! Temperature in Kelvin (K)

  real :: P_initial, P_step, P_final ! Iteration variables (kPa)

  ! Welcome sequence
  write(*,*) "Calculator for the volume of a gas"
  write(*,*) "It's going to be considered that:"

  write(*,'(a)', advance= 'no') " - Sample contains: "
  write(*, '(F11.3)', advance= 'no') n
  write(*,'(a)', advance= 'no') " moles"
  write(*,*) ""

  write(*,'(a)', advance= 'no') " - Sample at temperature conditions: "
  write(*, '(F11.3)', advance= 'no') T
  write(*,'(a)', advance= 'no') " Kelvin"
  write(*,*) ""

  write(*,*) ""
  write(*,'(a)', advance= 'no') " Enter the variation on pressure (kPa), use the notation P_initial, P_step, Pfinal: "
  read(*,*) P_initial, P_step, P_final

  write(*,*) "" ! New line

  ! Control of the iterator: initial step
  P = P_initial

  ! Variation on the pressure
  pressure_iter_1: do

    ! Iteration for the volume
    017 V = n * R * T / P

    ! Printing out sequence:
    write(*,'(a)', advance= 'no') " >> For P = "
    write(*, '(F11.3)', advance= 'no') P
    write(*,'(a)', advance= 'no') " kPa, the Volume found is V = "
    write(*, '(F11.3)', advance= 'no') V
    write(*,'(a)', advance= 'no') " Liters"
    write(*,*) ""

    ! Control flow of the iterator
    if(P .GT. P_final) then; EXIT; endif

    ! Next iteration control
    P = P + P_step
  end do pressure_iter_1 

  ! Returning of the results
  write(*,*) ""
  write(*,*) "The sequence presented shows the evolution of the volume in terms of the pressure"
  write(*,*) ""

  ! Changing of Temperature
  T = T + 300

  write(*,*) "Now let's consider a change in the temperature"
  write(*,*) "It's going to be considered that:"

  write(*,'(a)', advance= 'no') " - Sample contains: "
  write(*, '(F11.3)', advance= 'no') n
  write(*,'(a)', advance= 'no') " moles"
  write(*,*) ""

  write(*,'(a)', advance= 'no') " - Sample at temperature conditions: "
  write(*, '(F11.3)', advance= 'no') T
  write(*,'(a)', advance= 'no') " Kelvin"
  write(*,*) ""

  write(*,*) ""

  ! Control flow: initial condition
  P = P_initial

  ! Variation on the pressure
  pressure_iter_2: do

    ! Iteration for the volume
    018 V = n * R * T / P

    ! Printing out sequence:
    write(*,'(a)', advance= 'no') " >> For P = "
    write(*, '(F11.3)', advance= 'no') P
    write(*,'(a)', advance= 'no') " kPa, the Volume found is V = "
    write(*, '(F11.3)', advance= 'no') V
    write(*,'(a)', advance= 'no') " Liters"
    write(*,*) ""

    ! Control flow of the iterator
    if(P .GT. P_final) then; EXIT; endif

    ! Next iteration control
    P = P + P_step
  end do pressure_iter_2

  ! Ending sequence
  stop

end program IdealGasLaw

