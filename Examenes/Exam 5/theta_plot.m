% Clear of previous values
clear

% Definition of intervals on program
intervals = 100000;

% Open of the file
file = fopen("theta_values.txt", 'r');

% Scan of values on file
matrix_values = fscanf(file, '%f', [3, intervals + 1])';

% Close of file
fclose(file);

% Definition of contents
T     = matrix_values(:, 1);
theta = matrix_values(:, 2);
omega = matrix_values(:, 3);

% Definition of the problems values
L = .5; g = 9.78;

% Definition of expected behaviour values
Period    = 2 * pi * sqrt(L / g);
frecuency = 1 / Period;
 
f  = @(x) pi/4 * cos(pi/frecuency * x);

df = @(x) - pi/frecuency * pi/4 * sin(pi/frecuency * x);

% Clear of previous plots
clf('reset')

hold on

% Plot of theta vs time
plot(T, theta, 'b', DisplayName= '\theta(t)')

% Plot of expected theta behaviour
fplot(f, [0, 5], DisplayName= '\theta_{expected}(t)')

% Plot of omega vs time
plot(T, omega, 'r', DisplayName= '\omega(t)')

% Plot of expected omega behaviour
fplot(df, [0, 5], DisplayName= '\omega_{expected}(t)')

% Plot of the Periods ticks
plot([1 * Period, 1 * Period], [-pi/4, pi/4], ...
    'k', LineStyle= ':', DisplayName = 'Expected Period')

plot([2 * Period, 2 * Period], [-pi/4, pi/4], ...
    'k', LineStyle= ':', HandleVisibility='off')

plot([3 * Period, 3 * Period], [-pi/4, pi/4], ...
    'k', LineStyle= ':', HandleVisibility='off')

% Auxiliar definitions for better visualization
yline(0, HandleVisibility = 'off')

title('Behaviour present on the theta values file')
subtitle('Comparation of expected vs obtained')

xlabel('time(seconds)')

legend(Location = 'bestoutside', Interpreter= 'tex')

grid on
