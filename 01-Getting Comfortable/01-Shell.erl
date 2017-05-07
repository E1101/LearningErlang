%% # ###################### #
%% # How to exit from shell #
%% # ###################### #

% > The shell suggests ^G, Ctrl-G
% > Ctrl-C will bring you to a menu.
% > calling a function q that itself calls the init:stop()
q().

%% # #################### #
%% # Moving through Files #
%% # #################### #

pwd().
cd("..").
cd("simonstl").

%% # ##################### #
%% # shell as a calculator #
%% # ##################### #

2+2.
27-14.
35*42023943.
200/15.
200 div 15. % division for integers
200 rem 15. % reminder (mod)
3*(4+15).

round(200/15).

% calculations on integers using a base other than 10
2#1010111. % = 87
16#cafe.   % = 51966

-16#cafe.  % = -51966


%% # ############### #
%% # Bound Variables #
%% # ############### #

% > Seeing bound variables
b().
% N = 1
%Number = 5

% > Clearing Bound Variables
f(N).
%Number = 5
f(). % with no arguments clear all variables



