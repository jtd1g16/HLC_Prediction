# load "ref1776-2551-62-4.gnu"
# chem = "sulfur hexafluoride"

set terminal postscript eps color
set title "ref = 1776; chem = sulfur hexafluoride; casrn = 2551-62-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2391871E-05 * exp(  -2903.174    *(1/   298.    -1/T))

set label "" at    273.3300    ,   0.6112509E-05 point
set label "" at    273.4000    ,   0.6043030E-05 point
set label "" at    273.4300    ,   0.5996447E-05 point
set label "" at    273.5300    ,   0.6076980E-05 point
set label "" at    283.4200    ,   0.3854725E-05 point
set label "" at    283.4700    ,   0.3883642E-05 point
set label "" at    283.4900    ,   0.3858179E-05 point
set label "" at    283.5100    ,   0.3808340E-05 point
set label "" at    283.5500    ,   0.3893511E-05 point
set label "" at    293.1600    ,   0.2725290E-05 point
set label "" at    293.1600    ,   0.2659265E-05 point
set label "" at    293.2100    ,   0.2674562E-05 point
set label "" at    293.3200    ,   0.2658673E-05 point
set label "" at    303.9700    ,   0.1958154E-05 point
set label "" at    304.4200    ,   0.1924895E-05 point
set label "" at    304.5300    ,   0.1932494E-05 point
set label "" at    304.6300    ,   0.1909697E-05 point
set label "" at    305.3100    ,   0.1898643E-05 point
set label "" at    312.2400    ,   0.1607402E-05 point
set label "" at    312.2600    ,   0.1542561E-05 point
set label "" at    312.2600    ,   0.1601678E-05 point
set label "" at    312.5300    ,   0.1560523E-05 point
set label "" at    312.6700    ,   0.1575130E-05 point
set label "" at    312.7800    ,   0.1567333E-05 point
set label "" at    312.8700    ,   0.1548187E-05 point
set label "" at    298.1500    ,   0.2391871E-05 point ps 2 pt 6

plot [270:320] H(T)
