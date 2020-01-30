# load "ref2121-75-68-3.gnu"
# chem = "1-chloro-1,1-difluoroethane"

set terminal postscript eps color
set title "ref = 2121; chem = 1-chloro-1,1-difluoroethane; casrn = 75-68-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1527866E-03 * exp(  -3016.874    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.3103532E-03 point
set label "" at    285.1500    ,   0.2413015E-03 point
set label "" at    295.1500    ,   0.1678441E-03 point
set label "" at    303.1500    ,   0.1236746E-03 point
set label "" at    313.1500    ,   0.9771517E-04 point
set label "" at    298.1500    ,   0.1527866E-03 point ps 2 pt 6

plot [270:320] H(T)
