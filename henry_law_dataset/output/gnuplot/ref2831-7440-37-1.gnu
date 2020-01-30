# load "ref2831-7440-37-1.gnu"
# chem = "argon"

set terminal postscript eps color
set title "ref = 2831; chem = argon; casrn = 7440-37-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1428653E-04 * exp(  -1430.691    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2089617E-04 point
set label "" at    283.1500    ,   0.1856950E-04 point
set label "" at    288.1500    ,   0.1668182E-04 point
set label "" at    293.1500    ,   0.1514534E-04 point
set label "" at    298.1500    ,   0.1391615E-04 point
set label "" at    308.1500    ,   0.1198457E-04 point
set label "" at    318.1500    ,   0.1066758E-04 point
set label "" at    323.1500    ,   0.1014079E-04 point
set label "" at    298.1500    ,   0.1428653E-04 point ps 2 pt 6

plot [270:330] H(T)
