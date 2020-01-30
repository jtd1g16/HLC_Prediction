# load "ref2939-75-68-3.gnu"
# chem = "1-chloro-1,1-difluoroethane"

set terminal postscript eps color
set title "ref = 2939; chem = 1-chloro-1,1-difluoroethane; casrn = 75-68-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1473836E-03 * exp(  -2626.119    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.1480385E-03 point
set label "" at    353.0000    ,   0.3750308E-04 point
set label "" at    298.1500    ,   0.1473836E-03 point ps 2 pt 6

plot [290:360] H(T)
