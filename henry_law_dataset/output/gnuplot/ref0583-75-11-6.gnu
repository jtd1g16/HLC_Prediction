# load "ref0583-75-11-6.gnu"
# chem = "diiodomethane"

set terminal postscript eps color
set title "ref = 583; chem = diiodomethane; casrn = 75-11-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2282324E-01 * exp(  -5270.383    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1158727     point
set label "" at    283.1500    ,   0.5740075E-01 point
set label "" at    293.1500    ,   0.3108150E-01 point
set label "" at    298.1500    ,   0.2282324E-01 point ps 2 pt 6

plot [270:300] H(T)
