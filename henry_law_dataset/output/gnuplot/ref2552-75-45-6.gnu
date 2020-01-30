# load "ref2552-75-45-6.gnu"
# chem = "chlorodifluoromethane"

set terminal postscript eps color
set title "ref = 2552; chem = chlorodifluoromethane; casrn = 75-45-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3465170E-03 * exp(  -2551.849    *(1/   298.    -1/T))

set label "" at    283.4000    ,   0.5980755E-03 point
set label "" at    296.7000    ,   0.3493708E-03 point
set label "" at    314.1000    ,   0.1983716E-03 point
set label "" at    332.4000    ,   0.1361954E-03 point
set label "" at    352.3000    ,   0.1036269E-03 point
set label "" at    298.1500    ,   0.3465170E-03 point ps 2 pt 6

plot [280:360] H(T)
