# load "ref0483-107-02-8.gnu"
# chem = "propenal"

set terminal postscript eps color
set title "ref = 483; chem = propenal; casrn = 107-02-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7202014E-01 * exp(  -5139.219    *(1/   298.    -1/T))

set label "" at    273.0000    ,   0.3524465     point
set label "" at    298.0000    ,   0.7264774E-01 point
set label "" at    298.1500    ,   0.7202014E-01 point ps 2 pt 6

plot [270:300] H(T)
