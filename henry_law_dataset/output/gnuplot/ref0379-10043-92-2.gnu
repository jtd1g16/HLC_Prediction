# load "ref0379-10043-92-2.gnu"
# chem = "radon"

set terminal postscript eps color
set title "ref = 379; chem = radon; casrn = 10043-92-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8324287E-04 * exp(  -3172.344    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2245612E-03 point
set label "" at    283.1500    ,   0.1435431E-03 point
set label "" at    293.1500    ,   0.9775019E-04 point
set label "" at    303.1500    ,   0.7133122E-04 point
set label "" at    298.1500    ,   0.8324287E-04 point ps 2 pt 6

plot [270:310] H(T)
