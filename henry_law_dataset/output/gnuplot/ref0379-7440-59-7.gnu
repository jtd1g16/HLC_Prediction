# load "ref0379-7440-59-7.gnu"
# chem = "helium"

set terminal postscript eps color
set title "ref = 379; chem = helium; casrn = 7440-59-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3740944E-05 * exp(  -436.0498    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4315098E-05 point
set label "" at    283.1500    ,   0.4011280E-05 point
set label "" at    293.1500    ,   0.3786719E-05 point
set label "" at    303.1500    ,   0.3694253E-05 point
set label "" at    298.1500    ,   0.3740944E-05 point ps 2 pt 6

plot [270:310] H(T)
