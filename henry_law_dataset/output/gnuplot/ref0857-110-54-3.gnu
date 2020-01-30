# load "ref0857-110-54-3.gnu"
# chem = "hexane"

set terminal postscript eps color
set title "ref = 857; chem = hexane; casrn = 110-54-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2405590E-03 * exp(  -8695.176    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.5377018E-04 point
set label "" at    333.1500    ,   0.1552367E-04 point
set label "" at    343.1500    ,   0.4205940E-05 point
set label "" at    298.1500    ,   0.2405590E-03 point ps 2 pt 6

plot [310:350] H(T)
