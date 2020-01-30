# load "ref0379-7440-01-9.gnu"
# chem = "neon"

set terminal postscript eps color
set title "ref = 379; chem = neon; casrn = 7440-01-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4540048E-05 * exp(  -642.5733    *(1/   298.    -1/T))

set label "" at    282.1500    ,   0.5151699E-05 point
set label "" at    293.1500    ,   0.4667351E-05 point
set label "" at    303.1500    ,   0.4403162E-05 point
set label "" at    298.1500    ,   0.4540048E-05 point ps 2 pt 6

plot [280:310] H(T)
