# load "ref2993-30389-25-4.gnu"
# chem = "10:2 FTO"

set terminal postscript eps color
set title "ref = 2993; chem = 10:2 FTO; casrn = 30389-25-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3252505E-07 * exp(  -6507.507    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1643354E-06 point
set label "" at    288.1500    ,   0.6247856E-07 point
set label "" at    298.1500    ,   0.3433452E-07 point
set label "" at    298.1500    ,   0.3252505E-07 point ps 2 pt 6

plot [270:300] H(T)
