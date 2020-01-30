# load "ref2993-19430-93-4.gnu"
# chem = "4:2 FTO"

set terminal postscript eps color
set title "ref = 2993; chem = 4:2 FTO; casrn = 19430-93-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2510062E-05 * exp(  -4109.391    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.6850638E-05 point
set label "" at    288.1500    ,   0.3942131E-05 point
set label "" at    298.1500    ,   0.2545253E-05 point
set label "" at    298.1500    ,   0.2510062E-05 point ps 2 pt 6

plot [270:300] H(T)
