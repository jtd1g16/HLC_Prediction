# load "ref2610-76-16-4.gnu"
# chem = "hexafluoroethane"

set terminal postscript eps color
set title "ref = 2610; chem = hexafluoroethane; casrn = 76-16-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1171891E-05 * exp(  -1680.000    *(1/   298.    -1/T))

set label "" at    280.0000    ,   0.1688551E-05 point
set label "" at    300.0000    ,   0.1131870E-05 point
set label "" at    298.1500    ,   0.1171891E-05 point ps 2 pt 6

plot [280:300] H(T)
