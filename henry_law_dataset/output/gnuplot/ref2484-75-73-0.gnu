# load "ref2484-75-73-0.gnu"
# chem = "tetrafluoromethane"

set terminal postscript eps color
set title "ref = 2484; chem = tetrafluoromethane; casrn = 75-73-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2032542E-05 * exp(  -2321.790    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3640817E-05 point
set label "" at    283.1500    ,   0.3071055E-05 point
set label "" at    288.1500    ,   0.2600371E-05 point
set label "" at    293.1500    ,   0.2264723E-05 point
set label "" at    298.1500    ,   0.2033113E-05 point
set label "" at    303.1500    ,   0.1832948E-05 point
set label "" at    298.1500    ,   0.2032542E-05 point ps 2 pt 6

plot [270:310] H(T)
