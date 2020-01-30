# load "ref2484-75-73-0-fromx.gnu"
# chem = "tetrafluoromethane"

set terminal postscript eps color
set title "ref = 2484; chem = tetrafluoromethane; casrn = 75-73-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2040247E-05 * exp(  -2308.682    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3648497E-05 point
set label "" at    283.1500    ,   0.3069544E-05 point
set label "" at    288.1500    ,   0.2605289E-05 point
set label "" at    293.1500    ,   0.2272118E-05 point
set label "" at    298.1500    ,   0.2042721E-05 point
set label "" at    303.1500    ,   0.1840634E-05 point
set label "" at    298.1500    ,   0.2040247E-05 point ps 2 pt 6

plot [270:310] H(T)
