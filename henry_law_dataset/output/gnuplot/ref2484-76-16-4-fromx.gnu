# load "ref2484-76-16-4-fromx.gnu"
# chem = "hexafluoroethane"

set terminal postscript eps color
set title "ref = 2484; chem = hexafluoroethane; casrn = 76-16-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5703059E-06 * exp(  -2858.520    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1174292E-05 point
set label "" at    283.1500    ,   0.9448952E-06 point
set label "" at    288.1500    ,   0.7919642E-06 point
set label "" at    293.1500    ,   0.6281095E-06 point
set label "" at    298.1500    ,   0.5625676E-06 point
set label "" at    303.1500    ,   0.5145036E-06 point
set label "" at    298.1500    ,   0.5703059E-06 point ps 2 pt 6

plot [270:310] H(T)
