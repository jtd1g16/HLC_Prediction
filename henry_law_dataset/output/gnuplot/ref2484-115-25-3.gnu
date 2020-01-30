# load "ref2484-115-25-3.gnu"
# chem = "octafluorocyclobutane"

set terminal postscript eps color
set title "ref = 2484; chem = octafluorocyclobutane; casrn = 115-25-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1169826E-05 * exp(  -3750.273    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3026808E-05 point
set label "" at    283.1500    ,   0.2272496E-05 point
set label "" at    288.1500    ,   0.1728015E-05 point
set label "" at    293.1500    ,   0.1386732E-05 point
set label "" at    298.1500    ,   0.1165813E-05 point
set label "" at    303.1500    ,   0.9997900E-06 point
set label "" at    298.1500    ,   0.1169826E-05 point ps 2 pt 6

plot [270:310] H(T)
