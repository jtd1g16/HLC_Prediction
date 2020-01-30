# load "ref2484-115-25-3-fromx.gnu"
# chem = "octafluorocyclobutane"

set terminal postscript eps color
set title "ref = 2484; chem = octafluorocyclobutane; casrn = 115-25-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1201820E-05 * exp(  -3759.486    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3129624E-05 point
set label "" at    283.1500    ,   0.2343122E-05 point
set label "" at    288.1500    ,   0.1753245E-05 point
set label "" at    293.1500    ,   0.1430997E-05 point
set label "" at    298.1500    ,   0.1201601E-05 point
set label "" at    303.1500    ,   0.1026823E-05 point
set label "" at    298.1500    ,   0.1201820E-05 point ps 2 pt 6

plot [270:310] H(T)
