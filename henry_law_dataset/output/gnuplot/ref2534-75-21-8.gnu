# load "ref2534-75-21-8.gnu"
# chem = "oxirane"

set terminal postscript eps color
set title "ref = 2534; chem = oxirane; casrn = 75-21-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5800395E-01 * exp(  -3212.388    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1030532     point
set label "" at    293.1500    ,   0.6913699E-01 point
set label "" at    303.1500    ,   0.4876627E-01 point
set label "" at    298.1500    ,   0.5800395E-01 point ps 2 pt 6

plot [280:310] H(T)
