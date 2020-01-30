# load "ref2496-2050-68-2.gnu"
# chem = "4,4'-dichlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 4,4'-dichlorobiphenyl; casrn = 2050-68-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3479727E-01 * exp(  -5311.594    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1000000     point
set label "" at    288.1500    ,   0.7692308E-01 point
set label "" at    298.1500    ,   0.5000000E-01 point
set label "" at    308.1500    ,   0.1428571E-01 point
set label "" at    298.1500    ,   0.3479727E-01 point ps 2 pt 6

plot [270:310] H(T)
