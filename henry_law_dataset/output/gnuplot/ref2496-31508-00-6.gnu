# load "ref2496-31508-00-6.gnu"
# chem = "2,3',4,4',5-pentachlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 2,3',4,4',5-pentachlorobiphenyl; casrn = 31508-00-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1846398E-01 * exp(  -14014.24    *(1/   298.    -1/T))

set label "" at    278.1500    ,    1.666667     point
set label "" at    288.1500    ,   0.2083333E-01 point
set label "" at    298.1500    ,   0.1136364E-01 point
set label "" at    308.1500    ,   0.9615385E-02 point
set label "" at    298.1500    ,   0.1846398E-01 point ps 2 pt 6

plot [270:310] H(T)
