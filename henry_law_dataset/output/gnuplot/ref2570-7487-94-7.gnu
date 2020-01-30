# load "ref2570-7487-94-7.gnu"
# chem = "mercury dichloride"

set terminal postscript eps color
set title "ref = 2570; chem = mercury dichloride; casrn = 7487-94-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    13835.34     * exp(  -9459.391    *(1/   298.    -1/T))

set label "" at    333.0000    ,    500.0000     point
set label "" at    353.0000    ,    100.0000     point
set label "" at    298.1500    ,    13835.34     point ps 2 pt 6

plot [330:360] H(T)
