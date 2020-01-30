# load "ref0379-7439-90-9.gnu"
# chem = "krypton"

set terminal postscript eps color
set title "ref = 379; chem = krypton; casrn = 7439-90-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2484571E-04 * exp(  -2133.880    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4865494E-04 point
set label "" at    283.1500    ,   0.3566561E-04 point
set label "" at    293.1500    ,   0.2756379E-04 point
set label "" at    303.1500    ,   0.2250016E-04 point
set label "" at    298.1500    ,   0.2484571E-04 point ps 2 pt 6

plot [270:310] H(T)
