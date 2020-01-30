# load "ref0379-7440-63-3.gnu"
# chem = "xenon"

set terminal postscript eps color
set title "ref = 379; chem = xenon; casrn = 7440-63-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4854326E-04 * exp(  -2536.823    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1065565E-03 point
set label "" at    283.1500    ,   0.7661501E-04 point
set label "" at    293.1500    ,   0.5415889E-04 point
set label "" at    303.1500    ,   0.4315098E-04 point
set label "" at    298.1500    ,   0.4854326E-04 point ps 2 pt 6

plot [270:310] H(T)
