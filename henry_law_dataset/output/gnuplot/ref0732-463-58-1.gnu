# load "ref0732-463-58-1.gnu"
# chem = "carbon oxide sulfide"

set terminal postscript eps color
set title "ref = 732; chem = carbon oxide sulfide; casrn = 463-58-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2099960E-03 * exp(  -3292.586    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.5913083E-03 point
set label "" at    278.1500    ,   0.4658270E-03 point
set label "" at    283.1500    ,   0.3702930E-03 point
set label "" at    288.1500    ,   0.3007960E-03 point
set label "" at    293.1500    ,   0.2497584E-03 point
set label "" at    298.1500    ,   0.2110312E-03 point
set label "" at    303.1500    ,   0.1793857E-03 point
set label "" at    298.1500    ,   0.2099960E-03 point ps 2 pt 6

plot [270:310] H(T)
