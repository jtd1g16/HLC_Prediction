# load "ref2830-75-71-8.gnu"
# chem = "dichlorodifluoromethane"

set terminal postscript eps color
set title "ref = 2830; chem = dichlorodifluoromethane; casrn = 75-71-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2333618E-04 * exp(  -3368.646    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.6736837E-04 point
set label "" at    275.7500    ,   0.5856205E-04 point
set label "" at    278.7500    ,   0.5151699E-04 point
set label "" at    283.5500    ,   0.4147778E-04 point
set label "" at    288.3500    ,   0.3403644E-04 point
set label "" at    291.1500    ,   0.2844442E-04 point
set label "" at    292.8500    ,   0.2844442E-04 point
set label "" at    293.8500    ,   0.2844442E-04 point
set label "" at    298.3500    ,   0.2223597E-04 point
set label "" at    298.6500    ,   0.2342482E-04 point
set label "" at    304.7500    ,   0.1910972E-04 point
set label "" at    298.1500    ,   0.2333618E-04 point ps 2 pt 6

plot [270:310] H(T)
