# load "ref2936-2207-01-4.gnu"
# chem = "{cis}-1,2-dimethylcyclohexane"

set terminal postscript eps color
set title "ref = 2936; chem = {cis}-1,2-dimethylcyclohexane; casrn = 2207-01-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2860469E-04 * exp(  -4900.027    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1384642E-03 point
set label "" at    278.1500    ,   0.9461731E-04 point
set label "" at    283.1500    ,   0.6668219E-04 point
set label "" at    288.1500    ,   0.4819803E-04 point
set label "" at    293.1500    ,   0.3598911E-04 point
set label "" at    298.1500    ,   0.2744187E-04 point
set label "" at    303.1500    ,   0.2144552E-04 point
set label "" at    308.1500    ,   0.1719404E-04 point
set label "" at    313.1500    ,   0.1396628E-04 point
set label "" at    298.1500    ,   0.2860469E-04 point ps 2 pt 6

plot [270:320] H(T)
