# load "ref2936-1678-91-7.gnu"
# chem = "ethylcyclohexane"

set terminal postscript eps color
set title "ref = 2936; chem = ethylcyclohexane; casrn = 1678-91-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2110298E-04 * exp(  -4718.867    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.9592945E-04 point
set label "" at    278.1500    ,   0.6662575E-04 point
set label "" at    283.1500    ,   0.4783396E-04 point
set label "" at    288.1500    ,   0.3507521E-04 point
set label "" at    293.1500    ,   0.2629973E-04 point
set label "" at    298.1500    ,   0.2037351E-04 point
set label "" at    303.1500    ,   0.1599766E-04 point
set label "" at    308.1500    ,   0.1288134E-04 point
set label "" at    313.1500    ,   0.1055145E-04 point
set label "" at    298.1500    ,   0.2110298E-04 point ps 2 pt 6

plot [270:320] H(T)
