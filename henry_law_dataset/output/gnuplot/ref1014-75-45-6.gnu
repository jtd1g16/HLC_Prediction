# load "ref1014-75-45-6.gnu"
# chem = "chlorodifluoromethane"

set terminal postscript eps color
set title "ref = 1014; chem = chlorodifluoromethane; casrn = 75-45-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3437338E-03 * exp(  -2752.822    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.6742857E-03 point
set label "" at    288.1500    ,   0.5079421E-03 point
set label "" at    293.1500    ,   0.4003571E-03 point
set label "" at    298.1500    ,   0.3257143E-03 point
set label "" at    303.1500    ,   0.2713008E-03 point
set label "" at    308.1500    ,   0.2315318E-03 point
set label "" at    313.1500    ,   0.2012266E-03 point
set label "" at    318.1500    ,   0.1773890E-03 point
set label "" at    323.1500    ,   0.1586009E-03 point
set label "" at    328.1500    ,   0.1432334E-03 point
set label "" at    333.1500    ,   0.1305808E-03 point
set label "" at    338.1500    ,   0.1199822E-03 point
set label "" at    343.1500    ,   0.1110817E-03 point
set label "" at    348.1500    ,   0.1035035E-03 point
set label "" at    298.1500    ,   0.3437338E-03 point ps 2 pt 6

plot [280:350] H(T)
