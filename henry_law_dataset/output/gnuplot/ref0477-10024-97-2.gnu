# load "ref0477-10024-97-2.gnu"
# chem = "dinitrogen monoxide"

set terminal postscript eps color
set title "ref = 477; chem = dinitrogen monoxide; casrn = 10024-97-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2399667E-03 * exp(  -2551.845    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.3248692E-03 point
set label "" at    293.1500    ,   0.2768051E-03 point
set label "" at    298.1500    ,   0.2385178E-03 point
set label "" at    303.1500    ,   0.2078223E-03 point
set label "" at    308.1500    ,   0.1828618E-03 point
set label "" at    298.1500    ,   0.2399667E-03 point ps 2 pt 6

plot [280:310] H(T)
