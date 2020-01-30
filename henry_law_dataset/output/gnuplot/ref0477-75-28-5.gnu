# load "ref0477-75-28-5.gnu"
# chem = "2-methylpropane"

set terminal postscript eps color
set title "ref = 477; chem = 2-methylpropane; casrn = 75-28-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9198777E-05 * exp(  -2673.186    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1274243E-04 point
set label "" at    293.1500    ,   0.1063417E-04 point
set label "" at    298.1500    ,   0.9061162E-05 point
set label "" at    303.1500    ,   0.7881409E-05 point
set label "" at    308.1500    ,   0.6980208E-05 point
set label "" at    298.1500    ,   0.9198777E-05 point ps 2 pt 6

plot [280:310] H(T)
