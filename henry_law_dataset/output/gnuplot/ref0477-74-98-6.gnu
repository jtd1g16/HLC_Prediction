# load "ref0477-74-98-6.gnu"
# chem = "propane"

set terminal postscript eps color
set title "ref = 477; chem = propane; casrn = 74-98-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1508756E-04 * exp(  -2674.883    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2082593E-04 point
set label "" at    293.1500    ,   0.1747783E-04 point
set label "" at    298.1500    ,   0.1492170E-04 point
set label "" at    303.1500    ,   0.1294452E-04 point
set label "" at    308.1500    ,   0.1140428E-04 point
set label "" at    298.1500    ,   0.1508756E-04 point ps 2 pt 6

plot [280:310] H(T)
