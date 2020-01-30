# load "ref0477-74-84-0.gnu"
# chem = "ethane"

set terminal postscript eps color
set title "ref = 477; chem = ethane; casrn = 74-84-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1875525E-04 * exp(  -2347.050    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2488406E-04 point
set label "" at    293.1500    ,   0.2133934E-04 point
set label "" at    298.1500    ,   0.1857566E-04 point
set label "" at    303.1500    ,   0.1639639E-04 point
set label "" at    308.1500    ,   0.1467045E-04 point
set label "" at    298.1500    ,   0.1875525E-04 point ps 2 pt 6

plot [280:310] H(T)
