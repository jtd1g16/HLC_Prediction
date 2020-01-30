# load "ref0477-7782-44-7.gnu"
# chem = "oxygen"

set terminal postscript eps color
set title "ref = 477; chem = oxygen; casrn = 7782-44-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1260857E-04 * exp(  -1464.413    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1505278E-04 point
set label "" at    293.1500    ,   0.1366002E-04 point
set label "" at    298.1500    ,   0.1252396E-04 point
set label "" at    303.1500    ,   0.1158999E-04 point
set label "" at    308.1500    ,   0.1082533E-04 point
set label "" at    298.1500    ,   0.1260857E-04 point ps 2 pt 6

plot [280:310] H(T)
