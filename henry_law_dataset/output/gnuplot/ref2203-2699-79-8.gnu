# load "ref2203-2699-79-8.gnu"
# chem = "sulfuryl fluoride"

set terminal postscript eps color
set title "ref = 2203; chem = sulfuryl fluoride; casrn = 2699-79-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8913866E-04 * exp(  -3129.020    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2329273E-03 point
set label "" at    296.4500    ,   0.9466798E-04 point
set label "" at    298.1500    ,   0.8913866E-04 point ps 2 pt 6

plot [270:300] H(T)
