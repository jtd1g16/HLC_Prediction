# load "ref2496-2051-62-9.gnu"
# chem = "4-chlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 4-chlorobiphenyl; casrn = 2051-62-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3507821E-01 * exp(  -6709.279    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1754386     point
set label "" at    288.1500    ,   0.7692308E-01 point
set label "" at    298.1500    ,   0.3571429E-01 point
set label "" at    308.1500    ,   0.1666667E-01 point
set label "" at    298.1500    ,   0.3507821E-01 point ps 2 pt 6

plot [270:310] H(T)
