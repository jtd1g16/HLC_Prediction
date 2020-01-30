# load "ref2496-2437-79-8.gnu"
# chem = "2,2',4,4'-tetrachlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 2,2',4,4'-tetrachlorobiphenyl; casrn = 2437-79-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1835641     * exp(   6031.658    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.5000000E-01 point
set label "" at    288.1500    ,   0.6666667E-01 point
set label "" at    298.1500    ,   0.2127660     point
set label "" at    308.1500    ,   0.3571429     point
set label "" at    298.1500    ,   0.1835641     point ps 2 pt 6

plot [270:310] H(T)
