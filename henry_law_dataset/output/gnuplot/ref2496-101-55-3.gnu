# load "ref2496-101-55-3.gnu"
# chem = "4-bromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 4-bromodiphenyl ether; casrn = 101-55-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5842004E-01 * exp(  -5459.662    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2127660     point
set label "" at    288.1500    ,   0.1234568     point
set label "" at    298.1500    ,   0.5000000E-01 point
set label "" at    308.1500    ,   0.3448276E-01 point
set label "" at    298.1500    ,   0.5842004E-01 point ps 2 pt 6

plot [270:310] H(T)
