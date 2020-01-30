# load "ref2496-2050-47-7.gnu"
# chem = "4,4'-dibromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 4,4'-dibromodiphenyl ether; casrn = 2050-47-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7321750E-01 * exp(  -4536.013    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2173913     point
set label "" at    288.1500    ,   0.1176471     point
set label "" at    298.1500    ,   0.8333333E-01 point
set label "" at    308.1500    ,   0.4166667E-01 point
set label "" at    298.1500    ,   0.7321750E-01 point ps 2 pt 6

plot [270:310] H(T)
