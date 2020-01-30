# load "ref2496-189084-64-8.gnu"
# chem = "2,2',4,4',6-pentabromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 2,2',4,4',6-pentabromodiphenyl ether; casrn = 189084-64-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1910185     * exp(  -12.33623    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.6250000E-01 point
set label "" at    288.1500    ,   0.8333333     point
set label "" at    298.1500    ,   0.3333333     point
set label "" at    308.1500    ,   0.7692308E-01 point
set label "" at    298.1500    ,   0.1910185     point ps 2 pt 6

plot [270:310] H(T)
