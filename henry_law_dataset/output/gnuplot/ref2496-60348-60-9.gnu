# load "ref2496-60348-60-9.gnu"
# chem = "2,2',4,4',5-pentabromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 2,2',4,4',5-pentabromodiphenyl ether; casrn = 60348-60-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2706393     * exp(   6650.617    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2083333E-01 point
set label "" at    288.1500    ,   0.3703704     point
set label "" at    298.1500    ,   0.6250000     point
set label "" at    308.1500    ,   0.2127660     point
set label "" at    298.1500    ,   0.2706393     point ps 2 pt 6

plot [270:310] H(T)
