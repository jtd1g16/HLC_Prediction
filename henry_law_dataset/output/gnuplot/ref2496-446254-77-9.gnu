# load "ref2496-446254-77-9.gnu"
# chem = "2,3',4,4',5-Pentabromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 2,3',4,4',5-Pentabromodiphenyl ether; casrn = 446254-77-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8777164     * exp(  -4045.644    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.9090909     point
set label "" at    288.1500    ,    7.142857     point
set label "" at    298.1500    ,   0.6250000     point
set label "" at    308.1500    ,   0.4000000     point
set label "" at    298.1500    ,   0.8777164     point ps 2 pt 6

plot [270:310] H(T)
