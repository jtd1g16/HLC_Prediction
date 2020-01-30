# load "ref2496-41318-75-6.gnu"
# chem = "2,4,4'-tribromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 2,4,4'-tribromodiphenyl ether; casrn = 41318-75-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1198087     * exp(  -11726.36    *(1/   298.    -1/T))

set label "" at    278.1500    ,    5.555556     point
set label "" at    288.1500    ,   0.1041667     point
set label "" at    298.1500    ,   0.1052632     point
set label "" at    308.1500    ,   0.6250000E-01 point
set label "" at    298.1500    ,   0.1198087     point ps 2 pt 6

plot [270:310] H(T)
