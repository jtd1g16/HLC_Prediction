# load "ref2496-5436-43-1.gnu"
# chem = "2,2',4,4'-tetrabromodiphenyl ether"

set terminal postscript eps color
set title "ref = 2496; chem = 2,2',4,4'-tetrabromodiphenyl ether; casrn = 5436-43-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1657418     * exp(  -621.3618    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1041667     point
set label "" at    288.1500    ,   0.4761905     point
set label "" at    298.1500    ,   0.1562500     point
set label "" at    308.1500    ,   0.1136364     point
set label "" at    298.1500    ,   0.1657418     point ps 2 pt 6

plot [270:310] H(T)
