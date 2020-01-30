# load "ref2122-4549-32-0.gnu"
# chem = "1,8-dibromooctane"

set terminal postscript eps color
set title "ref = 2122; chem = 1,8-dibromooctane; casrn = 4549-32-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1369501E-01 * exp(  -7266.205    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1363101     point
set label "" at    278.1500    ,   0.7997386E-01 point
set label "" at    283.1500    ,   0.4854553E-01 point
set label "" at    288.1500    ,   0.3057564E-01 point
set label "" at    293.1500    ,   0.1983581E-01 point
set label "" at    298.1500    ,   0.1320809E-01 point
set label "" at    303.1500    ,   0.9042796E-02 point
set label "" at    308.1500    ,   0.6339279E-02 point
set label "" at    313.1500    ,   0.4536222E-02 point
set label "" at    298.1500    ,   0.1369501E-01 point ps 2 pt 6

plot [270:320] H(T)
