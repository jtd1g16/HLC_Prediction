# load "ref2431-108-20-3.gnu"
# chem = "diisopropyl ether"

set terminal postscript eps color
set title "ref = 2431; chem = diisopropyl ether; casrn = 108-20-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3103042E-02 * exp(  -6368.116    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1441337E-01 point
set label "" at    298.1500    ,   0.3103042E-02 point
set label "" at    298.1500    ,   0.3103042E-02 point ps 2 pt 6

plot [270:300] H(T)
