# load "ref2431-637-92-3.gnu"
# chem = "ethyl {tert}-butyl ether"

set terminal postscript eps color
set title "ref = 2431; chem = ethyl {tert}-butyl ether; casrn = 637-92-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3667232E-02 * exp(  -7569.381    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2275795E-01 point
set label "" at    298.1500    ,   0.3667232E-02 point
set label "" at    298.1500    ,   0.3667232E-02 point ps 2 pt 6

plot [270:300] H(T)
