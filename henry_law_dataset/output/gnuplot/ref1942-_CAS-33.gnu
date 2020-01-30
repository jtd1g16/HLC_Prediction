# load "ref1942-_CAS-33.gnu"
# chem = "4-(3',5'-dimethyl-3'-heptyl)-phenol(+)"

set terminal postscript eps color
set title "ref = 1942; chem = 4-(3',5'-dimethyl-3'-heptyl)-phenol(+); casrn = _CAS-33"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    2.858053     * exp(  -8722.157    *(1/   298.    -1/T))

set label "" at    278.0000    ,    21.22872     point
set label "" at    283.0000    ,    14.54725     point
set label "" at    288.0000    ,    8.388848     point
set label "" at    293.0000    ,    5.852455     point
set label "" at    298.0000    ,    2.388354     point
set label "" at    298.1500    ,    2.858053     point ps 2 pt 6

plot [270:300] H(T)
