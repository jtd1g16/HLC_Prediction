# load "ref0484-78-98-8.gnu"
# chem = "propanonal"

set terminal postscript eps color
set title "ref = 484; chem = propanonal; casrn = 78-98-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    34.32966     * exp(  -7545.319    *(1/   298.    -1/T))

set label "" at    288.1500    ,    83.59240     point
set label "" at    298.1500    ,    36.61485     point
set label "" at    308.1500    ,    12.73131     point
set label "" at    318.1500    ,    7.688132     point
set label "" at    298.1500    ,    34.32966     point ps 2 pt 6

plot [280:320] H(T)
