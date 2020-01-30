# load "ref0477-7791-21-1.gnu"
# chem = "dichlorine monoxide"

set terminal postscript eps color
set title "ref = 477; chem = dichlorine monoxide; casrn = 7791-21-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1678187     * exp(  -1672.708    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2867456     point
set label "" at    276.6100    ,   0.2479667     point
set label "" at    283.1500    ,   0.2333836     point
set label "" at    293.1500    ,   0.1831349     point
set label "" at    298.1500    ,   0.1678187     point ps 2 pt 6

plot [270:300] H(T)
