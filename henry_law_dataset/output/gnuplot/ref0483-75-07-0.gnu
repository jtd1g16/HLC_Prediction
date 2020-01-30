# load "ref0483-75-07-0.gnu"
# chem = "ethanal"

set terminal postscript eps color
set title "ref = 483; chem = ethanal; casrn = 75-07-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1238916     * exp(  -5823.101    *(1/   298.    -1/T))

set label "" at    273.0000    ,   0.7489488     point
set label "" at    298.0000    ,   0.1251155     point
set label "" at    298.1500    ,   0.1238916     point ps 2 pt 6

plot [270:300] H(T)
