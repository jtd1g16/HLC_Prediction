# load "ref2464-72402-00-7.gnu"
# chem = "plinol"

set terminal postscript eps color
set title "ref = 2464; chem = plinol; casrn = 72402-00-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3994150     * exp(  -17110.61    *(1/   298.    -1/T))

set label "" at    279.1500    ,    19.85338     point
set label "" at    296.6500    ,   0.5338889     point
set label "" at    298.1500    ,   0.3994150     point ps 2 pt 6

plot [270:300] H(T)
