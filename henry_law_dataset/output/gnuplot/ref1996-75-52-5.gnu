# load "ref1996-75-52-5.gnu"
# chem = "nitromethane"

set terminal postscript eps color
set title "ref = 1996; chem = nitromethane; casrn = 75-52-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3458860     * exp(  -3981.049    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.4392215     point
set label "" at    303.1500    ,   0.2726202     point
set label "" at    313.1500    ,   0.1826466     point
set label "" at    323.1500    ,   0.1238074     point
set label "" at    298.1500    ,   0.3458860     point ps 2 pt 6

plot [290:330] H(T)
