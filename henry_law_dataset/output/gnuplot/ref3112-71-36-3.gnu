# load "ref3112-71-36-3.gnu"
# chem = "1-butanol"

set terminal postscript eps color
set title "ref = 3112; chem = 1-butanol; casrn = 71-36-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8221628     * exp(  -6183.142    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.3007712     point
set label "" at    323.0000    ,   0.1785223     point
set label "" at    333.0000    ,   0.8868896E-01 point
set label "" at    343.0000    ,   0.5534191E-01 point
set label "" at    353.0000    ,   0.3198954E-01 point
set label "" at    363.0000    ,   0.2064997E-01 point
set label "" at    298.1500    ,   0.8221628     point ps 2 pt 6

plot [310:370] H(T)
