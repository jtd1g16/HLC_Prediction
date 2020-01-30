# load "ref3112-591-78-6.gnu"
# chem = "2-hexanone"

set terminal postscript eps color
set title "ref = 3112; chem = 2-hexanone; casrn = 591-78-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7887986E-01 * exp(  -4821.284    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.3696854E-01 point
set label "" at    323.0000    ,   0.2278382E-01 point
set label "" at    333.0000    ,   0.1447983E-01 point
set label "" at    343.0000    ,   0.9641448E-02 point
set label "" at    353.0000    ,   0.5906287E-02 point
set label "" at    363.0000    ,   0.4650581E-02 point
set label "" at    298.1500    ,   0.7887986E-01 point ps 2 pt 6

plot [310:370] H(T)
