# load "ref3112-71-23-8.gnu"
# chem = "1-propanol"

set terminal postscript eps color
set title "ref = 3112; chem = 1-propanol; casrn = 71-23-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.244052     * exp(  -6202.981    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.4392215     point
set label "" at    323.0000    ,   0.2515541     point
set label "" at    333.0000    ,   0.1448741     point
set label "" at    343.0000    ,   0.8840561E-01 point
set label "" at    353.0000    ,   0.5172141E-01 point
set label "" at    363.0000    ,   0.2726202E-01 point
set label "" at    298.1500    ,    1.244052     point ps 2 pt 6

plot [310:370] H(T)
