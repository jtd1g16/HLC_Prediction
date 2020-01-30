# load "ref3112-78-93-3.gnu"
# chem = "butanone"

set terminal postscript eps color
set title "ref = 3112; chem = butanone; casrn = 78-93-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1493648     * exp(  -4408.488    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.7675716E-01 point
set label "" at    323.0000    ,   0.4742237E-01 point
set label "" at    333.0000    ,   0.3117854E-01 point
set label "" at    343.0000    ,   0.2072731E-01 point
set label "" at    353.0000    ,   0.1482505E-01 point
set label "" at    363.0000    ,   0.1115764E-01 point
set label "" at    298.1500    ,   0.1493648     point ps 2 pt 6

plot [310:370] H(T)
