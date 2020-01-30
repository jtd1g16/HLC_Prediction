# load "ref3132-67-56-1.gnu"
# chem = "methanol"

set terminal postscript eps color
set title "ref = 3132; chem = methanol; casrn = 67-56-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.998217     * exp(  -5572.566    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.8068754     point
set label "" at    323.1500    ,   0.4839888     point
set label "" at    333.1500    ,   0.2734967     point
set label "" at    338.1500    ,   0.2209177     point
set label "" at    298.1500    ,    1.998217     point ps 2 pt 6

plot [310:340] H(T)
