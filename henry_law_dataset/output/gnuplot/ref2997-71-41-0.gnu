# load "ref2997-71-41-0.gnu"
# chem = "1-pentanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-pentanol; casrn = 71-41-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7494345     * exp(  -6121.683    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.1538462     point
set label "" at    333.1500    ,   0.8771930E-01 point
set label "" at    343.1500    ,   0.4901961E-01 point
set label "" at    353.1500    ,   0.3095975E-01 point
set label "" at    363.1500    ,   0.1912046E-01 point
set label "" at    298.1500    ,   0.7494345     point ps 2 pt 6

plot [320:370] H(T)
