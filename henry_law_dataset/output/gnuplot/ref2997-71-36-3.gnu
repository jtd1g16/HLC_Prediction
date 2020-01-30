# load "ref2997-71-36-3.gnu"
# chem = "1-butanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-butanol; casrn = 71-36-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.082574     * exp(  -6039.013    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.2325581     point
set label "" at    333.1500    ,   0.1351351     point
set label "" at    343.1500    ,   0.6666667E-01 point
set label "" at    353.1500    ,   0.4587156E-01 point
set label "" at    363.1500    ,   0.3067485E-01 point
set label "" at    298.1500    ,    1.082574     point ps 2 pt 6

plot [320:370] H(T)
