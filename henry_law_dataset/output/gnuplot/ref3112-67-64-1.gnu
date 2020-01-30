# load "ref3112-67-64-1.gnu"
# chem = "propanone"

set terminal postscript eps color
set title "ref = 3112; chem = propanone; casrn = 67-64-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2393376     * exp(  -4227.115    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.1218985     point
set label "" at    323.0000    ,   0.8067334E-01 point
set label "" at    333.0000    ,   0.5447038E-01 point
set label "" at    343.0000    ,   0.3716717E-01 point
set label "" at    353.0000    ,   0.2653016E-01 point
set label "" at    363.0000    ,   0.1901784E-01 point
set label "" at    298.1500    ,   0.2393376     point ps 2 pt 6

plot [310:370] H(T)
