# load "ref2451-108-43-0.gnu"
# chem = "3-hydroxychlorobenzene"

set terminal postscript eps color
set title "ref = 2451; chem = 3-hydroxychlorobenzene; casrn = 108-43-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    34.33888     * exp(  -6444.464    *(1/   298.    -1/T))

set label "" at    323.1800    ,    6.510813     point
set label "" at    333.5200    ,    3.458869     point
set label "" at    343.1700    ,    1.969463     point
set label "" at    353.2200    ,    1.190149     point
set label "" at    363.2200    ,   0.7196607     point
set label "" at    298.1500    ,    34.33888     point ps 2 pt 6

plot [320:370] H(T)
