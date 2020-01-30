# load "ref2451-106-48-9.gnu"
# chem = "4-hydroxychlorobenzene"

set terminal postscript eps color
set title "ref = 2451; chem = 4-hydroxychlorobenzene; casrn = 106-48-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1442.753     * exp(  -11097.34    *(1/   298.    -1/T))

set label "" at    323.1200    ,    110.6838     point
set label "" at    333.5200    ,    20.49700     point
set label "" at    343.2100    ,    9.072444     point
set label "" at    353.2300    ,    4.290071     point
set label "" at    363.1500    ,    2.240563     point
set label "" at    298.1500    ,    1442.753     point ps 2 pt 6

plot [320:370] H(T)
