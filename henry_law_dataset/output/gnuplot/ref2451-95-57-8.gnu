# load "ref2451-95-57-8.gnu"
# chem = "2-hydroxychlorobenzene"

set terminal postscript eps color
set title "ref = 2451; chem = 2-hydroxychlorobenzene; casrn = 95-57-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    3.594766     * exp(  -5674.878    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.8347196     point
set label "" at    333.3900    ,   0.4820724     point
set label "" at    343.0900    ,   0.2895966     point
set label "" at    353.0800    ,   0.1842274     point
set label "" at    363.0900    ,   0.1219253     point
set label "" at    298.1500    ,    3.594766     point ps 2 pt 6

plot [320:370] H(T)
