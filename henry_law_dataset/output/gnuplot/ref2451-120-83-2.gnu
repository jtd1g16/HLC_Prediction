# load "ref2451-120-83-2.gnu"
# chem = "2,4-dichlorophenol"

set terminal postscript eps color
set title "ref = 2451; chem = 2,4-dichlorophenol; casrn = 120-83-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    6.579178     * exp(  -6791.072    *(1/   298.    -1/T))

set label "" at    323.1000    ,    1.148172     point
set label "" at    333.4800    ,   0.5887437     point
set label "" at    343.1600    ,   0.3249672     point
set label "" at    353.1800    ,   0.1865877     point
set label "" at    363.2100    ,   0.1136618     point
set label "" at    298.1500    ,    6.579178     point ps 2 pt 6

plot [320:370] H(T)
