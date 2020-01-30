# load "ref0484-75-87-6.gnu"
# chem = "trichloroethanal"

set terminal postscript eps color
set title "ref = 484; chem = trichloroethanal; casrn = 75-87-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    3355.955     * exp(  -3501.970    *(1/   298.    -1/T))

set label "" at    288.1500    ,    4924.747     point
set label "" at    298.1500    ,    3395.016     point
set label "" at    308.1500    ,    2417.962     point
set label "" at    318.1500    ,    1539.600     point
set label "" at    298.1500    ,    3355.955     point ps 2 pt 6

plot [280:320] H(T)
