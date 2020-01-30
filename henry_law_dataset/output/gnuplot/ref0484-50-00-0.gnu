# load "ref0484-50-00-0.gnu"
# chem = "methanal"

set terminal postscript eps color
set title "ref = 484; chem = methanal; casrn = 50-00-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    30.76994     * exp(  -7187.770    *(1/   298.    -1/T))

set label "" at    288.1500    ,    72.14409     point
set label "" at    298.1500    ,    29.31162     point
set label "" at    308.1500    ,    14.80385     point
set label "" at    318.1500    ,    6.641994     point
set label "" at    298.1500    ,    30.76994     point ps 2 pt 6

plot [280:320] H(T)
