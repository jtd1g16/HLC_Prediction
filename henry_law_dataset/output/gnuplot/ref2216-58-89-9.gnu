# load "ref2216-58-89-9.gnu"
# chem = "gamma-HCH"

set terminal postscript eps color
set title "ref = 2216; chem = gamma-HCH; casrn = 58-89-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    4.277133     * exp(  -7540.320    *(1/   298.    -1/T))

set label "" at    278.1500    ,    25.00000     point
set label "" at    283.1500    ,    16.39344     point
set label "" at    293.1500    ,    7.142857     point
set label "" at    303.1500    ,    3.030303     point
set label "" at    308.1500    ,    1.694915     point
set label "" at    298.1500    ,    4.277133     point ps 2 pt 6

plot [270:310] H(T)
