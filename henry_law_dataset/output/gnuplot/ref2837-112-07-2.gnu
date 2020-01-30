# load "ref2837-112-07-2.gnu"
# chem = "2-butoxyethyl ethanoate"

set terminal postscript eps color
set title "ref = 2837; chem = 2-butoxyethyl ethanoate; casrn = 112-07-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.807552     * exp(  -25494.17    *(1/   298.    -1/T))

set label "" at    293.1500    ,    7.771049     point
set label "" at    298.1500    ,    1.807552     point
set label "" at    298.1500    ,    1.807552     point ps 2 pt 6

plot [290:300] H(T)
