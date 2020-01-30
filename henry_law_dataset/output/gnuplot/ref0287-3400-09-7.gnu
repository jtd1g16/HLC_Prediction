# load "ref0287-3400-09-7.gnu"
# chem = "dichloroamine"

set terminal postscript eps color
set title "ref = 287; chem = dichloroamine; casrn = 3400-09-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2832765     * exp(  -4157.199    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3593304     point
set label "" at    313.1500    ,   0.1452612     point
set label "" at    298.1500    ,   0.2832765     point ps 2 pt 6

plot [290:320] H(T)
