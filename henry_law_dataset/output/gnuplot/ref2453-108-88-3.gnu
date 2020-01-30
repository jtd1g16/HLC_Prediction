# load "ref2453-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1517552E-02 * exp(  -4224.093    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2515541E-02 point
set label "" at    298.1500    ,   0.1499781E-02 point
set label "" at    308.1500    ,   0.9395910E-03 point
set label "" at    318.1500    ,   0.6339279E-03 point
set label "" at    298.1500    ,   0.1517552E-02 point ps 2 pt 6

plot [280:320] H(T)
