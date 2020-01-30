# load "ref2999-112-30-1.gnu"
# chem = "1-decanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-decanol; casrn = 112-30-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7616973E-01 * exp(  -6630.249    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.8771930E-01 point
set label "" at    305.5500    ,   0.4166667E-01 point
set label "" at    323.1500    ,   0.1098901E-01 point
set label "" at    343.1500    ,   0.4739336E-02 point
set label "" at    298.1500    ,   0.7616973E-01 point ps 2 pt 6

plot [290:350] H(T)
