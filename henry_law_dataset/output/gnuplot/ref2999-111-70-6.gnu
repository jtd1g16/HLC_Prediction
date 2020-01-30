# load "ref2999-111-70-6.gnu"
# chem = "1-heptanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-heptanol; casrn = 111-70-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3799619     * exp(  -7249.694    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.4000000     point
set label "" at    305.5500    ,   0.2083333     point
set label "" at    323.1500    ,   0.5232862E-01 point
set label "" at    343.1500    ,   0.1666667E-01 point
set label "" at    298.1500    ,   0.3799619     point ps 2 pt 6

plot [290:350] H(T)
