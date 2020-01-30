# load "ref2999-71-36-3.gnu"
# chem = "1-butanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-butanol; casrn = 71-36-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.025450     * exp(  -6767.974    *(1/   298.    -1/T))

set label "" at    298.1500    ,    1.000000     point
set label "" at    305.5500    ,   0.6250000     point
set label "" at    323.1500    ,   0.1689189     point
set label "" at    343.1500    ,   0.5319149E-01 point
set label "" at    298.1500    ,    1.025450     point ps 2 pt 6

plot [290:350] H(T)
