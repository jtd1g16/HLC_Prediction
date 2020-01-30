# load "ref2999-71-41-0.gnu"
# chem = "1-pentanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-pentanol; casrn = 71-41-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8065393     * exp(  -7091.820    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.8333333     point
set label "" at    305.5500    ,   0.4545455     point
set label "" at    323.1500    ,   0.1176471     point
set label "" at    343.1500    ,   0.3745318E-01 point
set label "" at    298.1500    ,   0.8065393     point ps 2 pt 6

plot [290:350] H(T)
