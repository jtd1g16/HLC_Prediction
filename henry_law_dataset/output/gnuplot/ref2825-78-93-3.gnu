# load "ref2825-78-93-3.gnu"
# chem = "butanone"

set terminal postscript eps color
set title "ref = 2825; chem = butanone; casrn = 78-93-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1509737     * exp(  -4530.137    *(1/   298.    -1/T))

set label "" at    323.0000    ,   0.4713421E-01 point
set label "" at    333.0000    ,   0.3086994E-01 point
set label "" at    343.0000    ,   0.2026869E-01 point
set label "" at    353.0000    ,   0.1443707E-01 point
set label "" at    298.1500    ,   0.1509737     point ps 2 pt 6

plot [320:360] H(T)
