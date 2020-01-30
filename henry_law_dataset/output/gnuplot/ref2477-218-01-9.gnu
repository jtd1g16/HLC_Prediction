# load "ref2477-218-01-9.gnu"
# chem = "chrysene"

set terminal postscript eps color
set title "ref = 2477; chem = chrysene; casrn = 218-01-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.889011     * exp(  -12724.88    *(1/   298.    -1/T))

set label "" at    277.2500    ,    50.00000     point
set label "" at    284.1500    ,    14.28571     point
set label "" at    291.1500    ,    5.263158     point
set label "" at    298.1500    ,    1.886792     point
set label "" at    304.1500    ,   0.8333333     point
set label "" at    298.1500    ,    1.889011     point ps 2 pt 6

plot [270:310] H(T)
