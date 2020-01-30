# load "ref2477-83-32-9.gnu"
# chem = "acenaphthene"

set terminal postscript eps color
set title "ref = 2477; chem = acenaphthene; casrn = 83-32-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5399177E-01 * exp(  -6559.185    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.2840909     point
set label "" at    284.1500    ,   0.1589825     point
set label "" at    291.1500    ,   0.9174312E-01 point
set label "" at    298.1500    ,   0.5405405E-01 point
set label "" at    304.1500    ,   0.3496503E-01 point
set label "" at    298.1500    ,   0.5399177E-01 point ps 2 pt 6

plot [270:310] H(T)
