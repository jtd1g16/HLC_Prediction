# load "ref2477-206-44-0.gnu"
# chem = "benzo[jk]fluorene"

set terminal postscript eps color
set title "ref = 2477; chem = benzo[jk]fluorene; casrn = 206-44-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5094673     * exp(  -4946.848    *(1/   298.    -1/T))

set label "" at    277.2500    ,    1.785714     point
set label "" at    284.1500    ,    1.149425     point
set label "" at    291.1500    ,   0.7575758     point
set label "" at    298.1500    ,   0.5102041     point
set label "" at    304.1500    ,   0.3676471     point
set label "" at    298.1500    ,   0.5094673     point ps 2 pt 6

plot [270:310] H(T)
